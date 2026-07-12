"
Deliberative Distortions -- clean pipeline
Tables 3/4: Domination, all four dimensions

Implements Eq. 3 as printed: D = (A_t2 - A_t1) * sign(A_adv_t1 - A_t1), with
the ADVANTAGED subgroup's initial mean as the sign reference, so D > 0 means
movement toward the advantaged subgroup's initial attitude (domination).

In each toward_<advantaged> table: extgrp/freqgrp = group movement toward the
advantaged (Eq. 3 D and its frequency); extdis/freqdis = the disadvantaged
subgroup's movement toward the advantaged. The toward_<disadvantaged> mirror
table measures movement toward the disadvantaged subgroup's initial mean
(extdis there is the advantaged subgroup's movement).

Replaces the duplicated sections of scripts/03a-03d with one loop over the
four dimensions. Differences from the originals (issues 1, 3-5, and 11 in
AUDIT.md, plus the portability note): the sign reference is the advantaged
subgroup for every dimension; a single signed metric per pair; ties handled
by sign() with a 1e-12 tolerance and kept in denominators as D = 0; a
subgroup with no valid responses on an index makes the pair NA; the Mean
row is excluded from the Weighted Mean; tables are sized by the polls
actually present.
"

source("clean/00_functions.R")

dat <- load_dp_data()
dpdat <- dat$dpdat |>
    mutate(triple = highinc & bettered & !female)
att_indices <- dat$att_indices

dims <- list(
    educ = list(keep = \(d) !is.na(d$bettered),
                adv = \(d) d$bettered == 1,
                table = "04_table_4b", adv_label = "highed", dis_label = "lowed"),
    gender = list(keep = \(d) !is.na(d$female),
                  adv = \(d) d$female == 0,
                  table = "04_table_4a", adv_label = "male", dis_label = "female"),
    income = list(keep = \(d) !is.na(d$hhincome) & !is.na(d$highinc),
                  adv = \(d) d$highinc == 1,
                  table = "04_table_4c", adv_label = "highinc", dis_label = "lowinc"),
    triple = list(keep = \(d) !is.na(d$hhincome) & !is.na(d$triple),
                  adv = \(d) d$triple,
                  table = "04_table_4d", adv_label = "triple", dis_label = "triple_disadv"))

poll_table <- function(pairs, poll_info, ext_dis, ext_grp) {
    pairs |>
        summarise(freqdis = mean(.data[[ext_dis]] > 0, na.rm = TRUE),
                  extdis = mean(.data[[ext_dis]], na.rm = TRUE),
                  freqgrp = mean(.data[[ext_grp]] > 0, na.rm = TRUE),
                  extgrp = mean(.data[[ext_grp]], na.rm = TRUE),
                  .by = poll_id) |>
        left_join(poll_info, by = "poll_id") |>
        mutate(pollnum = poll_id) |>
        select(pollname, pollnum, ngroups, nindices,
               freqdis, extdis, freqgrp, extgrp) |>
        arrange(pollnum) |>
        add_summary_rows()
}

dir.create("tabs_clean", showWarnings = FALSE)

for (dim_name in names(dims)) {
    cfg <- dims[[dim_name]]
    data <- dpdat |> filter(cfg$keep(dpdat))

    pairs <- att_indices |>
        filter(poll_id %in% unique(data$pollid)) |>
        pmap(function(poll_id, poll_name, t1var, t2_t3var, dpnum, ...) {
            smdata <- data |> filter(pollid == poll_id)
            pairs <- dom_pairs_index(smdata, t1var, t2_t3var, cfg$adv(smdata))
            if (is.null(pairs)) return(NULL)
            pairs |>
                mutate(unique_id = paste0(group_id, t1var),
                       poll_id = dpnum,
                       poll_name = poll_name)
        }) |>
        list_rbind()

    poll_info <- data |>
        summarise(pollname = first(pollname),
                  ngroups = n_distinct(pollgroup),
                  nindices = mean(numindices),
                  .by = dpnum) |>
        rename(poll_id = dpnum)

    res_adv <- poll_table(pairs, poll_info, "ext_dis", "ext_grp")
    res_dis <- poll_table(pairs, poll_info, "ext_adv_mir", "ext_grp_mir")
    res_normed <- pairs |>
        summarise(across(c(ext_grp, ext_dis, ext_grp_normed, ext_dis_normed),
                         \(x) mean(x, na.rm = TRUE)),
                  .by = poll_id) |>
        left_join(poll_info, by = "poll_id") |>
        mutate(pollnum = poll_id) |>
        select(pollname, pollnum, ngroups, nindices,
               extgrp = ext_grp, extdis = ext_dis,
               extgrp_normed = ext_grp_normed, extdis_normed = ext_dis_normed) |>
        arrange(pollnum) |>
        add_summary_rows()

    write.csv(res_adv,
              sprintf("tabs_clean/%s_toward_%s.csv", cfg$table, cfg$adv_label),
              row.names = FALSE)
    write.csv(res_dis,
              sprintf("tabs_clean/%s_toward_%s.csv", cfg$table, cfg$dis_label),
              row.names = FALSE)
    write.csv(res_normed,
              sprintf("tabs_clean/%s_toward_%s_normed.csv", cfg$table, cfg$adv_label),
              row.names = FALSE)

    pairs |>
        transmute(unique_id, poll_id, poll_name, group_id,
                  freqgrp_grp = ext_grp > 0,
                  extgrp_grp = ext_grp,
                  freqdis_grp = ext_dis > 0,
                  extdis_grp = ext_dis,
                  extgrp_mir = ext_grp_mir,
                  extadv_mir = ext_adv_mir) |>
        write.csv(sprintf("tabs_clean/03_dom_%s_by_group_issue.csv", dim_name),
                  row.names = FALSE)
}
