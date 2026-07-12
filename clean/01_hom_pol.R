"
Deliberative Distortions -- clean pipeline
Table 2: Homogenization and Polarization

Replaces scripts/02_hom_pol_table_2_3.R. Differences from the original
(issues 5-7, 13 in AUDIT.md): normed values built per index on their own
accumulator, the Mean row excluded from the Weighted Mean, and normed
denominators guarded.
"

source("clean/00_functions.R")

dat <- load_dp_data()
dpdat <- dat$dpdat
att_indices <- dat$att_indices

hom_pol_index <- function(smdata, t1var, t2var) {
    tibble(pollgroup = as.character(smdata$pollgroup),
           t1 = smdata[[t1var]],
           t2 = smdata[[t2var]]) |>
        summarise(t1mean = mean(t1, na.rm = TRUE),
                  t2mean = mean(t2, na.rm = TRUE),
                  t1sd = sd(t1, na.rm = TRUE),
                  t2sd = sd(t2, na.rm = TRUE),
                  .by = pollgroup) |>
        arrange(pollgroup) |>
        mutate(
            # polarization: movement toward the nearer extreme; undefined at .5
            polarex = if_else(t1mean == .5, NA,
                              (t2mean - t1mean) * sign(t1mean - .5)),
            homoex = t1sd - t2sd,
            polarfreq = polarex > 0,
            homofreq = homoex > 0,
            polarex_normed = normed_move(polarex, t1mean, t2mean),
            # the .5 denominator assumes sd <= .5, but a small group's sample
            # sd can reach or exceed .5, so guard against a zero or negative
            # headroom: undefined, not infinite
            homoex_normed = case_when(is.na(homoex) ~ NA,
                                      t2sd > t1sd ~ homoex / (.5 - t1sd),
                                      t2sd < t1sd ~ homoex / t1sd,
                                      t2sd == t1sd ~ 0),
            homoex_normed = if_else(is.finite(homoex_normed) &
                                        !(t2sd > t1sd & t1sd >= .5),
                                    homoex_normed, NA),
            unique_id = paste0(pollgroup, t1var))
}

getall <- att_indices |>
    pmap(function(poll_id, t1var, t2_t3var, dpnum, ...) {
        hom_pol_index(dpdat[dpdat$pollid == poll_id, ], t1var, t2_t3var) |>
            mutate(poll_id = dpnum)
    }) |>
    list_rbind()

poll_info <- dpdat |>
    summarise(pollname = first(pollname),
              ngroups = n_distinct(pollgroup),
              nindices = mean(numindices),
              .by = dpnum) |>
    rename(pollnum = dpnum)

res <- getall |>
    summarise(across(c(homofreq, homoex, polarfreq, polarex),
                     \(x) mean(x, na.rm = TRUE)),
              .by = poll_id) |>
    left_join(poll_info, by = c(poll_id = "pollnum")) |>
    mutate(pollnum = poll_id) |>
    select(pollname, pollnum, ngroups, nindices,
           homofreq, homoex, polarfreq, polarex) |>
    arrange(pollnum) |>
    add_summary_rows()

res_normed <- getall |>
    summarise(homofreq = mean(homofreq, na.rm = TRUE),
              homoex = mean(homoex_normed, na.rm = TRUE),
              polarfreq = mean(polarfreq, na.rm = TRUE),
              polarex = mean(polarex_normed, na.rm = TRUE),
              .by = poll_id) |>
    left_join(poll_info, by = c(poll_id = "pollnum")) |>
    mutate(pollnum = poll_id) |>
    select(pollname, pollnum, ngroups, nindices,
           homofreq, homoex, polarfreq, polarex) |>
    arrange(pollnum) |>
    add_summary_rows()

dir.create("tabs_clean", showWarnings = FALSE)
write.csv(res, "tabs_clean/02_table_2_hom_pol.csv", row.names = FALSE)
write.csv(res_normed, "tabs_clean/02_table_2_hom_pol_normed.csv", row.names = FALSE)
write.csv(getall[, c("unique_id", "poll_id", "polarfreq", "homofreq",
                     "polarex", "homoex")],
          "tabs_clean/03_hom_pol_by_group_issue.csv", row.names = FALSE)
write.csv(cor(getall[, c("polarfreq", "homofreq", "polarex", "homoex")],
              use = "na.or.complete"),
          "tabs_clean/05_corr_hom_pol.csv", row.names = FALSE)
