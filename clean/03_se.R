"
Deliberative Distortions -- clean pipeline
Standard errors for the hom/pol and domination tables

Per poll: lmer(y ~ (1 | issue)) intercept SE over group-issue pairs, as in
scripts/05a_hp_se.R and 05b_dom_se.R. Aggregate rows via fixed-effect
meta-analysis (fe_meta in 00_functions.R, equivalent to rmeta::meta.summaries
method = 'fixed').

Differences from the originals (issue 9 in AUDIT.md): every row range is
the actual number of polls in the dimension, and all dimensions share one
row layout.
"

source("clean/00_functions.R")
suppressPackageStartupMessages(library(lme4))

pair_col_of <- c(freqdis = "freqdis_grp", extdis = "extdis_grp",
                 freqgrp = "freqgrp_grp", extgrp = "extgrp_grp",
                 polarfreq = "polarfreq", homofreq = "homofreq",
                 polarex = "polarex", homoex = "homoex")

lmer_se <- function(y, issue) {
    if (n_distinct(issue[!is.na(y)]) < 2) {
        return(sd(y, na.rm = TRUE) / sqrt(sum(!is.na(y))))
    }
    fit <- suppressWarnings(suppressMessages(lmer(y ~ (1 | issue))))
    broom.mixed::tidy(fit, effects = "fixed")$std.error[1]
}

se_table <- function(pairs_file, table_file, out_file) {
    pairs <- read.csv(pairs_file) |>
        mutate(issue = gsub("^[0-9]*", "", unique_id))
    tab <- read.csv(table_file)

    polls <- tab |>
        filter(!pollname %in% c("Mean", "Weighted Mean (By Indices and Groups)")) |>
        mutate(across(-pollname, as.numeric))
    val_cols <- intersect(names(pair_col_of), names(polls))

    ses <- polls$pollnum |>
        map(\(p) {
            d <- pairs |> filter(poll_id == p)
            map_dbl(set_names(val_cols, paste0(val_cols, "_se")),
                    \(v) lmer_se(as.numeric(d[[pair_col_of[v]]]), d$issue))
        }) |>
        map(as_tibble_row) |>
        list_rbind()
    out <- bind_cols(polls, ses)

    w <- polls$ngroups * polls$nindices
    aggregate_rows <- val_cols |>
        map(\(v) {
            x <- polls[[v]]
            se <- ses[[paste0(v, "_se")]]
            null_val <- if (grepl("freq", v)) .5 else 0
            m_pr <- fe_meta(x, se)
            m_wt <- fe_meta(x, se, w = w)
            # Mean row: arithmetic mean (as in the main tables) with the
            # unweighted fixed-effect SE, matching the original 05a/05b pairing
            tibble("{v}" := c(mean(x), m_wt$est, NA, m_pr$est),
                   "{v}_se" := c(m_pr$se, m_wt$se,
                                 fe_meta(x - null_val, se, w = w)$p, m_pr$se))
        }) |>
        list_cbind() |>
        mutate(pollname = c("Mean", "Weighted Mean (By Indices and Groups)",
                            "p-value of Weighted Mean", "Precision Wtd Mean"))

    bind_rows(out, aggregate_rows) |>
        write.csv(out_file, row.names = FALSE)
}

se_table("tabs_clean/03_hom_pol_by_group_issue.csv",
         "tabs_clean/02_table_2_hom_pol.csv",
         "tabs_clean/02_table_2_hom_pol_se.csv")

dom_tables <- c(educ = "04_table_4b_toward_highed",
                gender = "04_table_4a_toward_male",
                income = "04_table_4c_toward_highinc",
                triple = "04_table_4d_toward_triple")
iwalk(dom_tables, \(table, d) {
    se_table(sprintf("tabs_clean/03_dom_%s_by_group_issue.csv", d),
             sprintf("tabs_clean/%s.csv", table),
             sprintf("tabs_clean/%s_se.csv", table))
})
