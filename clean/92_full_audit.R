"
Deliberative Distortions -- audit
Complete change inventory: for every output the original scripts produced,
what does the clean pipeline change? Compares like-labeled quantities
(committed tabs/<file> vs tabs_clean/<file>), so each row answers: 'this is
what the paper's file of that name said, and this is what it says corrected.'
Run after clean/05_run_all.R. Writes tabs_clean/92_full_audit.csv.
"

source("clean/00_functions.R")
library(knitr)

row_by_name <- function(f, name) {
    r <- read.csv(f)
    # which() so the committed files' stray NA-pollname rows can't match
    r[which(r$pollname == name), , drop = FALSE]
}

num <- function(df, col) if (nrow(df) && col %in% names(df)) as.numeric(df[[col]]) else NA_real_

# ---- 1. Main tables: weighted-mean rows, all value columns -----------------
main_tables <- tribble(
    ~file, ~what,
    "02_table_2_hom_pol.csv", "Table 2 H/P",
    "04_table_4a_toward_male.csv", "D toward men",
    "04_table_4a_toward_female.csv", "D toward women (mirror)",
    "04_table_4b_toward_highed.csv", "D toward better educated",
    "04_table_4b_toward_lowed.csv", "D toward less educated (mirror)",
    "04_table_4c_toward_highinc.csv", "D toward higher income",
    "04_table_4c_toward_lowinc.csv", "D toward lower income (mirror)",
    "04_table_4d_toward_triple.csv", "D toward triple advantaged",
    "04_table_4d_toward_triple_disadv.csv", "D toward triple disadvantaged (mirror)")

wm <- "Weighted Mean (By Indices and Groups)"
main_audit <- main_tables |>
    pmap(function(file, what) {
        com <- row_by_name(file.path("tabs", file), wm)
        cln <- row_by_name(file.path("tabs_clean", file), wm)
        cols <- intersect(names(cln),
                          c("freqdis", "extdis", "freqgrp", "extgrp",
                            "homofreq", "homoex", "polarfreq", "polarex"))
        map(cols, \(v) tibble(file, what, quantity = v,
                              committed = num(com, v), clean = num(cln, v))) |>
            list_rbind()
    }) |>
    list_rbind() |>
    mutate(change = clean - committed,
           flips_sign = sign(clean) != sign(committed) &
                        pmax(abs(clean), abs(committed)) > 1e-4,
           crosses_.5 = grepl("freq", quantity) &
                        sign(clean - .5) != sign(committed - .5))

cat("\n## Weighted-mean rows, every main table (committed vs clean)\n")
print(kable(main_audit |> select(-file), digits = 4))

# ---- 2. SE tables: weighted SE, p-value, precision rows --------------------
se_tables <- tribble(
    ~file, ~what,
    "02_table_2_hom_pol_se.csv", "H/P",
    "04_table_4a_toward_male_se.csv", "toward men",
    "04_table_4b_toward_highed_se.csv", "toward better educated",
    "04_table_4c_toward_highinc_se.csv", "toward higher income",
    "04_table_4d_toward_triple_se.csv", "toward triple advantaged")

se_audit <- se_tables |>
    pmap(function(file, what) {
        com <- read.csv(file.path("tabs", file))
        cln <- read.csv(file.path("tabs_clean", file))
        se_cols <- grep("_se$", intersect(names(com), names(cln)), value = TRUE)
        # in the committed files the p-values live in the *_se columns of the
        # one unlabeled row (the 21- and 11-poll blocks order their Mean and
        # Weighted Mean rows differently, so relative position is unreliable);
        # in the clean files they are in the labeled row
        com_p_row <- which(is.na(com$pollname) | com$pollname == "")[1]
        map(se_cols, \(v) {
            tibble(file, what, quantity = sub("_se$", "", v),
                   committed_wtd_se = num(row_by_name(file.path("tabs", file), wm), v),
                   clean_wtd_se = num(row_by_name(file.path("tabs_clean", file), wm), v),
                   committed_p = suppressWarnings(as.numeric(com[[v]][com_p_row])),
                   clean_p = num(row_by_name(file.path("tabs_clean", file),
                                             "p-value of Weighted Mean"), v))
        }) |>
            list_rbind()
    }) |>
    list_rbind() |>
    mutate(sig_change_at_05 = (committed_p < .05) != (clean_p < .05))

cat("\n## Weighted-mean SEs and p-values (committed vs clean)\n")
print(kable(se_audit |> select(-file), digits = 4))

# ---- 3. Normed tables ------------------------------------------------------
normed_tables <- tribble(
    ~com_file, ~cln_file, ~what,
    "02_table_2_hom_pol_normed.csv", "02_table_2_hom_pol_normed.csv", "H/P normed",
    "04_table_4a_toward_male_normed.csv", "04_table_4a_toward_male_normed.csv", "gender normed",
    "04_table_4b_toward_highed_normed.csv", "04_table_4b_toward_highed_normed.csv", "education normed",
    "04_table_4c_toward_highinc_normed.csv", "04_table_4c_toward_highinc_normed.csv", "income normed",
    "04_table_4d_toward_triple_normed.csv", "04_table_4d_toward_triple_normed.csv", "triple normed")

# The original scripts wrote no aggregate rows to the normed tables, so
# compute the committed weighted mean from their poll rows.
committed_normed_wm <- function(f, v) {
    r <- read.csv(f) |>
        filter(!pollname %in% c("Mean", wm), !is.na(pollnum)) |>
        mutate(across(-pollname, as.numeric))
    weighted.mean(r[[v]], r$ngroups * r$nindices, na.rm = TRUE)
}

normed_audit <- normed_tables |>
    pmap(function(com_file, cln_file, what) {
        cln <- row_by_name(file.path("tabs_clean", cln_file), wm)
        cols <- intersect(names(cln),
                          c("extgrp", "extdis", "extgrp_normed", "extdis_normed",
                            "homofreq", "homoex", "polarfreq", "polarex"))
        map(cols, \(v) tibble(what, quantity = v,
                              committed = committed_normed_wm(file.path("tabs", com_file), v),
                              clean = num(cln, v))) |>
            list_rbind()
    }) |>
    list_rbind() |>
    mutate(change = clean - committed)

cat("\n## Normed tables, weighted-mean rows (committed vs clean)\n")
print(kable(normed_audit, digits = 4))

# ---- 4. H/P x D correlations ----------------------------------------------
# The committed 05_corr_hpd.csv columns are unlabeled reorderings, so
# recompute the committed-file correlations directly from the committed
# group-issue files each dimension's published analysis used.
committed_pairs <- c(educ = "03_dom_ed_by_group_issue.csv",
                     gender = "03_dom_fem_by_group_issue.csv",
                     income = "03_dom_highinc_by_group_issue.csv",
                     triple = "03_dom_triple_by_group_issue.csv")
hp_pairs_com <- read.csv("tabs/03_hom_pol_by_group_issue.csv")
hp_pairs_cln <- read.csv("tabs_clean/03_hom_pol_by_group_issue.csv")

cor_audit <- names(committed_pairs) |>
    map(\(d) {
        com <- read.csv(file.path("tabs", committed_pairs[[d]])) |>
            left_join(hp_pairs_com, by = "unique_id")
        cln <- read.csv(sprintf("tabs_clean/03_dom_%s_by_group_issue.csv", d)) |>
            left_join(hp_pairs_cln, by = "unique_id")
        map(c("polarex", "homoex"), \(v) {
            tibble(dimension = d, correlation = paste0("cor(D, ", v, ")"),
                   committed = cor(com$extgrp_grp, com[[v]], use = "na.or.complete"),
                   clean = cor(cln$extgrp_grp, cln[[v]], use = "na.or.complete"))
        }) |>
            list_rbind()
    }) |>
    list_rbind()

cat("\n## Correlations of D with polarization/homogenization (pair level)\n")
print(kable(cor_audit, digits = 3))

# ---- 5. Parsing regressions (Eq. 4 discussion) -----------------------------
# Replicate the original 07_parsing_domination.R regressions from the
# committed inputs (including the mis-specified pt3), then set against the
# clean fits.
dpdat <- read.csv("data/polardata.csv")
p_com <- dpdat |>
    summarise(educ = mean(bettered == 0, na.rm = TRUE),
              gender = mean(female == 1, na.rm = TRUE),
              income = mean(highinc == 0, na.rm = TRUE),
              triple = mean(highinc == 0 & highinc == 0 & female == 1,
                            na.rm = TRUE),   # pt3 exactly as in 07:42
              .by = pollgroup)

parsing_com <- names(committed_pairs) |>
    map(\(d) {
        dom <- read.csv(file.path("tabs", committed_pairs[[d]])) |>
            left_join(p_com |> select(pollgroup, p_dis = all_of(d)),
                      by = c(group_id = "pollgroup"))
        map(c("extdis_grp", "extgrp_grp"), \(y) {
            cf <- summary(lm(dom[[y]] ~ dom$p_dis))$coefficients
            tibble(dimension = d, outcome = y,
                   committed_slope = cf[2, 1], committed_p = cf[2, 4])
        }) |>
            list_rbind()
    }) |>
    list_rbind()

parsing_audit <- parsing_com |>
    left_join(read.csv("tabs_clean/07_parsing_domination.csv") |>
                  select(dimension, outcome, clean_slope = slope,
                         clean_p = slope_p),
              by = c("dimension", "outcome"))

cat("\n## Parsing-domination regressions (original spec on committed files vs clean)\n")
print(kable(parsing_audit, digits = 4))

# ---- 6. Untouched outputs --------------------------------------------------
cat("\n## Outputs the clean pipeline does not change\n")
cat("- 01_table_1_dp_summary.csv, att_change.csv: no issues found; reproduce exactly.\n")
cat("- figs/: out of scope (06_figs.R no longer runs; would need regeneration\n")
cat("  from corrected files either way).\n")

write.csv(bind_rows(main_audit |> mutate(section = "main"),
                    se_audit |> mutate(section = "se"),
                    normed_audit |> mutate(section = "normed"),
                    cor_audit |> mutate(section = "correlation"),
                    parsing_audit |> mutate(section = "parsing")),
          "tabs_clean/92_full_audit.csv", row.names = FALSE)
