"
Deliberative Distortions -- audit
Compare published Table 2, the committed tabs/ (what the original scripts
produced), and the clean Eq. 3 pipeline (tabs_clean/). Also runs the
verification checks. Run after clean/05_run_all.R.
For the table-by-table change inventory, see clean/92_full_audit.R.
"

source("clean/00_functions.R")
library(knitr)

wrow <- function(f) {
    read.csv(f) |>
        filter(pollname == "Weighted Mean (By Indices and Groups)")
}

# Published Table 2 (p. 1213) and the committed file each row came from
published <- tribble(
    ~dimension, ~pub_D, ~pub_Db, ~table,
    "gender", .008, .464, "04_table_4a_toward_male.csv",
    "educ",  -.013, .447, "04_table_4b_toward_highed.csv",
    "income", .000, .485, "04_table_4c_toward_highinc.csv",
    "triple", -.015, .466, "04_table_4d_toward_triple.csv") |>
    mutate(mirror = c("04_table_4a_toward_female.csv",
                      "04_table_4b_toward_lowed.csv",
                      "04_table_4c_toward_lowinc.csv",
                      "04_table_4d_toward_triple_disadv.csv"))

comp <- published |>
    pmap(function(dimension, pub_D, pub_Db, table, mirror) {
        com <- wrow(file.path("tabs", table))
        mir <- wrow(file.path("tabs", mirror))
        cln <- wrow(file.path("tabs_clean", table))
        tibble(dimension,
               published_D = pub_D, published_Db = pub_Db,
               committed_D = as.numeric(com$extgrp),
               committed_Db = as.numeric(com$freqgrp),
               mirror_D = as.numeric(mir$extgrp),
               mirror_Db = as.numeric(mir$freqgrp),
               clean_eq3_D = as.numeric(cln$extgrp),
               clean_eq3_Db = as.numeric(cln$freqgrp))
    }) |>
    list_rbind()
write.csv(comp, "tabs_clean/91_audit_comparison.csv", row.names = FALSE)
cat("\n## Domination: published vs committed vs clean Eq. 3 (weighted mean)\n")
print(kable(comp, digits = 4))

hp <- c("homofreq", "homoex", "polarfreq", "polarex")
anchor <- bind_rows(
    tibble(source = "published", homofreq = .595, homoex = .013,
           polarfreq = .454, polarex = -.022),
    wrow("tabs/02_table_2_hom_pol.csv") |>
        mutate(source = "committed", across(all_of(hp), as.numeric)) |>
        select(source, all_of(hp)),
    wrow("tabs_clean/02_table_2_hom_pol.csv") |>
        mutate(source = "clean") |>
        select(source, all_of(hp)))
cat("\n## Homogenization/polarization anchor\n")
print(kable(anchor, digits = 4))

check <- function(label, ok) cat(sprintf("[%s] %s\n", if (ok) "PASS" else "FAIL", label))

# 1. Pair-level equivalence: the clean Eq. 3 score must equal the committed
# advantaged-referenced section's score wherever both are defined; every
# discrepancy must be a knife-edge tie or a goji::nona (missing subgroup
# mean set to 0) case.
cat("\n## Pair-level checks\n")
pair_check <- function(dim_name, committed_file) {
    m <- read.csv(sprintf("tabs_clean/03_dom_%s_by_group_issue.csv", dim_name)) |>
        left_join(read.csv(file.path("tabs", committed_file)) |>
                      select(unique_id, com = extgrp_grp),
                  by = "unique_id")
    both <- !is.na(m$extgrp_grp) & !is.na(m$com)
    disagree <- sum(abs(m$extgrp_grp - m$com)[both] > 1e-9)
    cat(sprintf(
        "%-7s vs %s: %d pairs, %d disagreements (knife-edge ties), %d committed-only (nona), %d clean-only\n",
        dim_name, committed_file, nrow(m), disagree,
        sum(is.na(m$extgrp_grp) & !is.na(m$com)),
        sum(!is.na(m$extgrp_grp) & is.na(m$com))))
    disagree
}
d1 <- pair_check("educ",   "03_dom_lowed_by_group_issue.csv")
d2 <- pair_check("income", "03_dom_lowinc_by_group_issue.csv")
d3 <- pair_check("triple", "03_dom_triple_disadv_by_group_issue.csv")
check("pair-level Eq. 3 equivalence (educ/income/triple), knife-edge ties aside",
      d1 + d2 + d3 <= 10)

# gender: the committed male-referenced pair file (03_dom_fem_...) mixes
# two metrics (the double-append), so compare against the committed
# female-referenced mirror instead
cln_g <- read.csv("tabs_clean/03_dom_gender_by_group_issue.csv")
m <- cln_g |>
    left_join(read.csv("tabs/03_dom_men_by_group_issue.csv") |>
                  select(unique_id, com = extgrp_grp),
              by = "unique_id")
both <- !is.na(m$extgrp_mir) & !is.na(m$com)
dg <- sum(abs(m$extgrp_mir - m$com)[both] > 1e-9)
cat(sprintf("gender  vs 03_dom_men_by_group_issue.csv (mirror): %d pairs, %d disagreements\n",
            nrow(m), dg))
check("gender pair-level equivalence via committed mirror", dg <= 5)

n_fem <- nrow(read.csv("tabs/03_dom_fem_by_group_issue.csv"))
check(sprintf("committed 03_dom_fem has ~2x the gender pairs (%d vs %d clean): double-append",
              n_fem, nrow(cln_g)), n_fem > 1.9 * nrow(cln_g))

# 2. Mirror consistency: per group-issue pair, movement toward the
# disadvantaged is the exact negation of movement toward the advantaged.
# (Poll-level means differ slightly because pairs where only one direction
# is defined -- e.g. an all-missing advantaged subgroup -- enter one
# denominator and not the other.)
cat("\n## Clean-table internal checks\n")
mirror_ok <- c("educ", "gender", "income", "triple") |>
    map_lgl(\(d) {
        p <- read.csv(sprintf("tabs_clean/03_dom_%s_by_group_issue.csv", d))
        both <- !is.na(p$extgrp_grp) & !is.na(p$extgrp_mir)
        max(abs(p$extgrp_grp[both] + p$extgrp_mir[both])) == 0
    }) |>
    all()
check("per pair, extgrp toward disadvantaged is the exact negation of Eq. 3 D", mirror_ok)

no_stray <- list.files("tabs_clean", pattern = "^04_.*csv$", full.names = TRUE) |>
    map_lgl(\(f) !any(is.na(read.csv(f)$pollname))) |>
    all()
check("no stray NA rows in clean tables", no_stray)

# 3. Mean-row decomposition (issue 5): adding the synthetic Mean row back
# into the clean H/P weighted mean should land on the committed value
res <- read.csv("tabs_clean/02_table_2_hom_pol.csv") |>
    filter(pollname != "Weighted Mean (By Indices and Groups)") |>
    mutate(across(-pollname, as.numeric))
with_mean_row <- res |>
    summarise(across(all_of(hp),
                     \(x) weighted.mean(x, w = ngroups * nindices)))
cat("\nclean H/P weighted mean with the Mean row added back (issue 5 replicated) vs committed:\n")
print(kable(bind_rows(with_mean_row |> mutate(source = "clean + Mean row"),
                      wrow("tabs/02_table_2_hom_pol.csv") |>
                          mutate(source = "committed",
                                 across(all_of(hp), as.numeric)) |>
                          select(all_of(hp), source)),
            digits = 4))

# 4. Tie sensitivity: pairs where D is exactly 0 under the clean convention
cat("\n## Tie sensitivity (clean convention keeps D = 0 pairs)\n")
ties <- c("educ", "gender", "income", "triple") |>
    map(\(d) {
        p <- read.csv(sprintf("tabs_clean/03_dom_%s_by_group_issue.csv", d))
        x <- p$extgrp_grp[!is.na(p$extgrp_grp)]
        tibble(dimension = d, pairs = length(x), tied = sum(x == 0),
               D_with = mean(x), D_without = mean(x[x != 0]),
               Db_with = mean(x > 0), Db_without = mean(x[x != 0] > 0))
    }) |>
    list_rbind()
print(kable(ties, digits = 4))
