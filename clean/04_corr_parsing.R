"
Deliberative Distortions -- clean pipeline
Correlations among H, P, D and the parsing-domination regressions

Replaces scripts/04_corr_hpd.R and 07_parsing_domination.R. Differences
from the originals (issues 2 and 8 in AUDIT.md): one Eq. 3 pair file per
dimension, a corrected triple-disadvantaged share, and regression results
written to a file rather than printed.
"

source("clean/00_functions.R")
library(broom)

dimensions <- c("educ", "gender", "income", "triple")

dom_pairs <- dimensions |>
    set_names() |>
    map(\(d) read.csv(sprintf("tabs_clean/03_dom_%s_by_group_issue.csv", d)))

# Correlations
pd <- dom_pairs |>
    imap(\(dom, d) {
        dom |>
            select(unique_id, freqgrp_grp, extgrp_grp, freqdis_grp, extdis_grp) |>
            rename_with(\(x) paste0(d, "_", x), -unique_id)
    }) |>
    reduce(\(x, y) full_join(x, y, by = "unique_id"),
           .init = read.csv("tabs_clean/03_hom_pol_by_group_issue.csv"))

pd |>
    select(-unique_id, -poll_id) |>
    cor(use = "na.or.complete") |>
    write.csv("tabs_clean/05_corr_hpd.csv", row.names = FALSE)

# Parsing domination: regress movement on the group's share of disadvantaged
# members. extdis_grp = disadvantaged subgroup's movement toward the
# advantaged; extgrp_grp = Eq. 3 D.
p_dis <- read.csv("data/polardata.csv") |>
    summarise(educ = mean(bettered == 0, na.rm = TRUE),
              gender = mean(female == 1, na.rm = TRUE),
              income = mean(highinc == 0, na.rm = TRUE),
              triple = mean(highinc == 0 & bettered == 0 & female == 1,
                            na.rm = TRUE),
              .by = pollgroup)

fits <- expand_grid(dimension = dimensions,
                    outcome = c("extdis_grp", "extgrp_grp")) |>
    pmap(function(dimension, outcome) {
        dom <- dom_pairs[[dimension]] |>
            left_join(p_dis |> select(pollgroup, p_dis = all_of(dimension)),
                      by = c(group_id = "pollgroup"))
        cf <- lm(reformulate("p_dis", outcome), data = dom) |> tidy()
        tibble(dimension, outcome,
               intercept = cf$estimate[cf$term == "(Intercept)"],
               slope = cf$estimate[cf$term == "p_dis"],
               slope_se = cf$std.error[cf$term == "p_dis"],
               slope_p = cf$p.value[cf$term == "p_dis"],
               n = sum(complete.cases(dom[[outcome]], dom$p_dis)))
    }) |>
    list_rbind()

write.csv(fits, "tabs_clean/07_parsing_domination.csv", row.names = FALSE)
