pooled_results <- scores |>
  group_by(format, construct, timing_class, membership, metric) |>
  group_modify(\(d, keys) map_dfr(c("pairs", "equal_family_event"), \(w) pool_scores(d, w))) |>
  ungroup()

family_results <- scores |>
  summarise(
    pairs = sum(!is.na(estimate)),
    mean = na_if(mean(estimate, na.rm = TRUE), NaN),
    positive_fraction = na_if(mean(positive, na.rm = TRUE), NaN),
    .by = c(family_id, format, construct, timing_class, membership, metric)
  )

leave_family_out <- scores |>
  group_by(format, construct, timing_class, membership, metric) |>
  group_modify(\(d, keys) {
    families <- unique(d$family_id)
    map_dfr(families, \(omit) {
      map_dfr(c("pairs", "equal_family_event"), \(w) {
        pool_scores(filter(d, family_id != omit), w) |> mutate(omitted_family = omit)
      })
    })
  }) |>
  ungroup()

readr::write_csv(pooled_results, "oos_replication/tabs/pooled_results.csv")
readr::write_csv(family_results, "oos_replication/tabs/family_results.csv")
readr::write_csv(leave_family_out, "oos_replication/tabs/leave_family_out.csv")

original <- readr::read_csv("tabs/03_hom_pol_by_group_issue.csv", show_col_types = FALSE)
original_comparison <- original |>
  summarise(
    h = mean(homoex, na.rm = TRUE), p = mean(polarex, na.rm = TRUE),
    h_paired = mean(homoex_cc, na.rm = TRUE), p_paired = mean(polarex_cc, na.rm = TRUE)
  )
readr::write_csv(original_comparison, "oos_replication/tabs/original_comparison.csv")

report_table <- family_results |>
  filter(metric %in% c("h", "p", "d_gender", "d_education", "d_income", "d_combined")) |>
  select(family_id, format, membership, metric, pairs, mean, positive_fraction)
writeLines(c(
  "# Results for the currently verified sources", "",
  "Search and source verification remain open; this is not the final corpus.", "",
  "Scores use the 0–1 instrument scale. Positive H means lower within-group SD;",
  "positive P means movement away from the initial midpoint direction; positive D",
  "means movement toward the initial advantaged-subgroup reference.", "",
  "No format/timing stratum yet has adequate independent families for pooled inference.",
  "The 19 OBOE sites share one synchronous national event. Hong Kong has one group",
  "per discussion format. Tanzania's February-to-May window also includes information",
  "sessions. These estimates describe the included participants and instruments.", "",
  as.character(knitr::kable(report_table, format = "pipe", digits = 4)), "",
  "## Corrected original benchmark", "",
  as.character(knitr::kable(original_comparison, format = "pipe", digits = 4)), "",
  "The original uses attitude indices, while these sources use individual items.",
  "Differences therefore combine setting, sample, timing and measurement; they are",
  "not estimates of a causal difference between deliberative formats."
), "oos_replication/tabs/results.md")
