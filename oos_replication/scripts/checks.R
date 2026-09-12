source("scripts/00_functions.R")
source("oos_replication/scripts/metrics.R")
testthat::test_file("oos_replication/tests/test_metrics.R",
  reporter = "summary", stop_on_failure = TRUE
)

ratings <- readRDS("oos_replication/data/ratings.rds")
scores <- readRDS("oos_replication/data/group_results.rds")
stopifnot(
  !anyDuplicated(select(scores, event_id, episode_id, group_id, item_id, metric, membership)),
  !anyNA(scores$family_id),
  all(!is.na(scores$fieldwork_start) | scores$date_precision == "unknown"),
  all(is.na(scores$estimate) | between(scores$estimate, -1, 1))
)
reversed <- score_groups(ratings[rev(seq_len(nrow(ratings))), ], "paired")
observed <- scores |>
  filter(membership == "paired") |>
  select(all_of(names(reversed)))
testthat::expect_equal(as.data.frame(reversed), as.data.frame(observed), tolerance = 1e-12)

flow <- readr::read_csv("oos_replication/tabs/sample_flow.csv", show_col_types = FALSE)
realized <- ratings |> summarise(participants = n_distinct(participant_id), .by = event_id)
matched_flow <- left_join(realized, flow, by = "event_id", relationship = "one-to-one")
stopifnot(
  setequal(realized$event_id, flow$event_id),
  all(matched_flow$participants == matched_flow$eligible_participants)
)
message("Source counts, normalized bounds, unique keys, and full paired row-order check passed.")
