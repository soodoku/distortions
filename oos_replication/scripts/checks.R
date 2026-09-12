source("scripts/00_functions.R")
source("oos_replication/scripts/metrics.R")
testthat::test_file("oos_replication/tests/test_metrics.R", reporter = "summary", stop_on_failure = TRUE)

ratings <- readRDS("oos_replication/data/ratings.rds")
scores <- readr::read_csv("oos_replication/tabs/group_results.csv", show_col_types = FALSE,
  col_types = readr::cols(
    .default = readr::col_character(), midpoint = readr::col_double(),
    estimate = readr::col_double(), n_pre = readr::col_integer(),
    n_post = readr::col_integer(), n_paired = readr::col_integer(),
    positive = readr::col_logical()
  )
)
stopifnot(
  nrow(readr::problems(scores)) == 0L,
  !anyDuplicated(scores[c("event_id", "episode_id", "group_id", "item_id", "metric", "membership")]),
  !anyNA(scores$family_id), !anyNA(scores$fieldwork_start),
  all(is.na(scores$estimate) | between(scores$estimate, -1, 1))
)
reversed <- score_groups(ratings[nrow(ratings):1, ], "paired")
observed <- scores |>
  filter(membership == "paired") |>
  select(all_of(names(reversed)))
testthat::expect_equal(as.data.frame(reversed), as.data.frame(observed), tolerance = 1e-12)

flow <- readr::read_csv("oos_replication/tabs/sample_flow.csv", show_col_types = FALSE)
realized <- ratings |> summarise(participants = n_distinct(participant_id), .by = event_id)
stopifnot(
  setequal(realized$event_id, flow$event_id),
  all(left_join(realized, flow, by = "event_id", relationship = "one-to-one") |>
    transmute(equal = participants == eligible_participants) |> pull(equal))
)
message("Source counts, normalized bounds, unique keys, and full paired row-order check passed.")
