source("scripts/00_functions.R")
source("oos_replication/scripts/metrics.R")
dir.create("oos_replication/data", showWarnings = FALSE)
dir.create("oos_replication/tabs", showWarnings = FALSE)

files <- readr::read_csv("oos_replication/files.csv", show_col_types = FALSE)
for (i in seq_len(nrow(files))) {
  path <- file.path("oos_replication/data", files$file[i])
  if (!file.exists(path)) {
    download.file(files$url[i], path, mode = "wb", quiet = TRUE)
  }
  stopifnot(identical(digest::digest(file = path, algo = "sha256"), files$sha256[i]))
}

source("oos_replication/scripts/prepare_a1r.R")
source("oos_replication/scripts/prepare_oboe.R")
source("oos_replication/scripts/prepare_hongkong.R")
source("oos_replication/scripts/prepare_tanzania.R")
source("oos_replication/scripts/prepare_climate.R")
ratings <- bind_rows(a1r, oboe, hongkong, tanzania, climate)
events <- readr::read_csv("oos_replication/events.csv", show_col_types = FALSE,
  col_types = readr::cols(.default = readr::col_character())
)
stopifnot(
  !anyDuplicated(events$event_id),
  all(ratings$event_id %in% events$event_id)
)

scores <- map_dfr(c("paired", "available"), \(sample) score_groups(ratings, sample)) |>
  left_join(events, by = "event_id", relationship = "many-to-one")

event_results <- scores |>
  summarise(
    pairs = sum(!is.na(estimate)),
    mean = na_if(mean(estimate, na.rm = TRUE), NaN),
    positive_fraction = na_if(mean(positive, na.rm = TRUE), NaN),
    .by = c(event_id, family_id, format, construct, membership, metric)
  )
minimum_five <- scores |>
  filter(n_pre >= 5, n_post >= 5) |>
  summarise(
    pairs = sum(!is.na(estimate)),
    mean = na_if(mean(estimate, na.rm = TRUE), NaN),
    positive_fraction = na_if(mean(positive, na.rm = TRUE), NaN),
    .by = c(event_id, family_id, format, construct, membership, metric)
  )

readr::write_csv(source_flow, "oos_replication/tabs/sample_flow.csv")
readr::write_csv(scores, "oos_replication/tabs/group_results.csv")
readr::write_csv(event_results, "oos_replication/tabs/event_results.csv")
readr::write_csv(minimum_five, "oos_replication/tabs/minimum_five.csv")
source("oos_replication/scripts/summarize.R")
saveRDS(ratings, "oos_replication/data/ratings.rds")
capture.output(sessionInfo(), file = "oos_replication/tabs/session_info.txt")
print(event_results, n = 12)
