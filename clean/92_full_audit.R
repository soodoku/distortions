"
Deliberative Distortions -- audit
Claim, value, artifact, and validation coverage
"

source("clean/00_functions.R")

claims <- read.csv("provenance/claims.csv")
values <- read.csv("provenance/values.csv")
artifacts <- read.csv("provenance/artifacts.csv")
checks <- read.csv("provenance/checks.csv")

claim_audit <- claims |>
  left_join(
    values |>
      summarise(
        value_rows = n(),
        versions = paste(sort(unique(version)), collapse = ","),
        .by = claim_id
      ),
    by = "claim_id"
  ) |>
  mutate(
    value_rows = coalesce(value_rows, 0L),
    versions = coalesce(versions, "none"),
    artifact_exists = artifact == "direct arithmetic" |
      artifact == "No untreated comparison or measured mechanism" |
      file.exists(artifact)
  )

write.csv(claim_audit, "tabs_clean/92_full_audit.csv", row.names = FALSE)

stopifnot(all(artifacts$exists), all(checks$passed), all(claim_audit$artifact_exists))
print(claim_audit)
