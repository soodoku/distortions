"
Deliberative Distortions -- audit
Published versus fully corrected Table 2
"

source("clean/00_functions.R")

values <- read.csv("provenance/values.csv") |>
  filter(claim_id == "C007", version %in% c("published", "fully_corrected")) |>
  select(construct, dimension, measure, version, estimate) |>
  pivot_wider(names_from = version, values_from = estimate) |>
  mutate(
    change = fully_corrected - published,
    sign_changed = sign(fully_corrected) != sign(published) &
      abs(fully_corrected) > movement_eps & abs(published) > movement_eps,
    parity_changed = grepl("b$", measure, ignore.case = TRUE) &
      sign(fully_corrected - .5) != sign(published - .5)
  )

stopifnot(
  nrow(values) == 12,
  all(complete.cases(values[c("published", "fully_corrected")]))
)

write.csv(values, "tabs_clean/91_audit_comparison.csv", row.names = FALSE)
print(values)
