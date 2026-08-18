"
Deliberative Distortions -- audit
Run the ORIGINAL scripts, byte-unmodified, against a scratch copy of the repo
and value-compare every regenerated tabs/*.csv against the committed file.
The committed tabs/ are never touched.

Usage: Rscript clean/90_repro_original.R   (from the repo root)
Scratch location can be overridden with the REPRO_SCRATCH env var.
"

repo <- normalizePath(".")
stopifnot(file.exists(file.path(repo, "data", "polardata.csv")))

scratch <- Sys.getenv("REPRO_SCRATCH", file.path(tempdir(), "distortions_repro"))
repo_copy <- file.path(scratch, "distortions")

unlink(repo_copy, recursive = TRUE)
dir.create(file.path(repo_copy, "tabs"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(repo_copy, "figs"), showWarnings = FALSE)
file.copy(file.path(repo, "data"), repo_copy, recursive = TRUE)
file.copy(file.path(repo, "scripts"), repo_copy, recursive = TRUE)

# The original scripts start with setwd(githubdir); setwd("distortions/")
githubdir <- scratch

# 10_run_all.R order, with 03d inserted after 03c. 10_run_all.R omits 03d even
# though 05b and 07 read its outputs, so the committed triple tables required a
# manual 03d run; sourcing it here documents that and lets 05b/07 complete.
run_order <- c(
  "01_summary_dp_data_table_1.R",
  "02_hom_pol_table_2_3.R",
  "03a_dom_educ.R",
  "03b_dom_gender.R",
  "03c_dom_income.R",
  "03d_dom_men_income_ed.R",
  "04_corr_hpd.R",
  "05a_hp_se.R",
  "05b_dom_se.R",
  "06_figs.R",
  "07_parsing_domination.R",
  "08_appendix_sample_description.R",
  "09_attitude_change.R"
)

status <- character(length(run_order))
names(status) <- run_order
for (s in run_order) {
  message("sourcing ", s)
  status[s] <- tryCatch(
    {
      source(file.path(repo_copy, "scripts", s))
      "ok"
    },
    error = function(e) paste("ERROR:", conditionMessage(e))
  )
}

# Value-level comparison of every regenerated CSV against the committed one.
# Cells that parse as numbers in both files are compared with a tolerance
# (SE files get a looser one: lme4/rmeta version drift); other cells exactly.
compare_csv <- function(f_new, f_old, tol) {
  a <- read.csv(f_new, colClasses = "character")
  b <- read.csv(f_old, colClasses = "character")
  if (!identical(dim(a), dim(b)) || !identical(names(a), names(b))) {
    return(list(
      match = FALSE, n_mismatch = NA, max_diff = NA,
      note = sprintf(
        "shape %dx%d vs %dx%d",
        nrow(a), ncol(a), nrow(b), ncol(b)
      )
    ))
  }
  a <- as.matrix(a)
  b <- as.matrix(b)
  an <- suppressWarnings(as.numeric(a))
  bn <- suppressWarnings(as.numeric(b))
  both_num <- !is.na(an) & !is.na(bn)
  both_na <- (is.na(a) | a == "NA" | a == "NaN") & (is.na(b) | b == "NA" | b == "NaN")
  diffs <- abs(an - bn)[both_num]
  num_bad <- sum(diffs > tol)
  txt_bad <- sum(!both_num & !both_na & (is.na(a != b) | a != b))
  list(
    match = num_bad + txt_bad == 0, n_mismatch = num_bad + txt_bad,
    max_diff = if (length(diffs)) max(diffs) else 0, note = ""
  )
}

committed <- list.files(file.path(repo, "tabs"), pattern = "\\.csv$")
manifest <- data.frame(
  file = committed, regenerated = NA, match = NA,
  n_mismatch = NA, max_diff = NA, note = ""
)
for (i in seq_along(committed)) {
  f <- committed[i]
  f_new <- file.path(repo_copy, "tabs", f)
  manifest$regenerated[i] <- file.exists(f_new)
  if (!manifest$regenerated[i]) next
  tol <- if (grepl("_se\\.csv$", f)) 1e-4 else 1e-8
  cmp <- compare_csv(f_new, file.path(repo, "tabs", f), tol)
  manifest$match[i] <- cmp$match
  manifest$n_mismatch[i] <- cmp$n_mismatch
  manifest$max_diff[i] <- cmp$max_diff
  manifest$note[i] <- cmp$note
}

setwd(repo)
dir.create("tabs_clean", showWarnings = FALSE)
write.csv(data.frame(script = run_order, status = status),
  "tabs_clean/90_repro_script_status.csv",
  row.names = FALSE
)
write.csv(manifest, "tabs_clean/90_repro_manifest.csv", row.names = FALSE)

message("\nscript status:")
print(data.frame(status), right = FALSE)
message("\ncomparison manifest:")
print(manifest, right = FALSE)
message("\nscratch copy: ", repo_copy)
