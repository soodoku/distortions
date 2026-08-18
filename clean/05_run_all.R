"
Deliberative Distortions -- clean pipeline
Run everything. From the repo root: Rscript clean/05_run_all.R
"

stopifnot(file.exists("data/polardata.csv"))
dir.create("tabs_clean", showWarnings = FALSE)

source("clean/00_inventory.R")
source("clean/01_hom_pol.R")
source("clean/02_domination.R")
source("clean/03_se.R")
source("clean/04_corr_parsing.R")
source("clean/05_attitude_change.R")
source("clean/06_figs.R")
source("clean/07_validate.R")
source("clean/08_provenance.R")
