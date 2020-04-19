"											
Distortions
Run all the scripts
"

# Set basedir
setwd(githubdir)
setwd("distortions/")

# Set StringsAsFactors as FALSE 
options(stringsAsFactors = FALSE)

source("scripts/01_summary_dp_data_table_1.R", chdir = TRUE)
source("scripts/02_hom_pol_table_2_3.R", chdir = TRUE)
source("scripts/03a_dom_educ.R", chdir = TRUE)
source("scripts/03b_dom_gender.R", chdir = TRUE)
source("scripts/03c_dom_income.R", chdir = TRUE)
source("scripts/04_corr_hpd.R", chdir = TRUE)
source("scripts/05a_hp_se.R", chdir = TRUE)
source("scripts/05b_dom_se.R", chdir = TRUE)
source("scripts/06_figs.R", chdir = TRUE)
source("scripts/07_parsing_domination.R", chdir = TRUE)
source("scripts/08_appendix_sample_description.R", chdir = TRUE)
source("scripts/09_attitude_change.R", chdir = TRUE)
