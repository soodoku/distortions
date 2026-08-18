## Replication Data and Scripts for Deliberative Distortions

Paper: https://doi.org/10.1017/S0007123421000168 (BJPS 52(3): 1205-1225); [preprint](http://gsood.com/research/papers/DeliberativeDistortions.pdf)

### Correction note

The published domination results do not implement Equation 3 consistently.
The analysis also uses stale policy-index counts as weights, conflates an
undefined domination direction with zero movement, duplicates 217 participant
records, and reports an uncertainty procedure that the code does not run.

The revised pipeline in [clean/](clean/) fixes these defects at their shared
sources. It analyzes 5,867 distinct participants, 397 groups, 129 policy
indices, and 2,480 group-index pairs. [AUDIT.md](AUDIT.md) states the corrected
conclusion and lists every affected claim. Machine-readable claim, value, and
artifact ledgers are in [provenance/](provenance/).

The original `scripts/`, `tabs/`, and `figs/` remain unchanged. They preserve
the provenance of the journal results and are not the source for corrected
estimates.

### Reproducibility

Direct dependencies are declared in [DESCRIPTION](DESCRIPTION), and resolved
versions are pinned in [renv.lock](renv.lock) for R 4.6.0. From the repository
root:

```sh
make ci
```

`make analysis` regenerates the corrected tables, figures, validation results,
and provenance ledgers. `make test` runs the analysis and test suite.

The SessionInfo below records the original published run and is retained only
for historical reproduction.

### Data

* [Data](data/polardata.csv)
* [Metadata on Polls](data/poll_indices.csv)

### Scripts and Outputs

1. [DP Summary](scripts/01_summary_dp_data_table_1.R)
    * Produces [Table 1](tabs/01_table_1_dp_summary.csv)

2. [Homogenization and Polarization by Poll and Aggregate](scripts/02_hom_pol_table_2_3.R)
    * [s.e.](scripts/05a_hp_se.R)
    * Produces [Table 2 Rows](tabs/02_table_2_hom_pol.csv)

3. Domination
    - [Education](scripts/03a_dom_educ.R)
    - [Gender](scripts/03b_dom_gender.R)
    - [Income](scripts/03c_dom_income.R)
    - [Education, Income, and Gender Combined](scripts/03d_dom_men_income_ed.R)
    * Produces Table 3 Rows
        * Underlying tables: 
            * [Female](tabs/04_table_4a_toward_female.csv)
            * [Male](tabs/04_table_4a_toward_male.csv)
            * [Lower Education](tabs/04_table_4b_toward_lowed.csv)
            * [Higher Education](tabs/04_table_4b_toward_highed.csv)
            * [Lower Income](tabs/04_table_4c_toward_lowinc.csv)
            * [Higher Income](tabs/04_table_4c_toward_highinc.csv)
            * [Triple Disadv.](tabs/04_table_4d_toward_triple.csv)
            * [Triple Adv.](tabs/04_table_4d_toward_triple_disadv.csv)
    * [s.e.](scripts/05b_dom_se.R)

4. [Correlation Between HPD](scripts/04_corr_hpd.R)
    * Produces [tabs/05_corr_hom_pol.csv](tabs/05_corr_hom_pol.csv)

5. [Figures](scripts/06_figs.R)
    - Uses output tables from steps 1, 2, and 3 to produce all the figures except those produced in step 7. 

6. [Parsing Domination](scripts/07_parsing_domination.R) produces in-text numbers for the section on parsing domination.

7. [Description of Groups](scripts/08_appendix_sample_description.R) produces exploratory figures that are not in the paper.

8. [Attitude Change](scripts/09_attitude_change.R) produces poll-level pre and post attitude means and standard deviations. The corrected producer for the net and gross change discussed on pp. 1220-1221 is [clean/05_attitude_change.R](clean/05_attitude_change.R).

9. [Run All Original Scripts](scripts/10_run_all.R)

### SessionInfo

```
R version 3.6.3 (2020-02-29)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 18362)

Matrix products: default

locale:
[1] LC_COLLATE=English_United States.1252  LC_CTYPE=English_United States.1252    LC_MONETARY=English_United States.1252
[4] LC_NUMERIC=C                           LC_TIME=English_United States.1252    

attached base packages:
[1] grid      stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] forcats_0.5.0   stringr_1.4.0   purrr_0.3.4     readr_1.3.1     tidyr_1.0.2     tibble_3.0.1    tidyverse_1.3.0
 [8] dplyr_0.8.5     ggplot2_3.3.0   rmeta_3.0       lme4_1.1-23     Matrix_1.2-18   goji_0.1.2     

loaded via a namespace (and not attached):
 [1] statmod_1.4.34   tidyselect_1.0.0 splines_3.6.3    haven_2.2.0      lattice_0.20-41  colorspace_1.4-1 vctrs_0.2.4     
 [8] generics_0.0.2   rlang_0.4.5      nloptr_1.2.2.1   pillar_1.4.3     glue_1.4.0       withr_2.1.2      DBI_1.1.0       
[15] dbplyr_1.4.3     modelr_0.1.6     readxl_1.3.1     lifecycle_0.2.0  plyr_1.8.6       munsell_0.5.0    gtable_0.3.0    
[22] cellranger_1.1.0 rvest_0.3.5      labeling_0.3     fansi_0.4.1      broom_0.5.5      Rcpp_1.0.4.6     scales_1.1.0    
[29] backports_1.1.6  jsonlite_1.6.1   farver_2.0.3     fs_1.4.1         hms_0.5.3        digest_0.6.25    stringi_1.4.6   
[36] cli_2.0.2        tools_3.6.3      magrittr_1.5     crayon_1.3.4     pkgconfig_2.0.3  MASS_7.3-51.5    ellipsis_0.3.0  
[43] xml2_1.3.1       reprex_0.3.0     lubridate_1.7.8  rstudioapi_0.11  assertthat_0.2.1 minqa_1.2.4      httr_1.4.1      
[50] R6_2.4.1         boot_1.3-24      nlme_3.1-147     compiler_3.6.3 
```

### Authors

Robert Luskin, Gaurav Sood, James Fishkin, Kyu Hahn
