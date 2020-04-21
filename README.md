## Replication Data and Scripts for Deliberative Distortions

Paper: http://gsood.com/research/papers/DeliberativeDistortions.pdf

### Data

* [Data](data/polardata.csv)
* [Metadata on Polls](data/poll_indices.csv)

### Scripts and Outputs

* [DP Summary](scripts/01_summary_dp_data_table_1.R)
    * Produces [Table 1](tabs/01_table_1_dp_summary.csv)

* [Homogenization and Polarization by Poll and Aggregate](scripts/02_hom_pol_table_2_3.R)
    * [s.e.](scripts/05a_hp_se.R)
    * Produces [Table 2 Rows](tabs/02_table_2_hom_pol.csv)
    

* Domination
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

* [Correlation Between HPD](scripts/04_corr_hpd.R)
    * Produces [tabs/05_corr_hom_pol.csv]

* [Figures](scripts/)
    - Produces 

* [Table 4. DP Summary.](tabs/01_table_1_dp_summary.csv)

* [Table 5. DP Summary.](tabs/01_table_1_dp_summary.csv)

* [Table 6. DP Summary.](tabs/01_table_1_dp_summary.csv)

* [Table 7. DP Summary.](tabs/01_table_1_dp_summary.csv)

### Authors

Robert Luskin, Gaurav Sood, James Fishkin, Kyu Hahn
