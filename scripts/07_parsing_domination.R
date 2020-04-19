"			
Deliberative Distortions 	
Parsing Domination
"

# Set basedir
setwd(githubdir)
setwd("distortions/")

# Load libs
if ("package:plyr" %in% search()) detach(package:plyr)
library(dplyr)

# Domination by grp-issue pair 
dom_fem <-  read.csv("tabs/03_dom_fem_by_group_issue.csv")
dom_inc <-  read.csv("tabs/03_dom_highinc_by_group_issue.csv")
dom_ed  <-  read.csv("tabs/03_dom_ed_by_group_issue.csv")
dom_t3  <-  read.csv("tabs/03_dom_triple_by_group_issue.csv")

dom_fem_r <- read.csv("tabs/03_dom_men_by_group_issue.csv")
dom_inc_r <- read.csv("tabs/03_dom_lowinc_by_group_issue.csv")
dom_ed_r  <- read.csv("tabs/03_dom_lowed_by_group_issue.csv")
dom_t3_r  <- read.csv("tabs/03_dom_triple_disadv_by_group_issue.csv")

dpdat <- read.csv("data/polardata.csv")

# Proportion of Less Educated, Low Income per Group
p_female_group <- dpdat %>%
    group_by(pollgroup) %>%
    summarise(pfemale = mean(female == 1, na.rm = T))

p_led_group <- dpdat %>%
    group_by(pollgroup) %>%
    summarise(plowed = mean(bettered == 0, na.rm = T))

p_linc_group <- dpdat %>%
    group_by(pollgroup) %>%
    summarise(plinc = mean(highinc == 0, na.rm = T))

p_t3_group <- dpdat %>%
    group_by(pollgroup) %>%
    summarise(pt3 = mean(highinc == 0 & highinc == 0 & female == 1, na.rm = T))

# Left Join
dom_fem_pfem <- dom_fem %>%
  left_join(p_female_group, by = c("group_id" = "pollgroup"))

dom_fem_pfem_r <- dom_fem_r %>%
  left_join(p_female_group, by = c("group_id" = "pollgroup"))

dom_ed_pled <- dom_ed %>%
  left_join(p_led_group, by = c("group_id" = "pollgroup"))

dom_ed_pled_r <- dom_ed_r %>%
  left_join(p_led_group, by = c("group_id" = "pollgroup"))

dom_inc_plinc <- dom_inc %>%
  left_join(p_linc_group, by = c("group_id" = "pollgroup"))

dom_inc_plinc_r <- dom_inc_r %>%
  left_join(p_linc_group, by = c("group_id" = "pollgroup"))

dom_t3_p3   <- dom_t3 %>%
  left_join(p_t3_group, by = c("group_id" = "pollgroup"))

dom_t3_p3_r   <- dom_t3_r %>%
  left_join(p_t3_group, by = c("group_id" = "pollgroup"))

# Run the regressions
with(dom_fem_pfem, summary(lm(extdis_grp ~ pfemale)))
with(dom_fem_pfem_r, summary(lm(-extdis_grp ~ pfemale)))
with(dom_fem_pfem, summary(lm(extgrp_grp ~ pfemale)))

with(dom_ed_pled, summary(lm(extdis_grp ~ plowed)))
with(dom_ed_pled_r, summary(lm(-extdis_grp ~ plowed)))
with(dom_ed_pled, summary(lm(extgrp_grp ~ plowed)))

with(dom_inc_plinc, summary(lm(extdis_grp ~ plinc)))
with(dom_inc_plinc_r, summary(lm(-extdis_grp ~ plinc)))
with(dom_inc_plinc, summary(lm(extgrp_grp ~ plinc)))

with(dom_t3_p3, summary(lm(extdis_grp ~ pt3)))
with(dom_t3_p3_r, summary(lm(-extdis_grp ~ pt3)))
with(dom_t3_p3, summary(lm(extgrp_grp ~ pt3)))
