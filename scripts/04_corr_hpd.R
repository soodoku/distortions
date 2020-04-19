"
Distortions
Corr. H, P and D
Crosstab of H > 0, P > 0
"


# Set basedir
setwd(githubdir)
setwd("distortions/")

# Load data
pol_hom <- read.csv("tabs/03_hom_pol_by_group_issue.csv")

dom_edu <- read.csv("tabs/03_dom_ed_by_group_issue.csv")
names(dom_edu) <- paste0("ed_", names(dom_edu))
pd      <- merge(pol_hom, dom_edu, by.x = "unique_id", by.y = "ed_unique_id", all.x = T, all.y = T)

dom_fem <- read.csv("tabs/03_dom_fem_by_group_issue.csv")
names(dom_fem) <- paste0("fem_", names(dom_fem))
pdf     <- merge(pd, dom_fem, by.x = "unique_id", by.y = "fem_unique_id", all.x = T, all.y = T)

dom_inc <- read.csv("tabs/03_dom_highinc_by_group_issue.csv")
names(dom_inc) <- paste0("inc_", names(dom_inc))
pdfi    <- merge(pdf, dom_inc, by.x = "unique_id", by.y = "inc_unique_id", all.x = T, all.y = T)

cor_cols <- c("polarfreq", "homofreq", "polarex", "homoex",
	          "fem_freqgrp_grp", "fem_extgrp_grp", "fem_freqdis_grp", "fem_extdis_grp",
	          "inc_freqgrp_grp", "inc_extgrp_grp", "inc_freqdis_grp", "inc_extdis_grp",
	          "ed_freqgrp_grp", "ed_extgrp_grp", "ed_freqdis_grp", "ed_extdis_grp")

write.csv(cor(pdfi[, cor_cols], use = "na.or.complete"), file = "tabs/05_corr_hpd.csv", row.names = F)

# Crosstabs
table(pol_hom$polarex > 0, pol_hom$homoex > 0)
