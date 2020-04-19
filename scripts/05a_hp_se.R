#
# Deliberative Distortions      
# Table 2: Polarization and Homogenization --- Standard Errors
#                          

# Set basedir
setwd(githubdir)
setwd("distortions/")

# Load lme4
library(lme4)
library(rmeta)

# Read in results
dom_ed <- read.csv("tabs/03_hom_pol_by_group_issue.csv")

# Get Issue 
dom_ed$issue <- gsub("^[0-9]*", "", dom_ed$unique_id)

res <- data.frame(poll_id = NA, polarfreq_se = 1:21, polarex_se = NA, homofreq_se = NA, homoex_se = NA)

j <- 1
for (i in unique(dom_ed$poll_id)){

    res$poll_id[j] <- i
    res$polarfreq_se[j] <- with(dom_ed[dom_ed$poll_id == i, ], summary(lmer(polarfreq ~  (1 | issue)))$coef[1, 2])
    res$polarex_se[j]   <- with(dom_ed[dom_ed$poll_id == i, ], summary(lmer(polarex ~  (1 | issue)))$coef[1, 2])
    res$homofreq_se[j]  <- with(dom_ed[dom_ed$poll_id == i, ], summary(lmer(homofreq ~  (1 | issue)))$coef[1, 2])
    res$homoex_se[j]    <- with(dom_ed[dom_ed$poll_id == i, ], summary(lmer(homoex ~  (1 | issue)))$coef[1, 2])

    j <- j + 1
}

# Load dat
bas_dat <- read.csv("tabs/02_table_2_hom_pol.csv")
bas_dat$pollnum[c(22, 23)] <- NA
fin_res <- merge(bas_dat, res, by.x = "pollnum", by.y = "poll_id", all.x = T, all.y = F)

# Mean 
fin_res$polarfreq_se[22] <- with(fin_res[1:21, ], meta.summaries(as.numeric(polarfreq), as.numeric(polarfreq_se), method = "fixed"))$se
fin_res$polarex_se[22]  <- with(fin_res[1:21, ], meta.summaries(as.numeric(polarex),  as.numeric(polarex_se),  method = "fixed"))$se
fin_res$homofreq_se[22] <- with(fin_res[1:21, ], meta.summaries(as.numeric(homofreq), as.numeric(homofreq_se), method = "fixed"))$se
fin_res$homoex_se[22]  <- with(fin_res[1:21, ], meta.summaries(as.numeric(homoex),  as.numeric(homoex_se),  method = "fixed"))$se

# Weighted Mean 
fin_res$polarfreq_se[23] <- with(fin_res[1:21, ], meta.summaries(as.numeric(polarfreq), as.numeric(polarfreq_se), method = "fixed",  weights = ngroups*nindices))$se
fin_res$polarex_se[23]  <- with(fin_res[1:21, ], meta.summaries(as.numeric(polarex),  as.numeric(polarex_se), method = "fixed",  weights = ngroups*nindices))$se
fin_res$homofreq_se[23] <- with(fin_res[1:21, ], meta.summaries(as.numeric(homofreq), as.numeric(homofreq_se), method = "fixed",  weights = ngroups*nindices))$se
fin_res$homoex_se[23]  <- with(fin_res[1:21, ], meta.summaries(as.numeric(homoex),  as.numeric(homoex_se), method = "fixed",  weights = ngroups*nindices))$se

# p-value of weighted mean 
fin_res[24, ] <- NA
fin_res$polarfreq_se[24] <- with(fin_res[1:21, ], meta.summaries(as.numeric(polarfreq) - .5, as.numeric(polarfreq_se), method = "fixed", weights = ngroups*nindices))$test[2]
fin_res$polarex_se[24]   <- with(fin_res[1:21, ], meta.summaries(as.numeric(polarex),  as.numeric(polarex_se), method = "fixed", weights = ngroups*nindices))$test[2]
fin_res$homofreq_se[24]  <- with(fin_res[1:21, ], meta.summaries(as.numeric(homofreq) - .5, as.numeric(homofreq_se), method = "fixed", weights = ngroups*nindices))$test[2]
fin_res$homoex_se[24]    <- with(fin_res[1:21, ], meta.summaries(as.numeric(homoex),  as.numeric(homoex_se), method = "fixed", weights = ngroups*nindices))$test[2]

# Precision weighted mean 
#fin_res[(nrow(fin_res)+1), ] <- c("", "Precision Wtd Mean", "", "", 
#                                                weighted.mean(fin_res$freqdis[1:21], w = 1/fin_res$freqdis_se[1:21]^2), 
#                                                weighted.mean(fin_res$extdis[1:21],  w = 1/fin_res$extdis_se[1:21]^2),
#                                                weighted.mean(fin_res$freqgrp[1:21], w = 1/fin_res$freqgrp_se[1:21]^2),
#                                                weighted.mean(fin_res$extgrp[1:21],  w = 1/fin_res$extgrp_se[1:21]^2),
#                                                sqrt(1/sum(1/fin_res$freqdis_se[1:11]^2)),
#                                                sqrt(1/sum(1/fin_res$extdis_se[1:11]^2)),
#                                                sqrt(1/sum(1/fin_res$freqgrp_se[1:11]^2)),
#                                                sqrt(1/sum(1/fin_res$extgrp_se[1:11]^2)))

write.csv(fin_res, file = "tabs/02_table_2_hom_pol_se.csv", row.names = F)
