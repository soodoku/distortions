#
# Deliberative Distortions      
# Table 2: Unequal Influence --- Standard Errors
#

# Set basedir
setwd(githubdir)
setwd("distortions/")

# Load lme4
library(lme4)
library(rmeta)

# Toward HigherEd
# ~~~~~~~~~~~~~~~~~~~~~~~~~~`

# Read in results
dom_ed <- read.csv("tabs/03_dom_ed_by_group_issue.csv")

# Get Issue 
dom_ed$issue <- gsub("^[0-9]*", "", dom_ed$unique_id)

res <- data.frame(poll_id = NA, freqdis_se = 1:21, extdis_se = NA, freqgrp_se = NA, extgrp_se = NA)

j <- 1
for (i in unique(dom_ed$poll_id)) {

    res$poll_id[j] <- i
    res$freqdis_se[j] <- with(dom_ed[dom_ed$poll_id == i, ], summary(lmer(freqdis_grp ~  (1 | issue)))$coef[1, 2])
    res$extdis_se[j]  <- with(dom_ed[dom_ed$poll_id == i, ], summary(lmer(extdis_grp ~  (1 | issue)))$coef[1, 2])
    res$freqgrp_se[j] <- with(dom_ed[dom_ed$poll_id == i, ], summary(lmer(freqgrp_grp ~  (1 | issue)))$coef[1, 2])
    res$extgrp_se[j]  <- with(dom_ed[dom_ed$poll_id == i, ], summary(lmer(extgrp_grp ~  (1 | issue)))$coef[1, 2])

    j <- j + 1
}

# Load dat
bas_dat <- read.csv("tabs/04_table_4b_toward_highed.csv")
bas_dat$pollnum[c(22, 23)] <- NA
fin_res <- merge(bas_dat, res, by.x = "pollnum", by.y = "poll_id", all.x = T, all.y = F)

# Mean 
fin_res$freqdis_se[22] <- with(fin_res[1:21, ], meta.summaries(as.numeric(freqdis), as.numeric(freqdis_se), method = "fixed"))$se
fin_res$extdis_se[22]  <- with(fin_res[1:21, ], meta.summaries(as.numeric(extdis),  as.numeric(extdis_se),  method = "fixed"))$se
fin_res$freqgrp_se[22] <- with(fin_res[1:21, ], meta.summaries(as.numeric(freqgrp), as.numeric(freqgrp_se), method = "fixed"))$se
fin_res$extgrp_se[22]  <- with(fin_res[1:21, ], meta.summaries(as.numeric(extgrp),  as.numeric(extgrp_se),  method = "fixed"))$se

# Weighted Mean 
fin_res$freqdis_se[23] <- with(fin_res[1:21, ], meta.summaries(as.numeric(freqdis), as.numeric(freqdis_se), method = "fixed", weights = ngroups*nindices))$se
fin_res$extdis_se[23]  <- with(fin_res[1:21, ], meta.summaries(as.numeric(extdis),  as.numeric(extdis_se), method = "fixed", weights = ngroups*nindices))$se
fin_res$freqgrp_se[23] <- with(fin_res[1:21, ], meta.summaries(as.numeric(freqgrp), as.numeric(freqgrp_se), method = "fixed", weights = ngroups*nindices))$se
fin_res$extgrp_se[23]  <- with(fin_res[1:21, ], meta.summaries(as.numeric(extgrp),  as.numeric(extgrp_se), method = "fixed", weights = ngroups*nindices))$se

# p-value of weighted mean 
fin_res[24, ] <- NA 
fin_res$freqdis_se[24] <- with(fin_res[1:21, ], meta.summaries(as.numeric(freqdis) -.5, as.numeric(freqdis_se), method = "fixed", weights = ngroups*nindices))$test[2]
fin_res$extdis_se[24]  <- with(fin_res[1:21, ], meta.summaries(as.numeric(extdis),  as.numeric(extdis_se), method = "fixed", weights = ngroups*nindices))$test[2]
fin_res$freqgrp_se[24] <- with(fin_res[1:21, ], meta.summaries(as.numeric(freqgrp) -.5, as.numeric(freqgrp_se), method = "fixed", weights = ngroups*nindices))$test[2]
fin_res$extgrp_se[24]  <- with(fin_res[1:21, ], meta.summaries(as.numeric(extgrp),  as.numeric(extgrp_se), method = "fixed", weights = ngroups*nindices))$test[2]

# Precision weighted mean 
fin_res[(nrow(fin_res) + 1), ] <- c("", "Precision Wtd Mean", "", "", 
                                                weighted.mean(fin_res$freqdis[1:21], w = 1/fin_res$freqdis_se[1:21]^2), 
                                                weighted.mean(fin_res$extdis[1:21],  w = 1/fin_res$extdis_se[1:21]^2),
                                                weighted.mean(fin_res$freqgrp[1:21], w = 1/fin_res$freqgrp_se[1:21]^2),
                                                weighted.mean(fin_res$extgrp[1:21],  w = 1/fin_res$extgrp_se[1:21]^2),
                                                sqrt(1/sum(1/fin_res$freqdis_se[1:11]^2)),
                                                sqrt(1/sum(1/fin_res$extdis_se[1:11]^2)),
                                                sqrt(1/sum(1/fin_res$freqgrp_se[1:11]^2)),
                                                sqrt(1/sum(1/fin_res$extgrp_se[1:11]^2)))

write.csv(fin_res, file = "tabs/04_table_4b_toward_highed_se.csv", row.names = F)


# Toward LowerEd
# ~~~~~~~~~~~~~~~~~~~~~~~~~~`

# Read in results
dom_ed <- read.csv("tabs/03_dom_lowed_by_group_issue.csv")

# Get Issue 
dom_ed$issue <- gsub("^[0-9]*", "", dom_ed$unique_id)

res <- data.frame(poll_id = NA, freqdis_se = 1:21, extdis_se = NA, freqgrp_se = NA, extgrp_se = NA)

j <- 1
for (i in unique(dom_ed$poll_id)) {

    res$poll_id[j] <- i
    res$freqdis_se[j] <- with(dom_ed[dom_ed$poll_id == i, ], summary(lmer(freqdis_grp ~  (1 | issue)))$coef[1, 2])
    res$extdis_se[j]  <- with(dom_ed[dom_ed$poll_id == i, ], summary(lmer(extdis_grp ~  (1 | issue)))$coef[1, 2])
    res$freqgrp_se[j] <- with(dom_ed[dom_ed$poll_id == i, ], summary(lmer(freqgrp_grp ~  (1 | issue)))$coef[1, 2])
    res$extgrp_se[j]  <- with(dom_ed[dom_ed$poll_id == i, ], summary(lmer(extgrp_grp ~  (1 | issue)))$coef[1, 2])

    j <- j + 1
}

# Load dat
bas_dat <- read.csv("tabs/04_table_4b_toward_lowed.csv")
bas_dat$pollnum[c(22, 23)] <- NA
fin_res <- merge(bas_dat, res, by.x = "pollnum", by.y = "poll_id", all.x = T, all.y = F)

# Mean 
fin_res$freqdis_se[22] <- with(fin_res[1:21, ], meta.summaries(as.numeric(freqdis), as.numeric(freqdis_se), method = "fixed"))$se
fin_res$extdis_se[22]  <- with(fin_res[1:21, ], meta.summaries(as.numeric(extdis),  as.numeric(extdis_se),  method = "fixed"))$se
fin_res$freqgrp_se[22] <- with(fin_res[1:21, ], meta.summaries(as.numeric(freqgrp), as.numeric(freqgrp_se), method = "fixed"))$se
fin_res$extgrp_se[22]  <- with(fin_res[1:21, ], meta.summaries(as.numeric(extgrp),  as.numeric(extgrp_se),  method = "fixed"))$se

# Weighted Mean 
fin_res$freqdis_se[23] <- with(fin_res[1:21, ], meta.summaries(as.numeric(freqdis), as.numeric(freqdis_se), method = "fixed", weights = ngroups*nindices))$se
fin_res$extdis_se[23]  <- with(fin_res[1:21, ], meta.summaries(as.numeric(extdis),  as.numeric(extdis_se), method  = "fixed", weights = ngroups*nindices))$se
fin_res$freqgrp_se[23] <- with(fin_res[1:21, ], meta.summaries(as.numeric(freqgrp), as.numeric(freqgrp_se), method = "fixed", weights = ngroups*nindices))$se
fin_res$extgrp_se[23]  <- with(fin_res[1:21, ], meta.summaries(as.numeric(extgrp),  as.numeric(extgrp_se), method  = "fixed", weights = ngroups*nindices))$se


# p-value of weighted mean 
fin_res[24, ] <- NA 
fin_res$freqdis_se[24] <- with(fin_res[1:21, ], meta.summaries(as.numeric(freqdis) -.5, as.numeric(freqdis_se), method = "fixed", weights=ngroups*nindices))$test[2]
fin_res$extdis_se[24]  <- with(fin_res[1:21, ], meta.summaries(as.numeric(extdis),  as.numeric(extdis_se), method = "fixed", weights=ngroups*nindices))$test[2]
fin_res$freqgrp_se[24] <- with(fin_res[1:21, ], meta.summaries(as.numeric(freqgrp) -.5, as.numeric(freqgrp_se), method = "fixed", weights=ngroups*nindices))$test[2]
fin_res$extgrp_se[24]  <- with(fin_res[1:21, ], meta.summaries(as.numeric(extgrp),  as.numeric(extgrp_se), method = "fixed", weights=ngroups*nindices))$test[2]

# Precision weighted mean 
fin_res[(nrow(fin_res) + 1), ] <- c("", "Precision Wtd Mean", "", "", 
                                                weighted.mean(fin_res$freqdis[1:21], w = 1/fin_res$freqdis_se[1:21]^2), 
                                                weighted.mean(fin_res$extdis[1:21],  w = 1/fin_res$extdis_se[1:21]^2),
                                                weighted.mean(fin_res$freqgrp[1:21], w = 1/fin_res$freqgrp_se[1:21]^2),
                                                weighted.mean(fin_res$extgrp[1:21],  w = 1/fin_res$extgrp_se[1:21]^2),
                                                sqrt(1/sum(1/fin_res$freqdis_se[1:11]^2)),
                                                sqrt(1/sum(1/fin_res$extdis_se[1:11]^2)),
                                                sqrt(1/sum(1/fin_res$freqgrp_se[1:11]^2)),
                                                sqrt(1/sum(1/fin_res$extgrp_se[1:11]^2)))

write.csv(fin_res, file = "tabs/04_table_4b_toward_lowed_se.csv", row.names = F)

# Toward Men
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Read in results
dom_ed <- read.csv("tabs/03_dom_fem_by_group_issue.csv")

# Get Issue 
dom_ed$issue <- gsub("^[0-9]*", "", dom_ed$unique_id)

res <- data.frame(poll_id = NA, freqdis_se = 1:21, extdis_se = NA, freqgrp_se = NA, extgrp_se = NA)

j <- 1
for (i in unique(dom_ed$poll_id)) {

    res$poll_id[j]    <- i
    res$freqdis_se[j] <- with(dom_ed[dom_ed$poll_id == i, ], summary(lmer(freqdis_grp ~  (1 | issue)))$coef[1, 2])
    res$extdis_se[j]  <- with(dom_ed[dom_ed$poll_id == i, ], summary(lmer(extdis_grp ~  (1 | issue)))$coef[1, 2])
    res$freqgrp_se[j] <- with(dom_ed[dom_ed$poll_id == i, ], summary(lmer(freqgrp_grp ~  (1 | issue)))$coef[1, 2])
    res$extgrp_se[j]  <- with(dom_ed[dom_ed$poll_id == i, ], summary(lmer(extgrp_grp ~  (1 | issue)))$coef[1, 2])

    j <- j + 1
}

# Load dat
bas_dat <- read.csv("tabs/04_table_4a_toward_male.csv")
bas_dat$pollnum[c(22, 23)] <- NA
fin_res <- merge(bas_dat, res, by.x = "pollnum", by.y = "poll_id", all.x = T, all.y = F)


# Mean 
fin_res$freqdis_se[22] <- with(fin_res[1:21, ], meta.summaries(as.numeric(freqdis), as.numeric(freqdis_se), method = "fixed"))$se
fin_res$extdis_se[22]  <- with(fin_res[1:21, ], meta.summaries(as.numeric(extdis),  as.numeric(extdis_se),  method = "fixed"))$se
fin_res$freqgrp_se[22] <- with(fin_res[1:21, ], meta.summaries(as.numeric(freqgrp), as.numeric(freqgrp_se), method = "fixed"))$se
fin_res$extgrp_se[22]  <- with(fin_res[1:21, ], meta.summaries(as.numeric(extgrp),  as.numeric(extgrp_se),  method = "fixed"))$se

# Weighted Mean 
fin_res$freqdis_se[23] <- with(fin_res[1:21, ], meta.summaries(as.numeric(freqdis), as.numeric(freqdis_se), method = "fixed", weights=ngroups*nindices))$se
fin_res$extdis_se[23]  <- with(fin_res[1:21, ], meta.summaries(as.numeric(extdis),  as.numeric(extdis_se), method = "fixed", weights=ngroups*nindices))$se
fin_res$freqgrp_se[23] <- with(fin_res[1:21, ], meta.summaries(as.numeric(freqgrp), as.numeric(freqgrp_se), method = "fixed", weights=ngroups*nindices))$se
fin_res$extgrp_se[23]  <- with(fin_res[1:21, ], meta.summaries(as.numeric(extgrp),  as.numeric(extgrp_se), method = "fixed", weights=ngroups*nindices))$se


# p-value of weighted mean 
fin_res[24, ] <- NA 
fin_res$freqdis_se[24] <- with(fin_res[1:21, ], meta.summaries(as.numeric(freqdis) -.5, as.numeric(freqdis_se), method = "fixed", weights=ngroups*nindices))$test[2]
fin_res$extdis_se[24]  <- with(fin_res[1:21, ], meta.summaries(as.numeric(extdis),  as.numeric(extdis_se), method = "fixed", weights=ngroups*nindices))$test[2]
fin_res$freqgrp_se[24] <- with(fin_res[1:21, ], meta.summaries(as.numeric(freqgrp) -.5, as.numeric(freqgrp_se), method = "fixed", weights=ngroups*nindices))$test[2]
fin_res$extgrp_se[24]  <- with(fin_res[1:21, ], meta.summaries(as.numeric(extgrp),  as.numeric(extgrp_se), method = "fixed", weights=ngroups*nindices))$test[2]

# Add precision weighted mean 
fin_res[(nrow(fin_res)+1), ] <- c("", "Precision Wtd Mean", "", "", 
                                                weighted.mean(fin_res$freqdis[1:21], w = 1/fin_res$freqdis_se[1:21]^2), 
                                                weighted.mean(fin_res$extdis[1:21],  w = 1/fin_res$extdis_se[1:21]^2),
                                                weighted.mean(fin_res$freqgrp[1:21], w = 1/fin_res$freqgrp_se[1:21]^2),
                                                weighted.mean(fin_res$extgrp[1:21],  w = 1/fin_res$extgrp_se[1:21]^2),
                                                sqrt(1/sum(1/fin_res$freqdis_se[1:11]^2)),
                                                sqrt(1/sum(1/fin_res$extdis_se[1:11]^2)),
                                                sqrt(1/sum(1/fin_res$freqgrp_se[1:11]^2)),
                                                sqrt(1/sum(1/fin_res$extgrp_se[1:11]^2)))

write.csv(fin_res, file = "tabs/04_table_4a_toward_male_se.csv", row.names = F)

# Toward Women
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Read in results
dom_ed <- read.csv("tabs/03_dom_men_by_group_issue.csv")

# Get Issue 
dom_ed$issue <- gsub("^[0-9]*", "", dom_ed$unique_id)

res <- data.frame(poll_id = NA, freqdis_se = 1:21, extdis_se = NA, freqgrp_se = NA, extgrp_se = NA)

j <- 1
for (i in unique(dom_ed$poll_id)) {

    res$poll_id[j]    <- i
    res$freqdis_se[j] <- with(dom_ed[dom_ed$poll_id == i, ], summary(lmer(freqdis_grp ~  (1 | issue)))$coef[1, 2])
    res$extdis_se[j]  <- with(dom_ed[dom_ed$poll_id == i, ], summary(lmer(extdis_grp ~  (1 | issue)))$coef[1, 2])
    res$freqgrp_se[j] <- with(dom_ed[dom_ed$poll_id == i, ], summary(lmer(freqgrp_grp ~  (1 | issue)))$coef[1, 2])
    res$extgrp_se[j]  <- with(dom_ed[dom_ed$poll_id == i, ], summary(lmer(extgrp_grp ~  (1 | issue)))$coef[1, 2])

    j <- j + 1
}

# Load dat
bas_dat <- read.csv("tabs/04_table_4a_toward_female.csv")
bas_dat$pollnum[c(22, 23)] <- NA
fin_res <- merge(bas_dat, res, by.x = "pollnum", by.y = "poll_id", all.x = T, all.y = F)


# Mean 
fin_res$freqdis_se[22] <- with(fin_res[1:21, ], meta.summaries(as.numeric(freqdis), as.numeric(freqdis_se), method = "fixed"))$se
fin_res$extdis_se[22]  <- with(fin_res[1:21, ], meta.summaries(as.numeric(extdis),  as.numeric(extdis_se),  method = "fixed"))$se
fin_res$freqgrp_se[22] <- with(fin_res[1:21, ], meta.summaries(as.numeric(freqgrp), as.numeric(freqgrp_se), method = "fixed"))$se
fin_res$extgrp_se[22]  <- with(fin_res[1:21, ], meta.summaries(as.numeric(extgrp),  as.numeric(extgrp_se),  method = "fixed"))$se

# Weighted Mean 
fin_res$freqdis_se[23] <- with(fin_res[1:21, ], meta.summaries(as.numeric(freqdis), as.numeric(freqdis_se), method = "fixed", weights = ngroups*nindices))$se
fin_res$extdis_se[23]  <- with(fin_res[1:21, ], meta.summaries(as.numeric(extdis),  as.numeric(extdis_se), method = "fixed", weights = ngroups*nindices))$se
fin_res$freqgrp_se[23] <- with(fin_res[1:21, ], meta.summaries(as.numeric(freqgrp), as.numeric(freqgrp_se), method = "fixed", weights = ngroups*nindices))$se
fin_res$extgrp_se[23]  <- with(fin_res[1:21, ], meta.summaries(as.numeric(extgrp),  as.numeric(extgrp_se), method = "fixed", weights = ngroups*nindices))$se


# p-value of weighted mean 
fin_res[24, ] <- NA 
fin_res$freqdis_se[24] <- with(fin_res[1:21, ], meta.summaries(as.numeric(freqdis) -.5, as.numeric(freqdis_se), method = "fixed", weights = ngroups*nindices))$test[2]
fin_res$extdis_se[24]  <- with(fin_res[1:21, ], meta.summaries(as.numeric(extdis),  as.numeric(extdis_se), method = "fixed", weights = ngroups*nindices))$test[2]
fin_res$freqgrp_se[24] <- with(fin_res[1:21, ], meta.summaries(as.numeric(freqgrp) -.5, as.numeric(freqgrp_se), method = "fixed", weights = ngroups*nindices))$test[2]
fin_res$extgrp_se[24]  <- with(fin_res[1:21, ], meta.summaries(as.numeric(extgrp),  as.numeric(extgrp_se), method = "fixed", weights = ngroups*nindices))$test[2]

# Add precision weighted mean 
fin_res[(nrow(fin_res) + 1), ] <- c("", "Precision Wtd Mean", "", "", 
                                                weighted.mean(fin_res$freqdis[1:21], w = 1/fin_res$freqdis_se[1:21]^2), 
                                                weighted.mean(fin_res$extdis[1:21],  w = 1/fin_res$extdis_se[1:21]^2),
                                                weighted.mean(fin_res$freqgrp[1:21], w = 1/fin_res$freqgrp_se[1:21]^2),
                                                weighted.mean(fin_res$extgrp[1:21],  w = 1/fin_res$extgrp_se[1:21]^2),
                                                sqrt(1/sum(1/fin_res$freqdis_se[1:11]^2)),
                                                sqrt(1/sum(1/fin_res$extdis_se[1:11]^2)),
                                                sqrt(1/sum(1/fin_res$freqgrp_se[1:11]^2)),
                                                sqrt(1/sum(1/fin_res$extgrp_se[1:11]^2))
)
write.csv(fin_res, file = "tabs/04_table_4a_toward_female_se.csv", row.names = F)

# Toward Higher Income
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Read in results
dom_ed <- read.csv("tabs/03_dom_highinc_by_group_issue.csv")

# Get Issue 
dom_ed$issue <- gsub("^[0-9]*", "", dom_ed$unique_id)

res <- data.frame(poll_id = NA, freqdis_se = 1:11, extdis_se = NA, freqgrp_se = NA, extgrp_se = NA)

j <- 1
for (i in unique(dom_ed$poll_id)) {

    res$poll_id[j] <- i
    res$freqdis_se[j] <- with(dom_ed[dom_ed$poll_id == i, ], summary(lmer(freqdis_grp ~  (1 | issue)))$coef[1, 2])
    res$extdis_se[j]  <- with(dom_ed[dom_ed$poll_id == i, ], summary(lmer(extdis_grp ~  (1 | issue)))$coef[1, 2])
    res$freqgrp_se[j] <- with(dom_ed[dom_ed$poll_id == i, ], summary(lmer(freqgrp_grp ~  (1 | issue)))$coef[1, 2])
    res$extgrp_se[j]  <- with(dom_ed[dom_ed$poll_id == i, ], summary(lmer(extgrp_grp ~  (1 | issue)))$coef[1, 2])

    j <- j + 1
}

# Load dat
bas_dat <- read.csv("tabs/04_table_4c_toward_highinc.csv")
bas_dat <- bas_dat[-12, ]
bas_dat$pollnum[c(12, 13)] <- NA
fin_res <- merge(bas_dat, res, by.x = "pollnum", by.y = "poll_id", all.x = T, all.y = F)

# Mean 
fin_res$freqdis_se[13] <- with(fin_res[1:11, ], meta.summaries(as.numeric(freqdis), as.numeric(freqdis_se), method = "fixed"))$se
fin_res$extdis_se[13]  <- with(fin_res[1:11, ], meta.summaries(as.numeric(extdis),  as.numeric(extdis_se),  method = "fixed"))$se
fin_res$freqgrp_se[13] <- with(fin_res[1:11, ], meta.summaries(as.numeric(freqgrp), as.numeric(freqgrp_se), method = "fixed"))$se
fin_res$extgrp_se[13]  <- with(fin_res[1:11, ], meta.summaries(as.numeric(extgrp),  as.numeric(extgrp_se),  method = "fixed"))$se

# Weighted Mean 
fin_res$freqdis_se[12] <- with(fin_res[1:11, ], meta.summaries(as.numeric(freqdis), as.numeric(freqdis_se), method = "fixed", weights = ngroups*nindices))$se
fin_res$extdis_se[12]  <- with(fin_res[1:11, ], meta.summaries(as.numeric(extdis),  as.numeric(extdis_se), method = "fixed", weights = ngroups*nindices))$se
fin_res$freqgrp_se[12] <- with(fin_res[1:11, ], meta.summaries(as.numeric(freqgrp), as.numeric(freqgrp_se), method = "fixed", weights = ngroups*nindices))$se
fin_res$extgrp_se[12]  <- with(fin_res[1:11, ], meta.summaries(as.numeric(extgrp),  as.numeric(extgrp_se), method = "fixed", weights = ngroups*nindices))$se

# p-value of weighted mean 
fin_res[14, ] <- NA 
fin_res$freqdis_se[14] <- with(fin_res[1:11, ], meta.summaries(as.numeric(freqdis) -.5, as.numeric(freqdis_se), method = "fixed", weights=ngroups*nindices))$test[2]
fin_res$extdis_se[14]  <- with(fin_res[1:11, ], meta.summaries(as.numeric(extdis),  as.numeric(extdis_se), method = "fixed", weights=ngroups*nindices))$test[2]
fin_res$freqgrp_se[14] <- with(fin_res[1:11, ], meta.summaries(as.numeric(freqgrp) -.5, as.numeric(freqgrp_se), method = "fixed", weights=ngroups*nindices))$test[2]
fin_res$extgrp_se[14]  <- with(fin_res[1:11, ], meta.summaries(as.numeric(extgrp),  as.numeric(extgrp_se), method = "fixed", weights=ngroups*nindices))$test[2]

# Add precision weighted mean 
fin_res[(nrow(fin_res) + 1), ] <- c("", "Precision Wtd Mean", "", "", 
                                                weighted.mean(fin_res$freqdis[1:11], w = 1/fin_res$freqdis_se[1:11]^2), 
                                                weighted.mean(fin_res$extdis[1:11],  w = 1/fin_res$extdis_se[1:11]^2),
                                                weighted.mean(fin_res$freqgrp[1:11], w = 1/fin_res$freqgrp_se[1:11]^2),
                                                weighted.mean(fin_res$extgrp[1:11],  w = 1/fin_res$extgrp_se[1:11]^2),
                                                sqrt(1/sum(1/fin_res$freqdis_se[1:11]^2)),
                                                sqrt(1/sum(1/fin_res$extdis_se[1:11]^2)),
                                                sqrt(1/sum(1/fin_res$freqgrp_se[1:11]^2)),
                                                sqrt(1/sum(1/fin_res$extgrp_se[1:11]^2)))

write.csv(fin_res, file = "tabs/04_table_4c_toward_highinc_se.csv", row.names = F)

# Toward Low Income
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Read in results
dom_ed <- read.csv("tabs/03_dom_lowinc_by_group_issue.csv")

# Get Issue 
dom_ed$issue <- gsub("^[0-9]*", "", dom_ed$unique_id)

res <- data.frame(poll_id = NA, freqdis_se = 1:11, extdis_se = NA, freqgrp_se = NA, extgrp_se = NA)

j <- 1
for (i in unique(dom_ed$poll_id)) {

    res$poll_id[j] <- i
    res$freqdis_se[j] <- with(dom_ed[dom_ed$poll_id == i, ], summary(lmer(freqdis_grp ~  (1 | issue)))$coef[1, 2])
    res$extdis_se[j]  <- with(dom_ed[dom_ed$poll_id == i, ], summary(lmer(extdis_grp ~  (1 | issue)))$coef[1, 2])
    res$freqgrp_se[j] <- with(dom_ed[dom_ed$poll_id == i, ], summary(lmer(freqgrp_grp ~  (1 | issue)))$coef[1, 2])
    res$extgrp_se[j]  <- with(dom_ed[dom_ed$poll_id == i, ], summary(lmer(extgrp_grp ~  (1 | issue)))$coef[1, 2])

    j <- j + 1
}

# Load dat
bas_dat <- read.csv("tabs/04_table_4c_toward_lowinc.csv")
bas_dat$pollnum[c(12, 13)] <- NA
fin_res <- merge(bas_dat, res, by.x = "pollnum", by.y = "poll_id", all.x = T, all.y = F)

# Mean 
fin_res$freqdis_se[13] <- with(fin_res[1:11, ], meta.summaries(as.numeric(freqdis), as.numeric(freqdis_se), method = "fixed"))$se
fin_res$extdis_se[13]  <- with(fin_res[1:11, ], meta.summaries(as.numeric(extdis),  as.numeric(extdis_se),  method = "fixed"))$se
fin_res$freqgrp_se[13] <- with(fin_res[1:11, ], meta.summaries(as.numeric(freqgrp), as.numeric(freqgrp_se), method = "fixed"))$se
fin_res$extgrp_se[13]  <- with(fin_res[1:11, ], meta.summaries(as.numeric(extgrp),  as.numeric(extgrp_se),  method = "fixed"))$se

# Weighted Mean 
fin_res$freqdis_se[12] <- with(fin_res[1:11, ], meta.summaries(as.numeric(freqdis), as.numeric(freqdis_se), method = "fixed", weights = ngroups*nindices))$se
fin_res$extdis_se[12]  <- with(fin_res[1:11, ], meta.summaries(as.numeric(extdis),  as.numeric(extdis_se), method = "fixed", weights = ngroups*nindices))$se
fin_res$freqgrp_se[12] <- with(fin_res[1:11, ], meta.summaries(as.numeric(freqgrp), as.numeric(freqgrp_se), method = "fixed", weights = ngroups*nindices))$se
fin_res$extgrp_se[12]  <- with(fin_res[1:11, ], meta.summaries(as.numeric(extgrp),  as.numeric(extgrp_se), method = "fixed", weights = ngroups*nindices))$se

# p-value of weighted mean 
fin_res[14, ] <- NA 
fin_res$freqdis_se[14] <- with(fin_res[1:11, ], meta.summaries(as.numeric(freqdis) -.5, as.numeric(freqdis_se), method = "fixed", weights = ngroups*nindices))$test[2]
fin_res$extdis_se[14]  <- with(fin_res[1:11, ], meta.summaries(as.numeric(extdis),  as.numeric(extdis_se),  method = "fixed", weights = ngroups*nindices))$test[2]
fin_res$freqgrp_se[14] <- with(fin_res[1:11, ], meta.summaries(as.numeric(freqgrp) -.5, as.numeric(freqgrp_se), method = "fixed", weights = ngroups*nindices))$test[2]
fin_res$extgrp_se[14]  <- with(fin_res[1:11, ], meta.summaries(as.numeric(extgrp),  as.numeric(extgrp_se),  method = "fixed", weights = ngroups*nindices))$test[2]

# Add precision weighted mean 
fin_res[(nrow(fin_res)+1), ] <- c("", "Precision Wtd Mean", "", "", 
                                                weighted.mean(fin_res$freqdis[1:11], w = 1/fin_res$freqdis_se[1:11]^2), 
                                                weighted.mean(fin_res$extdis[1:11],  w = 1/fin_res$extdis_se[1:11]^2),
                                                weighted.mean(fin_res$freqgrp[1:11], w = 1/fin_res$freqgrp_se[1:11]^2),
                                                weighted.mean(fin_res$extgrp[1:11],  w = 1/fin_res$extgrp_se[1:11]^2),
                                                sqrt(1/sum(1/fin_res$freqdis_se[1:11]^2)),
                                                sqrt(1/sum(1/fin_res$extdis_se[1:11]^2)),
                                                sqrt(1/sum(1/fin_res$freqgrp_se[1:11]^2)),
                                                sqrt(1/sum(1/fin_res$extgrp_se[1:11]^2)))

write.csv(fin_res, file = "tabs/04_table_4c_toward_lowinc_se.csv", row.names = F)

# Load the triple threat
# -------------------------

# Read in results
dom_triple <- read.csv("tabs/03_dom_triple_by_group_issue.csv")

# Get Issue 
dom_triple$issue <- gsub("^[0-9]*", "", dom_triple$unique_id)

res <- data.frame(poll_id = NA, freqdis_se = 1:11, extdis_se =NA, freqgrp_se = NA, extgrp_se = NA)

j <- 1
for (i in unique(dom_triple$poll_id)) {

    res$poll_id[j] <- i
    res$freqdis_se[j] <- with(dom_triple[dom_triple$poll_id == i, ], summary(lmer(freqdis_grp ~  (1 | issue)))$coef[1, 2])
    res$extdis_se[j]  <- with(dom_triple[dom_triple$poll_id == i, ], summary(lmer(extdis_grp ~   (1 | issue)))$coef[1, 2])
    res$freqgrp_se[j] <- with(dom_triple[dom_triple$poll_id == i, ], summary(lmer(freqgrp_grp ~  (1 | issue)))$coef[1, 2])
    res$extgrp_se[j]  <- with(dom_triple[dom_triple$poll_id == i, ], summary(lmer(extgrp_grp ~   (1 | issue)))$coef[1, 2])

    j <- j + 1
}

# Load dat
bas_dat <- read.csv("tabs/04_table_4d_toward_triple.csv")
bas_dat$pollnum[c(13, 14)] <- NA
bas_dat <- bas_dat[-12, ] 

fin_res <- merge(bas_dat, res, by.x = "pollnum", by.y = "poll_id", all.x = T, all.y = F)

# Mean 
fin_res$freqdis_se[13] <- with(fin_res[1:11, ], meta.summaries(as.numeric(freqdis), as.numeric(freqdis_se), method = "fixed"))$se
fin_res$extdis_se[13]  <- with(fin_res[1:11, ], meta.summaries(as.numeric(extdis),  as.numeric(extdis_se),  method = "fixed"))$se
fin_res$freqgrp_se[13] <- with(fin_res[1:11, ], meta.summaries(as.numeric(freqgrp), as.numeric(freqgrp_se), method = "fixed"))$se
fin_res$extgrp_se[13]  <- with(fin_res[1:11, ], meta.summaries(as.numeric(extgrp),  as.numeric(extgrp_se),  method = "fixed"))$se

# Weighted Mean 
fin_res$freqdis_se[12] <- with(fin_res[1:11, ], meta.summaries(as.numeric(freqdis), as.numeric(freqdis_se), method = "fixed", weights = ngroups*nindices))$se
fin_res$extdis_se[12]  <- with(fin_res[1:11, ], meta.summaries(as.numeric(extdis),  as.numeric(extdis_se), method = "fixed", weights = ngroups*nindices))$se
fin_res$freqgrp_se[12] <- with(fin_res[1:11, ], meta.summaries(as.numeric(freqgrp), as.numeric(freqgrp_se), method = "fixed", weights = ngroups*nindices))$se
fin_res$extgrp_se[12]  <- with(fin_res[1:11, ], meta.summaries(as.numeric(extgrp),  as.numeric(extgrp_se), method = "fixed", weights = ngroups*nindices))$se

# p-value of weighted mean 
fin_res[14, ] <- NA 
fin_res$freqdis_se[14] <- with(fin_res[1:11, ], meta.summaries(as.numeric(freqdis) -.5, as.numeric(freqdis_se), method = "fixed", weights = ngroups*nindices))$test[2]
fin_res$extdis_se[14]  <- with(fin_res[1:11, ], meta.summaries(as.numeric(extdis),  as.numeric(extdis_se),  method = "fixed", weights = ngroups*nindices))$test[2]
fin_res$freqgrp_se[14] <- with(fin_res[1:11, ], meta.summaries(as.numeric(freqgrp) -.5, as.numeric(freqgrp_se), method = "fixed", weights = ngroups*nindices))$test[2]
fin_res$extgrp_se[14]  <- with(fin_res[1:11, ], meta.summaries(as.numeric(extgrp),  as.numeric(extgrp_se),  method = "fixed", weights = ngroups*nindices))$test[2]

# Add precision weighted mean 
fin_res[(nrow(fin_res) + 1), ] <- c("", "Precision Wtd Mean", "", "", 
                                                weighted.mean(fin_res$freqdis[1:11], w = 1/fin_res$freqdis_se[1:11]^2), 
                                                weighted.mean(fin_res$extdis[1:11],  w = 1/fin_res$extdis_se[1:11]^2),
                                                weighted.mean(fin_res$freqgrp[1:11], w = 1/fin_res$freqgrp_se[1:11]^2),
                                                weighted.mean(fin_res$extgrp[1:11],  w = 1/fin_res$extgrp_se[1:11]^2),
                                                sqrt(1/sum(1/fin_res$freqdis_se[1:11]^2)),
                                                sqrt(1/sum(1/fin_res$extdis_se[1:11]^2)),
                                                sqrt(1/sum(1/fin_res$freqgrp_se[1:11]^2)),
                                                sqrt(1/sum(1/fin_res$extgrp_se[1:11]^2)))

write.csv(fin_res, file = "tabs/04_table_4d_toward_triple_se.csv", row.names = F)


# Triple Disadvtange
# -------------------------

# Read in results
dom_triple <- read.csv("tabs/03_dom_triple_disadv_by_group_issue.csv")

# Get Issue 
dom_triple$issue <- gsub("^[0-9]*", "", dom_triple$unique_id)

res <- data.frame(poll_id = NA, freqdis_se = 1:11, extdis_se = NA, freqgrp_se = NA, extgrp_se = NA)

j <- 1
for (i in unique(dom_triple$poll_id)) {

    res$poll_id[j] <- i
    res$freqdis_se[j] <- with(dom_triple[dom_triple$poll_id == i, ], summary(lmer(freqdis_grp ~  (1 | issue)))$coef[1, 2])
    res$extdis_se[j]  <- with(dom_triple[dom_triple$poll_id == i, ], summary(lmer(extdis_grp ~   (1 | issue)))$coef[1, 2])
    res$freqgrp_se[j] <- with(dom_triple[dom_triple$poll_id == i, ], summary(lmer(freqgrp_grp ~  (1 | issue)))$coef[1, 2])
    res$extgrp_se[j]  <- with(dom_triple[dom_triple$poll_id == i, ], summary(lmer(extgrp_grp ~   (1 | issue)))$coef[1, 2])

    j <- j + 1
}

# Load dat
bas_dat <- read.csv("tabs/04_table_4d_toward_triple_disadv.csv")
bas_dat$pollnum[c(13, 14)] <- NA
bas_dat <- bas_dat[-12, ] 

fin_res <- merge(bas_dat, res, by.x = "pollnum", by.y = "poll_id", all.x = T, all.y = F)

# Mean 
fin_res$freqdis_se[13] <- with(fin_res[1:11, ], meta.summaries(as.numeric(freqdis), as.numeric(freqdis_se), method = "fixed"))$se
fin_res$extdis_se[13]  <- with(fin_res[1:11, ], meta.summaries(as.numeric(extdis),  as.numeric(extdis_se),  method = "fixed"))$se
fin_res$freqgrp_se[13] <- with(fin_res[1:11, ], meta.summaries(as.numeric(freqgrp), as.numeric(freqgrp_se), method = "fixed"))$se
fin_res$extgrp_se[13]  <- with(fin_res[1:11, ], meta.summaries(as.numeric(extgrp),  as.numeric(extgrp_se),  method = "fixed"))$se

# Weighted Mean 
fin_res$freqdis_se[12] <- with(fin_res[1:11, ], meta.summaries(as.numeric(freqdis), as.numeric(freqdis_se), method = "fixed", weights = ngroups*nindices))$se
fin_res$extdis_se[12]  <- with(fin_res[1:11, ], meta.summaries(as.numeric(extdis),  as.numeric(extdis_se), method = "fixed", weights = ngroups*nindices))$se
fin_res$freqgrp_se[12] <- with(fin_res[1:11, ], meta.summaries(as.numeric(freqgrp), as.numeric(freqgrp_se), method = "fixed", weights = ngroups*nindices))$se
fin_res$extgrp_se[12]  <- with(fin_res[1:11, ], meta.summaries(as.numeric(extgrp),  as.numeric(extgrp_se), method = "fixed", weights = ngroups*nindices))$se

# p-value of weighted mean 
fin_res[14, ] <- NA 
fin_res$freqdis_se[14] <- with(fin_res[1:11, ], meta.summaries(as.numeric(freqdis) -.5, as.numeric(freqdis_se), method = "fixed", weights = ngroups*nindices))$test[2]
fin_res$extdis_se[14]  <- with(fin_res[1:11, ], meta.summaries(as.numeric(extdis),  as.numeric(extdis_se),  method = "fixed", weights = ngroups*nindices))$test[2]
fin_res$freqgrp_se[14] <- with(fin_res[1:11, ], meta.summaries(as.numeric(freqgrp) -.5, as.numeric(freqgrp_se), method = "fixed", weights = ngroups*nindices))$test[2]
fin_res$extgrp_se[14]  <- with(fin_res[1:11, ], meta.summaries(as.numeric(extgrp),  as.numeric(extgrp_se),  method = "fixed", weights = ngroups*nindices))$test[2]

# Add precision weighted mean 
fin_res[(nrow(fin_res) + 1), ] <- c("", "Precision Wtd Mean", "", "", 
                                                weighted.mean(fin_res$freqdis[1:11], w = 1/fin_res$freqdis_se[1:11]^2), 
                                                weighted.mean(fin_res$extdis[1:11],  w = 1/fin_res$extdis_se[1:11]^2),
                                                weighted.mean(fin_res$freqgrp[1:11], w = 1/fin_res$freqgrp_se[1:11]^2),
                                                weighted.mean(fin_res$extgrp[1:11],  w = 1/fin_res$extgrp_se[1:11]^2),
                                                sqrt(1/sum(1/fin_res$freqdis_se[1:11]^2)),
                                                sqrt(1/sum(1/fin_res$extdis_se[1:11]^2)),
                                                sqrt(1/sum(1/fin_res$freqgrp_se[1:11]^2)),
                                                sqrt(1/sum(1/fin_res$extgrp_se[1:11]^2)))

write.csv(fin_res, file = "tabs/04_table_4d_toward_triple_disadv_se.csv", row.names = F)
