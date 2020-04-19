"
Deliberative Distortions
Table 3 and 4: Description of Unequal Influence: Triple Disadvantaged
"

# Source scripts 
# Use recode from 03a and 03c (03b doesn't need recode)

# Set basedir
setwd(githubdir)
setwd("distortions/")

# Set StringsAsFactors as FALSE 
options(stringsAsFactors = FALSE)

# Load data
dpdat       <- read.csv("data/polardata.csv")
att_indices <- read.csv("data/poll_indices.csv")

# Load library
if ("package:dplyr" %in% search()) detach(package:dplyr)
library(plyr)
library(goji)

# Subset polls with income
dpdat <- subset(dpdat, !is.na(hhincome))
# Triple Advantage
dpdat$triple <- dpdat$highinc & dpdat$bettered & !dpdat$female

# Towards Triple
# ~~~~~~~~~~~~~~~~~~~~~~~
# No. of groups*No. of indices 
res <- data.frame(pollname = 1:12, pollnum = NA, ngroups = NA, nindices = NA, freqdis = NA, extdis = NA, freqgrp = NA, extgrp = NA)
res_normed <- data.frame(pollname = 1:12, pollnum = NA, ngroups = NA, nindices = NA, extgrp = NA, extdis = NA, extgrp_normed = NA, extdis_normed = NA)

freqgrp_grp <- extgrp_grp <- freqdis_grp <- extdis_grp <- unique_id <- group_id <- poll_id <- poll_name <- c()

k <- 1
for(i in unique(dpdat$pollid)){
    t1vars   <- as.character(att_indices$t1var[att_indices$poll_id == i])
    t2vars   <- as.character(att_indices$t2_t3var[att_indices$poll_id == i])
    smdata   <- dpdat[dpdat$pollid == i & !is.na(dpdat$triple), ]

    freqdis <- extdis <- freqgrp <- extgrp <- uniqueid <- groupid <- pollid <- pollname <- grpex_normed <- disgrpex_normed <- c()
    
    for(j in 1:length(t1vars)){
        grpindex <- ddply(smdata, .(pollgroup), summarise, 
                                                            t1mean = mean(get(t1vars[j]), na.rm = T),
                                                            t2mean = mean(get(t2vars[j]), na.rm = T))

        subgrpindex <- ddply(smdata, .(pollgroup, triple), summarise, 
                                                            t1mean = nona(mean(get(t1vars[j]), na.rm = T)),
                                                            t2mean = nona(mean(get(t2vars[j]), na.rm = T)))

        # Remove groups in which there is no person from one of the subgroups. For e.g., no low inc. person
        grpindex    <- grpindex[match(subgrpindex$pollgroup[duplicated(subgrpindex$pollgroup)], grpindex$pollgroup), ]
        subgrpindex <- subgrpindex[(subgrpindex$pollgroup %in% subgrpindex$pollgroup[duplicated(subgrpindex$pollgroup)]), ]
        
        t1_grp     <- grpindex$t1mean - subgrpindex$t1mean[subgrpindex$triple == 0]
        t2_grp     <- grpindex$t2mean - subgrpindex$t1mean[subgrpindex$triple == 0]
        
        t1_disgrp  <- t2_disgrp <- NA
        t1_disgrp  <- subgrpindex$t1mean[subgrpindex$triple == 1] - subgrpindex$t1mean[subgrpindex$triple == 0]
        t2_disgrp  <- subgrpindex$t2mean[subgrpindex$triple == 1] - subgrpindex$t1mean[subgrpindex$triple == 0]

        signed_grp <- NA
        signed_grp[t1_grp < 0 & grpindex$t2mean > grpindex$t1mean] <- (t2_grp - t1_grp)[t1_grp < 0 & grpindex$t2mean > grpindex$t1mean]
        signed_grp[t1_grp < 0 & grpindex$t2mean < grpindex$t1mean] <- (t2_grp - t1_grp)[t1_grp < 0 & grpindex$t2mean < grpindex$t1mean]
        signed_grp[t1_grp > 0 & grpindex$t2mean > grpindex$t1mean] <- -(t2_grp - t1_grp)[t1_grp > 0 & grpindex$t2mean > grpindex$t1mean]
        signed_grp[t1_grp > 0 & grpindex$t2mean < grpindex$t1mean] <- -(t2_grp - t1_grp)[t1_grp > 0 & grpindex$t2mean < grpindex$t1mean]
        signed_grp[t1_disgrp == 0] <- 0
        
        signed_disgrp <- NA
        signed_disgrp[t1_disgrp < 0 & subgrpindex$t2mean[subgrpindex$triple == 1] > subgrpindex$t1mean[subgrpindex$triple == 0]] <- (t2_disgrp - t1_disgrp)[t1_disgrp < 0 & subgrpindex$t2mean[subgrpindex$triple == 1] > subgrpindex$t1mean[subgrpindex$triple == 0]]
        signed_disgrp[t1_disgrp < 0 & subgrpindex$t2mean[subgrpindex$triple == 1] < subgrpindex$t1mean[subgrpindex$triple == 0]] <- (t2_disgrp - t1_disgrp)[t1_disgrp < 0 & subgrpindex$t2mean[subgrpindex$triple == 1] < subgrpindex$t1mean[subgrpindex$triple == 0]]
        signed_disgrp[t1_disgrp > 0 & subgrpindex$t2mean[subgrpindex$triple == 1] > subgrpindex$t1mean[subgrpindex$triple == 0]] <- -(t2_disgrp - t1_disgrp)[t1_disgrp > 0 & subgrpindex$t2mean[subgrpindex$triple == 1] > subgrpindex$t1mean[subgrpindex$triple == 0]]
        signed_disgrp[t1_disgrp > 0 & subgrpindex$t2mean[subgrpindex$triple == 1] < subgrpindex$t1mean[subgrpindex$triple == 0]] <- -(t2_disgrp - t1_disgrp)[t1_disgrp > 0 & subgrpindex$t2mean[subgrpindex$triple == 1] < subgrpindex$t1mean[subgrpindex$triple == 0]]
        signed_disgrp[t1_disgrp == 0] <- 0

        freqgrp   <- append(freqgrp, signed_grp > 0)
        extgrp    <- append(extgrp,  signed_grp)

        freqdis   <- append(freqdis, signed_disgrp > 0)
        extdis    <- append(extdis,  signed_disgrp)

        grpex_normed     <- append(grpex_normed,      ifelse(grpindex$t2mean > grpindex$t1mean, signed_grp/(1 - grpindex$t1mean),  ifelse(grpindex$t2mean < grpindex$t1mean, signed_grp/grpindex$t1mean, 0)))
        disgrpex_normed  <- append(disgrpex_normed,   ifelse(subgrpindex$t2mean[subgrpindex$triple == 1] > subgrpindex$t1mean[subgrpindex$triple == 1], 
                                                            signed_disgrp/(1 - subgrpindex$t1mean[subgrpindex$triple == 1]),  
                                                            ifelse(subgrpindex$t2mean[subgrpindex$triple == 1] < subgrpindex$t1mean[subgrpindex$triple == 1], signed_disgrp/subgrpindex$t1mean[subgrpindex$triple == 1], 0)))

        uniqueid <- append(uniqueid, paste0(grpindex$pollgroup, t1vars[j]))
        pollid   <- append(pollid,   rep(att_indices$dpnum[att_indices$poll_id == i][1], length(grpindex$pollgroup)))
        pollname <- append(pollname, rep(att_indices$poll_name[att_indices$poll_id == i][1], length(grpindex$pollgroup)))
        groupid  <- append(groupid, grpindex$pollgroup)
    }

    # Preserve grp issue pairs
    freqgrp_grp  <- append(freqgrp_grp, freqgrp)
    extgrp_grp   <- append(extgrp_grp, extgrp)
    freqdis_grp  <- append(freqdis_grp, freqdis)
    extdis_grp   <- append(extdis_grp, extdis)
    unique_id    <- append(unique_id, uniqueid)
    poll_id      <- append(poll_id,   pollid)
    poll_name    <- append(poll_name,   pollname)
    group_id     <- append(group_id,  groupid)

    # For each poll, out data
    res[k, ] <- c(unique(as.character(smdata$pollname)), att_indices$dpnum[att_indices$poll_id == i][1], length(unique(smdata$pollgroup)), mean(smdata$numindices), mean(freqdis, na.rm = T), mean(extdis, na.rm = T), mean(freqgrp, na.rm = T), mean(extgrp, na.rm = T))
    
    # Normed/For each poll, out data
    res_normed[k, ] <- c(unique(as.character(smdata$pollname)), att_indices$dpnum[att_indices$poll_id == i][1], length(unique(smdata$pollgroup)), mean(smdata$numindices), mean(extgrp, na.rm = T), mean(extdis, na.rm = T), mean(grpex_normed, na.rm = T), mean(disgrpex_normed, na.rm = T))

    k <- k + 1
}

# Reorder
res <- res[order(as.numeric(res$pollnum)), ]
res_normed <- res_normed[order(as.numeric(res_normed$pollnum)), ]

# Add the means 
res[(nrow(res) + 1), ] <- c("Mean", colMeans(sapply(res[, 2:8], as.numeric), na.rm = T))
res[(nrow(res) + 1), ] <- c("Weighted Mean (By Indices and Groups)", sapply(lapply(res[, 2:8], as.numeric), weighted.mean, w = as.numeric(res$ngroups)*as.numeric(res$nindices), na.rm = T))

# Write Results
write.csv(res, file = "tabs/04_table_4d_toward_triple.csv", row.names = F)

# Normed
write.csv(res_normed, file = "tabs/04_table_4d_toward_triple_normed.csv", row.names = F)

# To allow for correlating with D, produce getall
getall <- data.frame(unique_id = unique_id, poll_id = poll_id, poll_name = poll_name, group_id = group_id, freqgrp_grp = freqgrp_grp, extgrp_grp = extgrp_grp, freqdis_grp = freqdis_grp, extdis_grp = extdis_grp)
write.csv(getall, file = "tabs/03_dom_triple_by_group_issue.csv", row.names = F)


# Towards Triple Disadvantaged
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# No. of groups*No. of indices 
res <- data.frame(pollname = 1:12, pollnum = NA, ngroups = NA, nindices = NA, freqdis=NA, extdis = NA, freqgrp=NA, extgrp = NA)
res_normed <- data.frame(pollname = 1:12, pollnum = NA, ngroups = NA, nindices = NA, extgrp = NA, extdis = NA, extgrp_normed = NA, extdis_normed = NA)

freqgrp_grp <- extgrp_grp <- freqdis_grp <- extdis_grp <- unique_id <- group_id <- poll_id <- poll_name <- c()

k <- 1
for(i in unique(dpdat$pollid)){
     t1vars   <- as.character(att_indices$t1var[att_indices$poll_id == i])
     t2vars   <- as.character(att_indices$t2_t3var[att_indices$poll_id == i])
     smdata   <- dpdat[dpdat$pollid == i & !is.na(dpdat$triple), ]

     freqdis <- extdis <- freqgrp <- extgrp <- uniqueid <- groupid <- pollid <- grpex_normed <- disgrpex_normed <- c()

     for(j in 1:length(t1vars)){
        grpindex <- ddply(smdata, .(pollgroup), summarise, 
                                                             t1mean = nona(mean(get(t1vars[j]), na.rm = T)),
                                                             t2mean = nona(mean(get(t2vars[j]), na.rm = T)))

        subgrpindex <- ddply(smdata, .(pollgroup, triple), summarise, 
                                                             t1mean = nona(mean(get(t1vars[j]), na.rm = T)),
                                                             t2mean = nona(mean(get(t2vars[j]), na.rm = T)))


        grpindex    <- grpindex[match(subgrpindex$pollgroup[duplicated(subgrpindex$pollgroup)], grpindex$pollgroup), ]
        subgrpindex <- subgrpindex[(subgrpindex$pollgroup %in% subgrpindex$pollgroup[duplicated(subgrpindex$pollgroup)]), ]

        t1_grp     <- grpindex$t1mean - subgrpindex$t1mean[subgrpindex$triple == 1]
        t2_grp     <- grpindex$t2mean - subgrpindex$t1mean[subgrpindex$triple == 1]

        signed_grp <- NA
        signed_grp[t1_grp < 0 & grpindex$t2mean > grpindex$t1mean] <- (t2_grp - t1_grp)[t1_grp < 0 & grpindex$t2mean > grpindex$t1mean]
        signed_grp[t1_grp < 0 & grpindex$t2mean < grpindex$t1mean] <- (t2_grp - t1_grp)[t1_grp < 0 & grpindex$t2mean < grpindex$t1mean]
        signed_grp[t1_grp > 0 & grpindex$t2mean > grpindex$t1mean] <- -(t2_grp - t1_grp)[t1_grp > 0 & grpindex$t2mean > grpindex$t1mean]
        signed_grp[t1_grp > 0 & grpindex$t2mean < grpindex$t1mean] <- -(t2_grp - t1_grp)[t1_grp > 0 & grpindex$t2mean < grpindex$t1mean]
        signed_grp[t1_grp == 0] <- 0

        t1_disgrp  <- subgrpindex$t1mean[subgrpindex$triple == 0] - subgrpindex$t1mean[subgrpindex$triple == 1]
        t2_disgrp  <- subgrpindex$t2mean[subgrpindex$triple == 0] - subgrpindex$t1mean[subgrpindex$triple == 1]

        signed_disgrp <- NA
        signed_disgrp[t1_disgrp < 0 & subgrpindex$t2mean[subgrpindex$triple == 0] > subgrpindex$t1mean[subgrpindex$triple == 1]] <- (t2_disgrp - t1_disgrp)[t1_disgrp < 0 & subgrpindex$t2mean[subgrpindex$triple == 0] > subgrpindex$t1mean[subgrpindex$triple == 1]]
        signed_disgrp[t1_disgrp < 0 & subgrpindex$t2mean[subgrpindex$triple == 0] < subgrpindex$t1mean[subgrpindex$triple == 1]] <- (t2_disgrp - t1_disgrp)[t1_disgrp < 0 & subgrpindex$t2mean[subgrpindex$triple == 0] < subgrpindex$t1mean[subgrpindex$triple == 1]]
        signed_disgrp[t1_disgrp > 0 & subgrpindex$t2mean[subgrpindex$triple == 0] > subgrpindex$t1mean[subgrpindex$triple == 1]] <- -(t2_disgrp - t1_disgrp)[t1_disgrp > 0 & subgrpindex$t2mean[subgrpindex$triple == 0] > subgrpindex$t1mean[subgrpindex$triple == 1]]
        signed_disgrp[t1_disgrp > 0 & subgrpindex$t2mean[subgrpindex$triple == 0] < subgrpindex$t1mean[subgrpindex$triple == 1]] <- -(t2_disgrp - t1_disgrp)[t1_disgrp > 0 & subgrpindex$t2mean[subgrpindex$triple == 0] < subgrpindex$t1mean[subgrpindex$triple == 1]]
        signed_disgrp[t1_disgrp == 0] <- 0

        freqgrp   = append(freqgrp, signed_grp > 0)
        extgrp    = append(extgrp,  signed_grp)

        freqdis   = append(freqdis, signed_disgrp > 0)
        extdis    = append(extdis,  signed_disgrp)

        grpex_normed     <- append(grpex_normed,      ifelse(grpindex$t2mean > grpindex$t1mean, signed_grp/(1 - grpindex$t1mean),  ifelse(grpindex$t2mean < grpindex$t1mean, signed_grp/grpindex$t1mean, 0)))
        disgrpex_normed  <- append(disgrpex_normed,   ifelse(subgrpindex$t2mean[subgrpindex$triple == 0] > subgrpindex$t1mean[subgrpindex$triple == 0], 
                                                            signed_disgrp/(1 - subgrpindex$t1mean[subgrpindex$triple == 0]),  
                                                            ifelse(subgrpindex$t2mean[subgrpindex$triple == 0] < subgrpindex$t1mean[subgrpindex$triple == 0], signed_disgrp/subgrpindex$t1mean[subgrpindex$triple == 0], 0)))

        uniqueid <- append(uniqueid, paste0(grpindex$pollgroup, t1vars[j]))
        pollid   <- append(pollid,  rep(att_indices$dpnum[att_indices$poll_id == i][1], length(grpindex$pollgroup)))
        groupid  <- append(groupid, grpindex$pollgroup)

    }

    # Preserve grp issue pairs
    freqgrp_grp  <- append(freqgrp_grp, freqgrp)
    extgrp_grp   <- append(extgrp_grp, extgrp)
    freqdis_grp  <- append(freqdis_grp, freqdis)
    extdis_grp   <- append(extdis_grp, extdis)
    unique_id    <- append(unique_id, uniqueid) 
    poll_id      <- append(poll_id,   pollid)
    group_id     <- append(group_id,  groupid)

    # For each poll, out data
    res[k, ] <- c(unique(as.character(smdata$pollname)), att_indices$dpnum[att_indices$poll_id == i][1], length(unique(smdata$pollgroup)), mean(smdata$numindices), mean(freqdis, na.rm = T), mean(extdis, na.rm = T), mean(freqgrp, na.rm = T), mean(extgrp, na.rm = T))
        
    # Normed/For each poll, out data
    res_normed[k, ] <- c(unique(as.character(smdata$pollname)), att_indices$dpnum[att_indices$poll_id == i][1], length(unique(smdata$pollgroup)), mean(smdata$numindices), mean(extgrp, na.rm = T), mean(extdis, na.rm = T), mean(grpex_normed, na.rm = T), mean(disgrpex_normed, na.rm = T))
    
    k <- k + 1
}

# Reorder
res <- res[order(as.numeric(res$pollnum)), ]
res_normed <- res_normed[order(as.numeric(res_normed$pollnum)), ]

# Add the means 
res[(nrow(res) + 1), ] <- c("Mean", colMeans(sapply(res[, 2:8], as.numeric), na.rm = T))
res[(nrow(res) + 1), ] <- c("Weighted Mean (By Indices and Groups)", sapply(lapply(res[, 2:8], as.numeric), weighted.mean, w = as.numeric(res$ngroups)*as.numeric(res$nindices), na.rm = T))

# Write Results
write.csv(res, file = "tabs/04_table_4d_toward_triple_disadv.csv", row.names = F)

# Normed
write.csv(res_normed, file = "tabs/04_table_4d_toward_triple_disadv_normed.csv", row.names = F)

# To allow for correlating with D, produce getall
getall <- data.frame(unique_id = unique_id, poll_id = poll_id, group_id = group_id, freqgrp_grp = freqgrp_grp, extgrp_grp = extgrp_grp, freqdis_grp = freqdis_grp, extdis_grp = extdis_grp)
write.csv(getall, file = "tabs/03_dom_triple_disadv_by_group_issue.csv", row.names = F)
