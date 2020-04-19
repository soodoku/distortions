"											
Distortions
Table 2: Homogenization and Polarization
"

# Set basedir
setwd(githubdir)
setwd("distortions/")

# Set StringsAsFactors as FALSE 
options(stringsAsFactors = FALSE)

# Load data
dpdat       <- read.csv("data/polardata.csv")
att_indices <- read.csv("data/poll_indices.csv")

# Table 2
# Output: Poll_name, No. of small Groups, No. of Indices, Frequency (polarization, homogenization), Extent (polarization, homogenization)

# load lib
detach(package:dplyr)
library(plyr)

# No. of groups*No. of indices 
res <- data.frame(pollname = 1:21, pollnum = NA, ngroups = NA, nindices = NA, polarfreq = NA, homofreq = NA, polarex = NA, homoex = NA)

# For normed data
res_normed <- data.frame(pollname = 1:21, pollnum = NA, ngroups = NA, nindices = NA, polarfreq = NA, homofreq = NA, polarex = NA, homoex = NA)

# For correlation
getall <- read.table(text = "", col.names = c("unique_id", "poll_id", "polarfreq", "homofreq", "polarex", "homoex"))

k <- 1
for(i in unique(att_indices$poll_id)){

    t1vars  <- att_indices$t1var[att_indices$poll_id == i]
    t2vars  <- att_indices$t2_t3var[att_indices$poll_id == i]
    smdata  <- dpdat[dpdat$pollid == i, ]

    polarfreq <- homofreq  <- polarex <- homoex  <- polarex_normed <- homoex_normed <- unique_id <- pollid <- c()

    for(j in 1:length(t1vars)){

        perindex <- ddply(smdata, ~pollgroup, summarise,
                                                         t1mean = mean(get(t1vars[j]), na.rm = T),
                                                         t2mean = mean(get(t2vars[j]), na.rm = T),
                                                         t1sd   = sd(get(t1vars[j]), na.rm = T),
                                                         t2sd   = sd(get(t2vars[j]), na.rm = T))

        polarfreq <- append(polarfreq, ifelse(perindex$t1mean == .5, NA, ifelse(perindex$t1mean < .5, perindex$t2mean < perindex$t1mean, perindex$t2mean > perindex$t1mean)))
        homofreq  <- append(homofreq, perindex$t2sd < perindex$t1sd)
        polarex	  <- append(polarex, ifelse(perindex$t1mean == .5, NA, ifelse(perindex$t1mean < .5, -1*(perindex$t2mean - perindex$t1mean), perindex$t2mean - perindex$t1mean)))
        homoex    <- append(homoex, perindex$t1sd - perindex$t2sd)
        unique_id    <- append(unique_id, paste0(perindex$pollgroup, t1vars[j]))
        pollid   <- append(pollid,  rep(smdata$dpnum[1], length(perindex$pollgroup)))

        polarex_normed <- append(polarex_normed, ifelse(perindex$t2mean > perindex$t1mean, polarex/(1 - perindex$t1mean),  ifelse(perindex$t2mean < perindex$t1mean, polarex/perindex$t1mean, 0)))
        homoex_normed  <- append(polarex_normed, ifelse(perindex$t2sd > perindex$t1sd,     homoex/(.5 - perindex$t1sd),    ifelse(perindex$t2sd < perindex$t1sd, homoex/perindex$t1sd, 0)))
    }

    # All issue/group pairs
    getall <- rbind(getall, data.frame(unique_id = unique_id, poll_id = pollid, polarfreq = polarfreq, homofreq = homofreq, polarex = polarex, homoex = homoex))

    # For each poll, out data
    res[k, ]       <- c(unique(as.character(smdata$pollname)), att_indices$dpnum[att_indices$poll_id == i][1], length(unique(smdata$pollgroup)), mean(smdata$numindices), mean(polarfreq, na.rm = T), mean(homofreq, na.rm = T), mean(polarex, na.rm = T), mean(homoex, na.rm = T))

    # Normed/For each poll, out data
    res_normed[k, ] <- c(unique(as.character(smdata$pollname)), att_indices$dpnum[att_indices$poll_id == i][1], length(unique(smdata$pollgroup)), mean(smdata$numindices), mean(polarfreq, na.rm = T), mean(homofreq, na.rm = T), mean(polarex_normed, na.rm = T), mean(homoex_normed, na.rm = T))

    k <- k + 1
}

# Write Results
# ~~~~~~~~~~~~~~~~~~

# Corr. b/w polarization, and homogenization

# Table 2
# Reorder
res <- res[order(as.numeric(res$pollnum)), c("pollname", "pollnum", "ngroups", "nindices", "homofreq", "homoex", "polarfreq", "polarex")]

# Add the means 
res[(nrow(res) + 1), ] <- c("Mean", colMeans(sapply(res[, 2:8], as.numeric)))
res[(nrow(res) + 1), ] <- c("Weighted Mean (By Indices and Groups)", sapply(lapply(res[, 2:8], as.numeric), weighted.mean, w = as.numeric(res$ngroups)*as.numeric(res$nindices)))

# Regular
write.csv(res, file = "tabs/02_table_2_hom_pol.csv", row.names = F)

# Normed
# Reorder
res_normed <- res_normed[order(as.numeric(res_normed$pollnum)), c("pollname", "pollnum", "ngroups", "nindices", "homofreq", "homoex", "polarfreq", "polarex")]

write.csv(res_normed, file = "tabs/02_table_2_hom_pol_normed.csv", row.names = F)

# Correlation between homogenization, polarization
write.csv(cor(getall[, c("polarfreq", "homofreq", "polarex", "homoex")], use = "na.or.complete"), file = "tabs/05_corr_hom_pol.csv", row.names = F)

# To allow for correlating with D, also produce getall
write.csv(getall, file = "tabs/03_hom_pol_by_group_issue.csv", row.names = F)
