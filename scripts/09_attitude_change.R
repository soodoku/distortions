##--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~---++
##   											
##	Polarization  	
##	Appendix C/Before and After Policy
##	By Gaurav Sood						
## 	Last Edited: 8/21/15 by GS
##--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~---++

# Set basedir
setwd(githubdir)
setwd("distortions/")

# Load data
dpdat       <- read.csv("data/polardata.csv")
att_indices <- read.csv("data/poll_indices.csv")

# Table 1
# Study, Participants, Country, Mode, Indices, Groups, Mean Group Size

# Output
 # Row  = Policy index
 # Cols = Study, Before.Before, SD.Before, After.Mean, After.SD

# Analyses
t1mean <- sapply(dpdat[, as.character(att_indices$t1var)], mean, na.rm = T)
t1sd   <- sapply(dpdat[, as.character(att_indices$t1var)], sd, na.rm = T)
t2mean <- sapply(dpdat[, as.character(att_indices$t2_t3var)], mean, na.rm = T)
t2sd   <- sapply(dpdat[, as.character(att_indices$t2_t3var)], sd, na.rm = T)
	
# Results
res <- cbind(att_indices, t1mean, t1sd, t2mean, t2sd)

# Reorder 
res <- res[order(res$dpnum), ]

# Write Results
write.csv(res, file = "tabs/att_change.csv", row.names = F)
