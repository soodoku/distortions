#
#	Deliberative Distortions  	
#	Summary of DP Data By Poll
#

# Set dir
setwd(githubdir)
setwd("distortions/")

# Load data
dpdat       <- read.csv("data/polardata.csv")
att_indices <- read.csv("data/poll_indices.csv")

# Fix dpdat Data (Using att_indices so no issues for n_indices)
# ------------------------------------------------------------
# N Indices for New Haven (2 rather than 3)
# N Indices for Bulgaria  (12 rather than 5)

# Load libraries
library(dplyr)

# Output: Row = Study and Cols = Study, n_participants, Country, Mode, Indices, Groups, Mean Group Size

res <- dpdat %>%
  group_by(pollid) %>%
  summarise(pollname        = unique(pollname),
            dpnum           = unique(dpnum),
            n_participants  = length(pollid),
            country         = unique(country),
            mode            = mean(mode),
            groups          = length(unique(pollgroup)),
            mean_grp_size   = mean(groupsize, na.rm = T)) %>%
  arrange(dpnum) %>%
  left_join(att_indices[!duplicated(att_indices$dpnum), ], by = "dpnum")

# Write Results
write.csv(res, file = "tabs/01_table_1_dp_summary.csv", row.names = F)
