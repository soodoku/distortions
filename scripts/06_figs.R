"
Deliberative Distortions
Figures:
      1. Boxplot, Density Plot of Polarization, Homogenization by Poll
      2. By Anglo/Others
      3. By Online/f2f
"


# Set basedir
setwd(githubdir)
setwd("distortions/")

# Polarization by grp-issue pair 
hp   <- read.csv("tabs/03_hom_pol_by_group_issue.csv")

# Domination by grp-issue pair 
dom_fem <-  read.csv("tabs/03_dom_fem_by_group_issue.csv")
dom_inc <-  read.csv("tabs/03_dom_highinc_by_group_issue.csv")
dom_ed  <-  read.csv("tabs/03_dom_ed_by_group_issue.csv")
dom_t3  <-  read.csv("tabs/03_dom_triple_disadv_by_group_issue.csv")

# Create an Anglo dummy
anglo <- c("UK EU", "UK Health", "UK Monarchy", "UK General Election", "UK Crime", "Central Power & Light", "San Mateo, CA", "West Texas Utilities", "National Issues Convention", 
          "By the People: National", "Southwestern Electric Power", "By the People: Health and Education", "By the People 2004 US General Election", "By the People 2004 US Presidential Primaries",
          "New Haven, CT", "National Issues Convention 2")

# Create Online/F2F vector
# dp_all <- read.csv("cdd/data/agg/polardata.csv")
# table(dp_all$pollname) --- compare to Table 1

online  <- c("By the People: National",
             "By the People 2004 US General Election",
             "By the People 2004 US Presidential Primaries",
             "By the People: Health and Education",
             "National Issues Convention")

# Load libs
library(ggplot2)
library(grid)
library(goji)
if ("package:plyr" %in% search()) detach(package:plyr)
library(dplyr)

# Polarization
# -------------

hp <- hp %>% left_join(dom_fem[, c("unique_id", "poll_name")])

# Take out missing
polar2  <- subset(hp, !is.na(polarex))

ggplot(polar2, aes(polarex)) +
geom_density(aes(x = polarex, y = ..count../sum(..count..))) +
theme_bw() +
xlab(expression(P["gj"])) +
ylab("Rel. Freq.") +
theme(axis.text = element_text(size = 7),
      axis.ticks = element_blank(),
      panel.grid.major.x = element_line(color = "#eeeeee"),
      panel.grid.major.y = element_line(color = "#dddddd", linetype = "dotted"),
      panel.grid.minor = element_blank(),
      plot.margin = unit(c(.5, .5, .5, .5), "cm"),
      panel.border = element_rect(0, 0, 0, 0))
ggsave("figs/density_polarization.png", width = 3, height = 3)

ggplot(polar2, aes(poll_name, polarex)) +
geom_boxplot() +
theme_bw() +
coord_flip() +
scale_y_continuous(breaks = round(seq(min(polar2$polarex), max(polar2$polarex), by = .1), 2)) + 
theme(axis.title.x = element_blank(),
      axis.text = element_text(size = 7),
      axis.title.y = element_blank(),
      axis.ticks = element_blank(),
      panel.grid.major.x = element_line(color = "#eeeeee"), 
      panel.grid.major.y = element_line(color = "#dddddd", linetype = "dotted"),
      panel.grid.minor = element_blank(),
      plot.margin = unit(c(.5, .5, .5, .5), "cm"),
      panel.border = element_rect(0, 0, 0, 0))
ggsave("figs/boxplot_polarization.png", width = 5, height = 5)

# Anglo vs. rest.

polar2$anglo <- ifelse(polar2$poll_name %in% anglo, "Anglo", "Other")

ggplot(polar2) +  
geom_density(aes(fill = anglo, color = anglo, group = anglo, polarex, y = ..scaled..), alpha = .1) + 
theme_bw() +
scale_fill_grey() +
scale_color_grey() +
xlab(expression(P["gj"])) +
ylab("Rel. Freq.") + 
theme(axis.text = element_text(size = 7), 
      axis.ticks = element_blank(),
      panel.grid.major.x = element_line(color = "#eeeeee"), 
      panel.grid.major.y = element_line(color = "#dddddd", linetype = "dotted"),
      panel.grid.minor = element_blank(),
      plot.margin = unit(c(.5, .5, .5, .5), "cm"),
      panel.border = element_rect(0, 0, 0, 0),
      legend.title = element_blank())
ggsave("figs/density_anglo_polarization.png", width = 5, height = 5)

# Online Vs. F2f

polar2$online <- ifelse(polar2$poll_name %in% online, "Online", "F2F")

ggplot(polar2) +  
geom_density(aes(fill = online, color = online, group = online, polarex, y = ..scaled..), alpha = .1) + 
theme_bw() +
scale_fill_grey() +
scale_color_grey() +
xlab(expression(P["gj"])) +
ylab("Rel. Freq.") + 
theme(axis.text = element_text(size = 7), 
      axis.ticks = element_blank(),
      panel.grid.major.x = element_line(color = "#eeeeee"), 
      panel.grid.major.y = element_line(color = "#dddddd", linetype = "dotted"),
      panel.grid.minor = element_blank(),
      plot.margin = unit(c(.5, .5, .5, .5), "cm"),
      panel.border = element_rect(0, 0, 0, 0),
      legend.title = element_blank())
ggsave("figs/density_online_f2f_polarization.png", width = 5, height = 5)

# Homogenization 
# ----------------

# Take out missing
homo2  <- subset(hp, !is.na(homoex))

ggplot(homo2, aes(homoex)) + 
geom_density(aes(x = homoex, y = ..count../sum(..count..))) +
theme_bw() +
xlab(expression(H["gj"])) +
ylab("Rel. Freq.") +
theme(axis.text = element_text(size = 7),
      axis.ticks = element_blank(),
      panel.grid.major.x = element_line(color = "#eeeeee"), 
      panel.grid.major.y = element_line(color = "#dddddd", linetype = "dotted"),
      panel.grid.minor = element_blank(),
      plot.margin = unit(c(.5, .5, .5, .5), "cm"),
      panel.border = element_rect(0, 0, 0, 0))
ggsave("figs/density_homogenization.png", width = 3, height = 3)

ggplot(homo2, aes(poll_name, homoex)) +
geom_boxplot() +
theme_bw() +
coord_flip() + 
scale_y_continuous(breaks = round(seq(min(homo2$homoex), max(homo2$homoex), by = .1), 2)) +
theme(axis.title.x = element_blank(),
      axis.text = element_text(size = 7),
      axis.title.y = element_blank(),
      axis.ticks = element_blank(),
      panel.grid.major.x = element_line(color = "#eeeeee"),
      panel.grid.major.y = element_line(color = "#dddddd", linetype = "dotted"),
      panel.grid.minor = element_blank(),
      plot.margin = unit(c(.5, .5, .5, .5), "cm"),
      panel.border = element_rect(0, 0, 0, 0))
ggsave("figs/boxplot_homogenization.png", width = 5, height = 5)

homo2$anglo <- ifelse(homo2$poll_name %in% anglo, "Anglo", "Other")

ggplot(homo2) +
geom_density(aes(fill = anglo, color = anglo, group = anglo, homoex, y = ..scaled..), alpha = .1) +
theme_bw() +
scale_fill_grey() +
scale_color_grey() +
xlab(expression(H["gj"])) +
ylab("Rel. Freq.") +
theme(axis.text = element_text(size = 7),
      axis.ticks = element_blank(),
      panel.grid.major.x = element_line(color = "#eeeeee"),
      panel.grid.major.y = element_line(color = "#dddddd", linetype = "dotted"),
      panel.grid.minor = element_blank(),
      plot.margin = unit(c(.5, .5, .5, .5), "cm"),
      panel.border = element_rect(0, 0, 0, 0),
      legend.title = element_blank())
ggsave("figs/density_anglo_homogenization.png", width = 5, height = 5)

# Online vs. F2f

homo2$online <- ifelse(homo2$poll_name %in% online, "Online", "F2F")

ggplot(homo2) +  
geom_density(aes(fill = online, color = online, group = online, homoex, y = ..scaled..), alpha = .1) + 
theme_bw() +
scale_fill_grey() +
scale_color_grey() +
xlab(expression(P["gj"])) +
ylab("Rel. Freq.") + 
theme(axis.text = element_text(size = 7), 
      axis.ticks = element_blank(),
      panel.grid.major.x = element_line(color = "#eeeeee"), 
      panel.grid.major.y = element_line(color = "#dddddd", linetype = "dotted"),
      panel.grid.minor = element_blank(),
      plot.margin = unit(c(.5, .5, .5, .5), "cm"),
      panel.border = element_rect(0, 0, 0, 0),
      legend.title = element_blank())
ggsave("figs/density_online_f2f_homogenization.png", width = 5, height = 5)

# Domination --- Income 
# ----------------------

# Take out missing
dom_inc2  <- subset(dom_inc, !is.na(extgrp_grp))

ggplot(dom_inc2, aes(extgrp_grp)) +
geom_density(aes(x = extgrp_grp, y = ..count../sum(..count..))) +
theme_bw() +
xlab(expression(D["gj"])) +
ylab("Rel. Freq.") +
theme(axis.text = element_text(size = 7),
      axis.ticks = element_blank(),
      panel.grid.major.x = element_line(color = "#eeeeee"),
      panel.grid.major.y = element_line(color = "#dddddd", linetype = "dotted"),
      panel.grid.minor = element_blank(),
      plot.margin = unit(c(.5, .5, .5, .5), "cm"),
      panel.border = element_rect(0, 0, 0, 0))
ggsave("figs/density_dom_inc.png", width = 3.3, height = 3)

ggplot(dom_inc2, aes(poll_name, extgrp_grp)) +
geom_boxplot() +
theme_bw() +
coord_flip() +
scale_y_continuous(breaks = round(seq(min(dom_inc2$extgrp_grp), max(dom_inc2$extgrp_grp), by = .1), 2)) + 
theme(axis.title.x = element_blank(),
      axis.text = element_text(size = 7),
      axis.title.y = element_blank(),
      axis.ticks = element_blank(),
      panel.grid.major.x = element_line(color = "#eeeeee"),
      panel.grid.major.y = element_line(color = "#dddddd", linetype = "dotted"),
      panel.grid.minor = element_blank(),
      plot.margin = unit(c(.5, .5, .5, .5), "cm"),
      panel.border = element_rect(0, 0, 0, 0))
ggsave("figs/boxplot_dom_inc.png", width = 5, height = 5)

dom_inc2$anglo <- ifelse(dom_inc2$poll_name %in% anglo, "Anglo", "Other")

ggplot(dom_inc2, aes(anglo, extgrp_grp)) +
geom_boxplot() +
theme_bw() +
ylab(expression(D["gj"])) +
xlab("") +
coord_flip() +
scale_y_continuous(breaks = round(seq(min(dom_inc2$extgrp_grp), max(dom_inc2$extgrp_grp), by = .1), 2)) +
theme(axis.text = element_text(size = 7),
      axis.ticks = element_blank(),
      panel.grid.major.x = element_line(color = "#eeeeee"),
      panel.grid.major.y = element_line(color = "#dddddd", linetype = "dotted"),
      panel.grid.minor = element_blank(),
      plot.margin = unit(c(.5, .5, .5, .5), "cm"),
      panel.border = element_rect(0, 0, 0, 0))
ggsave("figs/boxplot_anglo_dom_inc.png", width = 5, height = 5)

ggplot(dom_inc2, aes(anglo, extgrp_grp)) +
geom_density(aes(fill = anglo, color = anglo, group = anglo, extgrp_grp, y = ..scaled..), alpha = .1) +
theme_bw() +
scale_fill_grey() +
scale_color_grey() +
xlab(expression(D["gj"])) +
ylab("Rel. Freq.") +
theme(axis.text = element_text(size = 7), 
      axis.ticks = element_blank(),
      panel.grid.major.x = element_line(color = "#eeeeee"),
      panel.grid.major.y = element_line(color = "#dddddd", linetype = "dotted"),
      panel.grid.minor = element_blank(),
      plot.margin = unit(c(.5, .5, .5, .5), "cm"),
      panel.border = element_rect(0, 0, 0, 0),
      legend.title = element_blank())
ggsave("figs/density_anglo_dom_inc.png", width = 5, height = 5)

# Online/F2f
dom_inc2$online <- ifelse(dom_inc2$poll_name %in% online, "Online", "F2F")

ggplot(dom_inc2, aes(online, extgrp_grp)) +
geom_density(aes(fill = online, color = online, group = online, extgrp_grp, y = ..scaled..), alpha = .1) +
theme_bw() +
scale_fill_grey() +
scale_color_grey() +
xlab(expression(D["gj"])) +
ylab("Rel. Freq.") +
theme(axis.text = element_text(size = 7), 
      axis.ticks = element_blank(),
      panel.grid.major.x = element_line(color = "#eeeeee"),
      panel.grid.major.y = element_line(color = "#dddddd", linetype = "dotted"),
      panel.grid.minor = element_blank(),
      plot.margin = unit(c(.5, .5, .5, .5), "cm"),
      panel.border = element_rect(0, 0, 0, 0),
      legend.title = element_blank())
ggsave("figs/density_online_f2f_dom_inc.png", width = 5, height = 5)


# Domination ---  Gender 
# ----------------------

# Take out missing
dom_fem2  <- subset(dom_fem, !is.na(extgrp_grp))

ggplot(dom_fem2, aes(extgrp_grp)) +
geom_density(aes(x = extgrp_grp, y = ..count../sum(..count..))) +
theme_bw() +
xlab(expression(D["gj"])) + 
ylab("Rel. Freq.") +
theme(axis.text = element_text(size = 7),
      axis.ticks = element_blank(),
      panel.grid.major.x = element_line(color = "#eeeeee"), 
      panel.grid.major.y = element_line(color = "#dddddd", linetype = "dotted"),
      panel.grid.minor = element_blank(),
      plot.margin = unit(c(.5, .5, .5, .5), "cm"),
      panel.border = element_rect(0, 0, 0, 0))
ggsave("figs/density_dom_fem.png", width = 3, height = 3)

ggplot(dom_fem2, aes(poll_name, extgrp_grp)) +
geom_boxplot() +
theme_bw() +
coord_flip() +
scale_y_continuous(breaks = round(seq(min(dom_fem2$extgrp_grp), max(dom_fem2$extgrp_grp), by = .1), 2)) +
theme(axis.title.x = element_blank(),
      axis.text = element_text(size = 7),
      axis.title.y = element_blank(),
      axis.ticks = element_blank(),
      panel.grid.major.x = element_line(color = "#eeeeee"),
      panel.grid.major.y = element_line(color = "#dddddd", linetype = "dotted"),
      panel.grid.minor = element_blank(),
      plot.margin = unit(c(.5, .5, .5, .5), "cm"),
      panel.border = element_rect(0, 0, 0, 0))
ggsave("figs/boxplot_dom_fem.png", width = 7, height = 7)

dom_fem2$anglo <- ifelse(dom_fem2$poll_name %in% anglo, "Anglo", "Other")

ggplot(dom_fem2, aes(anglo, extgrp_grp)) +
geom_boxplot() +
theme_bw() +
ylab(expression(D["gj"])) +
xlab("") +
coord_flip() +
scale_y_continuous(breaks = round(seq(min(dom_fem2$extgrp_grp), max(dom_fem2$extgrp_grp), by = .1), 2)) +
theme(axis.text = element_text(size = 7), 
      axis.ticks = element_blank(),
      panel.grid.major.x = element_line(color = "#eeeeee"),
      panel.grid.major.y = element_line(color = "#dddddd", linetype = "dotted"),
      panel.grid.minor = element_blank(),
      plot.margin = unit(c(.5, .5, .5, .5), "cm"),
      panel.border = element_rect(0, 0, 0, 0))
ggsave("figs/boxplot_anglo_dom_fem.png", width = 5, height = 5)

ggplot(dom_fem2, aes(anglo, extgrp_grp)) +
geom_density(aes(fill = anglo, color = anglo, group = anglo, extgrp_grp, y = ..scaled..), alpha = .1) +
theme_bw() +
scale_fill_grey() +
scale_color_grey() +
xlab(expression(D["gj"])) +
ylab("Rel. Freq.") +
theme(axis.text = element_text(size = 7),
      axis.ticks = element_blank(),
      panel.grid.major.x = element_line(color = "#eeeeee"),
      panel.grid.major.y = element_line(color = "#dddddd", linetype = "dotted"),
      panel.grid.minor = element_blank(),
      plot.margin = unit(c(.5, .5, .5, .5), "cm"),
      panel.border = element_rect(0, 0, 0, 0),
      legend.title = element_blank())
ggsave("figs/density_anglo_dom_fem.png", width = 5, height = 5)

# Online/F2f
dom_fem2$online <- ifelse(dom_fem2$poll_name %in% online, "Online", "F2F")

ggplot(dom_fem2, aes(online, extgrp_grp)) +
geom_density(aes(fill = online, color = online, group = online, extgrp_grp, y = ..scaled..), alpha = .1) +
theme_bw() +
scale_fill_grey() +
scale_color_grey() +
xlab(expression(D["gj"])) +
ylab("Rel. Freq.") +
theme(axis.text = element_text(size = 7), 
      axis.ticks = element_blank(),
      panel.grid.major.x = element_line(color = "#eeeeee"),
      panel.grid.major.y = element_line(color = "#dddddd", linetype = "dotted"),
      panel.grid.minor = element_blank(),
      plot.margin = unit(c(.5, .5, .5, .5), "cm"),
      panel.border = element_rect(0, 0, 0, 0),
      legend.title = element_blank())
ggsave("figs/density_online_f2f_dom_fem.png", width = 5, height = 5)

# Domination ---  Education 
# -------------------------

# Take out missing
dom_ed2  <- subset(dom_ed, !is.na(extgrp_grp))

ggplot(dom_ed2, aes(extgrp_grp)) +
geom_density(aes(x = extgrp_grp, y = ..count../sum(..count..))) +
theme_bw() +
xlab(expression(D["gj"])) +
ylab("Rel. Freq.") +
theme(axis.text = element_text(size = 7),
      axis.ticks = element_blank(),
      panel.grid.major.x = element_line(color = "#eeeeee"), 
      panel.grid.major.y = element_line(color = "#dddddd", linetype = "dotted"),
      panel.grid.minor = element_blank(),
      plot.margin = unit(c(.5, .5, .5, .5), "cm"),
      panel.border = element_rect(0, 0, 0, 0))
ggsave("figs/density_dom_ed.png", width = 3.4, height = 3)

ggplot(dom_ed2, aes(poll_name, extgrp_grp)) +
geom_boxplot() +
theme_bw() +
coord_flip() +
scale_y_continuous(breaks = round(seq(min(dom_ed2$extgrp_grp), max(dom_ed2$extgrp_grp), by = .1), 2)) +
theme(axis.title.x = element_blank(),
      axis.text = element_text(size = 7), 
      axis.title.y = element_blank(),
      axis.ticks = element_blank(),
      panel.grid.major.x = element_line(color = "#eeeeee"),
      panel.grid.major.y = element_line(color = "#dddddd", linetype = "dotted"),
      panel.grid.minor = element_blank(),
      plot.margin = unit(c(.5, .5, .5, .5), "cm"),
      panel.border = element_rect(0, 0, 0, 0))
ggsave("figs/boxplot_dom_ed.png", width = 7, height = 7)

dom_ed2$anglo <- ifelse(dom_ed2$poll_name %in% anglo, "Anglo", "Other")

ggplot(dom_ed2, aes(anglo, extgrp_grp)) +
geom_boxplot() +
theme_bw() +
ylab(expression(D["gj"])) +
xlab("") +
coord_flip() +
scale_y_continuous(breaks = round(seq(min(dom_ed2$extgrp_grp), max(dom_ed2$extgrp_grp), by = .1), 2)) +
theme(axis.text = element_text(size = 7), 
      axis.ticks = element_blank(),
      panel.grid.major.x = element_line(color = "#eeeeee"),
      panel.grid.major.y = element_line(color = "#dddddd", linetype = "dotted"),
      panel.grid.minor = element_blank(),
      plot.margin = unit(c(.5, .5, .5, .5), "cm"),
      panel.border = element_rect(0, 0, 0, 0))
ggsave("figs/boxplot_anglo_dom_ed.png", width = 5, height = 5)

ggplot(dom_ed2, aes(anglo, extgrp_grp)) +
geom_density(aes(fill = anglo, color = anglo, group = anglo, extgrp_grp, y = ..scaled..), alpha = .1) +
theme_bw() +
scale_fill_grey() +
scale_color_grey() +
xlab(expression(D["gj"])) +
ylab("Rel. Freq.") +
theme(axis.text = element_text(size = 7),
      axis.ticks = element_blank(),
      panel.grid.major.x = element_line(color = "#eeeeee"),
      panel.grid.major.y = element_line(color = "#dddddd", linetype = "dotted"),
      panel.grid.minor = element_blank(),
      plot.margin = unit(c(.5, .5, .5, .5), "cm"),
      panel.border = element_rect(0, 0, 0, 0),
      legend.title = element_blank())
ggsave("figs/density_anglo_dom_ed.png", width = 5, height = 5)

# Online/F2f
dom_ed2$online <- ifelse(dom_ed2$poll_name %in% online, "Online", "F2F")

ggplot(dom_ed2, aes(online, extgrp_grp)) +
geom_density(aes(fill = online, color = online, group = online, extgrp_grp, y = ..scaled..), alpha = .1) +
theme_bw() +
scale_fill_grey() +
scale_color_grey() +
xlab(expression(D["gj"])) +
ylab("Rel. Freq.") +
theme(axis.text = element_text(size = 7), 
      axis.ticks = element_blank(),
      panel.grid.major.x = element_line(color = "#eeeeee"),
      panel.grid.major.y = element_line(color = "#dddddd", linetype = "dotted"),
      panel.grid.minor = element_blank(),
      plot.margin = unit(c(.5, .5, .5, .5), "cm"),
      panel.border = element_rect(0, 0, 0, 0),
      legend.title = element_blank())
ggsave("figs/density_online_f2f_dom_ed.png", width = 5, height = 5)

# Domination ---  Triple 
# -------------------------

# Take out missing
dom_t32  <- subset(dom_t3, !is.na(extgrp_grp))

ggplot(dom_t32, aes(extgrp_grp)) +
geom_density(aes(x = extgrp_grp, y = ..count../sum(..count..))) + 
theme_bw() +
xlab(expression(D["gj"])) +
ylab("Rel. Freq.") +
theme(axis.text = element_text(size = 7),
      axis.ticks = element_blank(),
      panel.grid.major.x = element_line(color = "#eeeeee"),
      panel.grid.major.y = element_line(color = "#dddddd", linetype = "dotted"),
      panel.grid.minor = element_blank(),
      plot.margin = unit(c(.5, .5, .5, .5), "cm"),
      panel.border = element_rect(0, 0, 0, 0))
ggsave("figs/density_dom_triple.png", width = 3.3, height = 3)

ggplot(dom_t32, aes(poll_name, extgrp_grp)) +
geom_boxplot() +
theme_bw() +
coord_flip() +
scale_y_continuous(breaks = round(seq(min(dom_t32$extgrp_grp), max(dom_t32$extgrp_grp), by = .1), 2)) +
theme(axis.title.x = element_blank(),
      axis.text = element_text(size = 7),
      axis.title.y = element_blank(),
      axis.ticks = element_blank(),
      panel.grid.major.x = element_line(color = "#eeeeee"),
      panel.grid.major.y = element_line(color = "#dddddd", linetype = "dotted"),
      panel.grid.minor = element_blank(),
      plot.margin = unit(c(.5, .5, .5, .5), "cm"),
      panel.border = element_rect(0, 0, 0, 0))
ggsave("figs/boxplot_dom_triple.png", width = 5, height = 5)

dom_t32$anglo <- ifelse(dom_t32$poll_name %in% anglo, "Anglo", "Other")

ggplot(dom_t32, aes(anglo, extgrp_grp)) +
geom_boxplot() +
theme_bw() +
ylab(expression(D["gj"])) +
xlab("") +
coord_flip() +
scale_y_continuous(breaks = round(seq(min(dom_t32$extgrp_grp), max(dom_t32$extgrp_grp), by = .1), 2)) +
theme(axis.text = element_text(size = 7),
      axis.ticks = element_blank(),
      panel.grid.major.x = element_line(color = "#eeeeee"),
      panel.grid.major.y = element_line(color = "#dddddd", linetype = "dotted"),
      panel.grid.minor = element_blank(),
      plot.margin = unit(c(.5, .5, .5, .5), "cm"),
      panel.border = element_rect(0, 0, 0, 0))
ggsave("figs/boxplot_anglo_dom_triple.png", width = 5, height = 5)


ggplot(dom_t32, aes(anglo, extgrp_grp)) +
geom_density(aes(fill = anglo, color = anglo, group = anglo, extgrp_grp, y = ..scaled..), alpha = .1) +
theme_bw() +
scale_fill_grey() +
scale_color_grey() +
xlab(expression(D["gj"])) +
ylab("Rel. Freq.") +
theme(axis.text = element_text(size = 7),
      axis.ticks = element_blank(),
      panel.grid.major.x = element_line(color = "#eeeeee"),
      panel.grid.major.y = element_line(color = "#dddddd", linetype = "dotted"),
      panel.grid.minor = element_blank(),
      plot.margin = unit(c(.5, .5, .5, .5), "cm"),
      panel.border = element_rect(0, 0, 0, 0),
      legend.title = element_blank())
ggsave("figs/density_anglo_dom_triple.png", width = 5, height = 5)


# Online/F2f
dom_t32$online <- ifelse(dom_t32$poll_name %in% online, "Online", "F2F")

ggplot(dom_t32, aes(online, extgrp_grp)) +
geom_density(aes(fill = online, color = online, group = online, extgrp_grp, y = ..scaled..), alpha = .1) +
theme_bw() +
scale_fill_grey() +
scale_color_grey() +
xlab(expression(D["gj"])) +
ylab("Rel. Freq.") +
theme(axis.text = element_text(size = 7), 
      axis.ticks = element_blank(),
      panel.grid.major.x = element_line(color = "#eeeeee"),
      panel.grid.major.y = element_line(color = "#dddddd", linetype = "dotted"),
      panel.grid.minor = element_blank(),
      plot.margin = unit(c(.5, .5, .5, .5), "cm"),
      panel.border = element_rect(0, 0, 0, 0),
      legend.title = element_blank())
ggsave("figs/density_online_f2f_dom_triple.png", width = 5, height = 5)
