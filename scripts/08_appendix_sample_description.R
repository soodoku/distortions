"											
Distortions

Proportion of High Income, Better Educated, Men in each small group and s.d. of that
"


# Set basedir
setwd(githubdir)
setwd("distortions/")

# Set StringsAsFactors as FALSE 
options(stringsAsFactors = FALSE)

# Load dat
dpdat <- read.csv("data/polardata.csv")

# Load libs
library(tidyverse)

props <- dpdat %>%
    group_by(pollgroup) %>%
    summarize(p_gender = mean(female == 0, na.rm = T), p_better_ed = mean(bettered == 1), p_high_income = mean(highinc == 1))

# Means
round(colMeans(props, na.rm = T), 3)

# s.d.
round(sd(props$p_gender, na.rm = T), 3)
round(sd(props$p_better_ed, na.rm = T), 3)
round(sd(props$p_high_income, na.rm = T), 3)


ggplot(props) +  
geom_density(aes(p_gender, y = ..scaled..), alpha = .7) + 
theme_bw() +
scale_fill_grey() +
scale_color_grey() +
xlab("Proportion Male") +
ylab("Rel. Freq.") + 
theme(axis.text = element_text(size = 7), 
      axis.ticks = element_blank(),
      panel.grid.major.x = element_line(color = "#eeeeee"), 
      panel.grid.major.y = element_line(color = "#dddddd", linetype = "dotted"),
      panel.grid.minor = element_blank(),
      plot.margin = unit(c(.5, .5, .5, .5), "cm"),
      panel.border = element_rect(0, 0, 0, 0),
      legend.title = element_blank())
ggsave("figs/density_p_gender.png", width = 5, height = 5)


ggplot(props) +  
geom_density(aes(p_high_income, y = ..scaled..), alpha = .7) + 
theme_bw() +
scale_fill_grey() +
scale_color_grey() +
xlab("Proportion Better Educated") +
ylab("Rel. Freq.") + 
theme(axis.text = element_text(size = 7), 
      axis.ticks = element_blank(),
      panel.grid.major.x = element_line(color = "#eeeeee"), 
      panel.grid.major.y = element_line(color = "#dddddd", linetype = "dotted"),
      panel.grid.minor = element_blank(),
      plot.margin = unit(c(.5, .5, .5, .5), "cm"),
      panel.border = element_rect(0, 0, 0, 0),
      legend.title = element_blank())
ggsave("figs/density_p_bettered.png", width = 5, height = 5)


ggplot(props) +  
geom_density(aes(p_better_ed, y = ..scaled..), alpha = .7) + 
theme_bw() +
scale_fill_grey() +
scale_color_grey() +
xlab("Proportion Higher Income") +
ylab("Rel. Freq.") + 
theme(axis.text = element_text(size = 7), 
      axis.ticks = element_blank(),
      panel.grid.major.x = element_line(color = "#eeeeee"), 
      panel.grid.major.y = element_line(color = "#dddddd", linetype = "dotted"),
      panel.grid.minor = element_blank(),
      plot.margin = unit(c(.5, .5, .5, .5), "cm"),
      panel.border = element_rect(0, 0, 0, 0),
      legend.title = element_blank())
ggsave("figs/density_p_highinc.png", width = 5, height = 5)

