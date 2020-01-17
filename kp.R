# LOAD PACKAGES
#install.packages("pacman")
library(pacman)
p_load(pacman, dplyr, GGally, ggplot2, ggthemes, 
       ggvis, httr, lubridate, plotly, rio, rmarkdown, shiny, 
       stringr, tidyr, MASS, oddsratio, finalfit, grid,survival,survminer,plyr, gh, git2r) 

#START HERE
#import KP data set
kp_csv <- import("kp_spine.csv")
df <- data.frame(kp_csv)

#data clean-up
df$los <- as.numeric(df$los)
df$outcome <- ifelse(df$outcome == 1 | df$outcome == 2 | df$outcome == 3 | df$outcome == 4, df$outcome, NA)
                     
##ggplots
ggplot(data=df, aes(x=los,y=levels,color=trauma)) + geom_point() + geom_smooth(aes(group=trauma), method = lm)
ggplot(data=df, aes(x=age,y=levels,color=trauma)) + geom_point()
ggplot(subset(na.omit(df)), aes(outcome,age)) + geom_bar(stat = "summary", fun.y = "mean")

