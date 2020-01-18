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
df$outcome <- ifelse(df$outcome == 1 | df$outcome == 2 | df$outcome == 3 | df$outcome == 4, df$outcome, 4)
df$sex <- factor(df$sex,levels = c("0","1"),labels=c("Female","Male"))
df$corp <- ifelse(df$corpectomy == "N/A", 0, 1)
df$corp <- factor(df$corp, levels = c("0","1"), labels=c("No Corpectomy","Corpectomy"))
df$out <- as.numeric(df$outcome)

#Initiate file output
sink(file="output.txt")

##Summary Tables dysphagia

#Summary Tables dysphagia
explanatory = c("age","levels","corp","sex","los","trauma")
dependent = 'outcome'
summary_factorlist(df,dependent, explanatory,
                   p=TRUE, add_dependent_label=TRUE)

#Regression Table dysphagia
explanatory = c("age","levels","corp","sex","los","trauma")
dependent = 'out'
finalfit(df,dependent, explanatory, metrics=TRUE)

#Regression Table dysphagia (only signficant variables)
explanatory = c("corp","los","trauma")
dependent = 'out'
finalfit(df,dependent, explanatory, metrics=TRUE)

sink()
                     
##ggplots
ggplot(data=df, aes(x=los,y=levels,color=trauma)) + geom_point() + geom_smooth(aes(group=trauma), method = lm)
ggplot(data=df, aes(x=age,y=levels,color=trauma)) + geom_point()
ggplot(subset(na.omit(df)), aes(outcome,age)) + geom_bar(stat = "summary", fun.y = "mean")

#remove packages
p_unload(all) 
detach("package:datasets", unload = TRUE) 

#clean up
rm(list = ls())
cat("\014")  
dev.off()