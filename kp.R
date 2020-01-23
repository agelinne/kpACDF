# LOAD PACKAGES
#install.packages("pacman")
library(pacman)
p_load(pacman, dplyr, GGally, ggplot2, ggthemes, 
       ggvis, httr, lubridate, plotly, rio, rmarkdown, shiny, 
       stringr, tidyr, MASS, oddsratio, finalfit, grid,survival,survminer,plyr, car, gh, git2r) 

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
df$lev <- as.numeric(as.character(df$levels))

#Initiate file output
sink(file="output.txt")

#ANOVA levels to outcome
leveneTest(lev~outcome, data=df)
lev.aov <- aov(lev ~ outcome, data = df)
summary(lev.aov)
TukeyHSD(lev.aov)
ggboxplot(df, x = "outcome", y = "lev", order = c("1","2","3","4"))

##Summary Tables dysphagia
#Summary Tables dysphagia
explanatory = c("age","lev","corp","sex","los","trauma")
dependent = 'outcome'
summary_factorlist(df,dependent, explanatory,
                   p=TRUE, add_dependent_label=TRUE, cont = "mean", na_to_missing = TRUE)

#Regression Table dysphagia
explanatory = c("age","lev","corp","sex","los","trauma")
dependent = 'out'
finalfit(df,dependent, explanatory, metrics=TRUE, na_to_missing = TRUE)

#Regression Table dysphagia (only signficant variables)
explanatory = c("corp","los","trauma")
dependent = 'out'
finalfit(df,dependent, explanatory, metrics=TRUE)

#--------------------dysphagia vs no dysphagia analysis, POD < 30--------------------------------#
df$out.abrev <- ifelse(df$outcome == 1 | df$outcome == 2, df$outcome, NA)
df$out.abrev <- ifelse(df$out.abrev == 2, 1, 0)
df$out.abrev <- factor(df$out.abrev, levels = c("0","1"), labels = c("No Dysphagia", "Dysphagia"))
df$los.abrev <- ifelse(df$los <= 30, df$los, NA)

##Summary Tables dysphagia (2 outcomes of dysphagia @ POD 21 vs no dyspagia)
#Summary Tables dysphagia
explanatory = c("age","lev","corp","sex","los.abrev","trauma")
dependent = 'out.abrev'
summary_factorlist(df,dependent, explanatory,
                   p=TRUE, add_dependent_label=TRUE, cont = "mean", na_to_missing = TRUE)

#Regression Table dysphagia
explanatory = c("age","lev","corp","sex","los.abrev","trauma")
dependent = 'out.abrev'
finalfit(df,dependent, explanatory, metrics=TRUE, na_to_missing = TRUE)

#ANOVA levels to outcome
leveneTest(lev~out.abrev, data=df)
lev.aov <- aov(lev ~ out.abrev, data = df)
summary(lev.aov)
TukeyHSD(lev.aov)
ggboxplot(df, x = "out.abrev", y = "lev", order = c("1","2"))


sink()
                    
##ggplots
ggplot(data=na.omit(df), aes(x=los.abrev,y=age,color=out.abrev)) + geom_point() + geom_smooth(method = lm)
p <- ggplot(data=df, aes(x=age,y=levels)) + geom_point() + geom_smooth(method = "auto")
facet(p,facet.by = "outcome")

#remove packages
p_unload(all) 
detach("package:datasets", unload = TRUE) 

#clean up
rm(list = ls())
cat("\014")  
dev.off()
