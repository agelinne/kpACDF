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

#--------------------dysphagia vs no dysphagia analysis -------------------------------#
df$out.abrev <- ifelse(df$outcome == 2 | df$outcome == 3, 1, 0)
df$out.abrev <- ifelse(df$outcome == 4, NA, df$out.abrev)
df$out.abrev <- factor(df$out.abrev, levels = c("0","1"), labels = c("No Dysphagia", "Dysphagia"))
#df$los.abrev <- ifelse(df$los <= 30, df$los, NA)
df.E <- df[ which(df$los != 36), ]
df.na <- df.E[ which(!is.na(df.E$out.abrev)), ]

##Summary Tables dysphagia (2 outcomes of dysphagia vs no dyspagia, excluding those unable to be assessed)
sink(file="simple.txt")
#Summary Tables dysphagia
explanatory = c("age","lev","corp","sex","los","trauma")
dependent = 'out.abrev'
summary_factorlist(df.na,dependent, explanatory,
                   p=TRUE, add_dependent_label=TRUE, p_cont_para = "t.test",
                   column = FALSE,  total_col = TRUE, add_col_totals = TRUE,  
                   add_row_totals = TRUE, digits=c(1,2,3,4))

#Regression Table dysphagia
df.na$lev.f <- as.factor(df.na$lev)
explanatory = c("age","lev.f","los")
dependent = 'out.abrev'
finalfit(df.na,dependent, explanatory, metrics=TRUE)
sink()

#ANOVA levels to outcome
leveneTest(lev~out.abrev, data=df)
lev.aov <- aov(lev ~ out.abrev, data = df)
summary(lev.aov)
TukeyHSD(lev.aov)

sink()
                    
##ggplots
df.na$los.j <- jitter(df.na$los, factor=0.2)
df.na$lev.j <- jitter(df.na$lev, factor=0.2)

ggplot(df.na, aes(x=los.j,y=lev.j,color=out.abrev)) + geom_point() + geom_smooth(method=lm)

##Jitter plots Dysphagia
binomial_smooth <- function(...) {
  geom_smooth(method = "glm", method.args = list(family = "binomial"), ...)
}

tiff("inf.tiff", units="in", width=3, height=5, res=300)
ggplot(df.na, aes(age, as.numeric(out.abrev)-1)) +
  geom_jitter(height=.05) + binomial_smooth() + facet_grid(lev~.) +
  theme(strip.text.y = element_text(size=12, face="bold"),
        axis.title.x=element_text(size=12,face="bold"),
        axis.title.y=element_text(size=12, face = "bold")) + 
  xlab("Age") + ylab("Dysphagia Index")
dev.off()

ggplot(df.na, aes(los, as.numeric(out.abrev)-1)) +
  geom_jitter(height=.05) + binomial_smooth() + theme_bw() +
  xlab("Age") + ylab("Dysphagia Index")

p <- ggplot(df.na, aes(x=age,y=levels)) + geom_point() + geom_smooth(method = "auto")
facet(p,facet.by = "out.abrev")

ggboxplot(df.na, x = "out.abrev", y = "los")

#remove packages
p_unload(all) 
detach("package:datasets", unload = TRUE) 

#clean up
rm(list = ls())
cat("\014")  
dev.off()
