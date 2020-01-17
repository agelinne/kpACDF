# LOAD PACKAGES
#install.packages("pacman")
library(pacman)
p_load(pacman, dplyr, GGally, ggplot2, ggthemes, 
       ggvis, httr, lubridate, plotly, rio, rmarkdown, shiny, 
       stringr, tidyr, MASS, oddsratio, finalfit, grid,survival,survminer,plyr, gh, git2r) 

#START HERE
#import KP data set
kp_csv <- import("C:/Users/safranco/Desktop/KP ACDF/kp_spine.csv")

#test