library(tidyverse)
library(readxl)
library(car) #for vif analysis

#importing the dataset with the raw and z variables (created the z-scores in R last fall)
Z_and_Final_Variables_10.28.22 <- read.csv("~/Desktop/R Directory/ResiliencyProject/Z_and_Final_Variables_10.28.22.csv")

View(Z_and_Final_Variables_10.28.22)

ggplot(data = Z_and_Final_Variables_10.28.22, mapping = aes(x = z_ACS_GINI_INDEX)) + geom_histogram() #yes, let's use as DV

ggplot(data = Z_and_Final_Variables_10.28.22, mapping = aes(x = z_ACS_PCT_WORK_NO_CAR)) + geom_histogram() #nope

ggplot(data = Z_and_Final_Variables_10.28.22, mapping = aes(x = z_unemp_perc)) + geom_histogram() #yes, let's use as the second DV
