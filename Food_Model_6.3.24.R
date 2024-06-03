# 6.3.24
# Food Model

library(tidyverse)
library(readxl)
library(car) #for vif analysis
options(scipen = 999)

#importing the dataset with the raw and z variables (created the z-scores in R last fall)
Z_and_Final_Variables_10.28.22 <- read.csv("~/Desktop/R Directory/ResiliencyProject/Z_and_Final_Variables_10.28.22.csv")
glimpse(Z_and_Final_Variables_10.28.22)

#DV 1: FI Percent
hist(Z_and_Final_Variables_10.28.22$z_fi_percent)
ggplot(data = Z_and_Final_Variables_10.28.22, mapping = aes(x = z_fi_percent)) + geom_histogram() 

#DV 2: childelig_lunch_perc
hist(Z_and_Final_Variables_10.28.22$z_childelig_lunch_perc)
ggplot(data = Z_and_Final_Variables_10.28.22, mapping = aes(x = z_childelig_lunch_perc)) + geom_histogram() 

#just checking the RAW variables
unique(Z_and_Final_Variables_10.28.22$z_fi_percent)
summary(Z_and_Final_Variables_10.28.22$z_fi_percent) # 206 missing values
#Where is that NA? 
missing_fi_percent <- Z_and_Final_Variables_10.28.22 %>%
  filter(is.na(z_fi_percent)) # Missing values are Illinois and Kansas
table(missing_fi_percent$State.x) # IL (101) and KS (105)
