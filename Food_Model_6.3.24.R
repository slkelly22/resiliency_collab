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

#just checking the RAW variables
round(unique(Z_and_Final_Variables_10.28.22$fi_percent)) #note:rounded to whole for quicker checks
summary(Z_and_Final_Variables_10.28.22$fi_percent) # 206 missing values
#Where are the NAs?
missing_fi_percent <- Z_and_Final_Variables_10.28.22 %>%
  filter(is.na(fi_percent)) # Missing values are Illinois and Kansas
table(missing_fi_percent$State.x) # IL (101) and KS (105)

#DV 2: childelig_lunch_perc
hist(Z_and_Final_Variables_10.28.22$z_childelig_lunch_perc)
ggplot(data = Z_and_Final_Variables_10.28.22, mapping = aes(x = z_childelig_lunch_perc)) + geom_histogram() 

#just checking the RAW variables
round(unique(Z_and_Final_Variables_10.28.22$childelig_lunch_perc)) # note: rounded to whole for quicker checks
summary(Z_and_Final_Variables_10.28.22$childelig_lunch_perc) # 227 missing values
# What are those strange numbers? 
strange_values_child <- Z_and_Final_Variables_10.28.22 %>% 
  filter(childelig_lunch_perc < abs(5)) 
table(strange_values_child$State.x)
strange_values_child$childelig_lunch_perc

#Where are the NAs?
missing_child <- Z_and_Final_Variables_10.28.22 %>%
  filter(is.na(childelig_lunch_perc)) 
table(missing_child$State.x) 

