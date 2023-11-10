#11.10.23
#Business Model

library(tidyverse)
library(readxl)
library(car) #for vif analysis

#importing the dataset with the raw and z variables (created the z-scores in R last fall)
Z_and_Final_Variables_10.28.22 <- read.csv("~/Desktop/R Directory/ResiliencyProject/Z_and_Final_Variables_10.28.22.csv")

#DV 1: GINI
ggplot(data = Z_and_Final_Variables_10.28.22, mapping = aes(x = z_ACS_GINI_INDEX)) + geom_histogram() 

#DV 2: Unemploy Perc
ggplot(data = Z_and_Final_Variables_10.28.22, mapping = aes(x = z_unemp_perc)) + geom_histogram() 

#just checking the RAW variables
unique(Z_and_Final_Variables_10.28.22$ACS_GINI_INDEX)
summary(Z_and_Final_Variables_10.28.22$ACS_GINI_INDEX) #1 NA
#Where is that NA? 
Z_and_Final_Variables_10.28.22 %>%
  filter(is.na(z_ACS_GINI_INDEX)) #it's FIPS 35039, New Mexico, Rio Arriba county

unique(Z_and_Final_Variables_10.28.22$unemp_perc)
summary(Z_and_Final_Variables_10.28.22$unemp_perc) #few high (max: 19.9, mean: 4.1, Median 3.8); #1 NA
#Where is that NA? 
Z_and_Final_Variables_10.28.22 %>%
  filter(is.na(z_unemp_perc)) #it's FIPS 15005, Hawaii, Kalawao county
