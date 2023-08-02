#8.2.23 
#Infrastructure Model
#DV: Broadband (z_ACS_PCT_BROADBAND)

#let's take a look at that variable

library(tidyverse)
library(readxl)
library(car) #for vif analysis

#importing the dataset with the raw and z variables (created the z-scores in R last fall)
Z_and_Final_Variables_10.28.22 <- read.csv("~/Desktop/R Directory/ResiliencyProject/Z_and_Final_Variables_10.28.22.csv")

summary(Z_and_Final_Variables_10.28.22)

#z variable
ggplot(data = Z_and_Final_Variables_10.28.22, mapping = aes(x = z_ACS_PCT_BROADBAND)) + geom_histogram()
#histogram looks good

#regular variable
ggplot(data = Z_and_Final_Variables_10.28.22, mapping = aes(x = ACS_PCT_BROADBAND)) + geom_histogram()
#histogram looks good

unique(Z_and_Final_Variables_10.28.22$ACS_PCT_BROADBAND)
summary(Z_and_Final_Variables_10.28.22$ACS_PCT_BROADBAND) #min 25.63, #max 95.51

#alright so this should work as a DV, now where was it an IV in the other models? 
#It was in the technology grouping along with two other IVs: ACS_PCT_NO_PC and ACS_PCT_SMARTPHONE
