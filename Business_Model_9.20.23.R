#9.20.23
#Business Model

library(tidyverse)
library(readxl)
library(car) #for vif analysis

#importing the dataset with the raw and z variables (created the z-scores in R last fall)
Z_and_Final_Variables_10.28.22 <- read.csv("~/Desktop/R Directory/ResiliencyProject/Z_and_Final_Variables_10.28.22.csv")

View(Z_and_Final_Variables_10.28.22)

ggplot(data = Z_and_Final_Variables_10.28.22, mapping = aes(x = z_ACS_GINI_INDEX)) + geom_histogram() #yes, let's use as DV

ggplot(data = Z_and_Final_Variables_10.28.22, mapping = aes(x = z_ACS_PCT_WORK_NO_CAR)) + geom_histogram() #nope

ggplot(data = Z_and_Final_Variables_10.28.22, mapping = aes(x = z_unemp_perc)) + geom_histogram() #yes, let's use as the second DV

#just checking the raw variables
unique(Z_and_Final_Variables_10.28.22$ACS_GINI_INDEX)
summary(Z_and_Final_Variables_10.28.22$ACS_GINI_INDEX)

unique(Z_and_Final_Variables_10.28.22$unemp_perc)
summary(Z_and_Final_Variables_10.28.22$unemp_perc)

#revisiting uc_percent weirdness again (10.31.23)
summary(Z_and_Final_Variables_10.28.22$uc_percent) 

Z_and_Final_Variables_10.28.22 %>%
  group_by(State.x) %>%
  summarize(state_sd = sd(uc_percent, na.rm = TRUE)) %>%
  arrange(desc(state_sd)) %>%
  print(n = 51) #Illinois's standard deviation is 8.88, the next is ND at 4.65

#The real question is how much does the Illinois data affect the models we've done so far? 
#One tactic would be to run a model without the IL data included and see how much (if any) the coefficients change....it might change in the models where uc_percent is one of the few IVs, but not a lot in the final models? Did we use uc_percent as a DV? I don't think so...

#I think the the bigger issue is the lack of DATA VALIDATION CHECKS in the original excel file that was sent to me when I agreed to build the models. The underlying data has issues that are being discovered as we work through the modeling process. 


