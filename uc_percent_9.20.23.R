#9.20.23
#Checking uc_percent variable
#AC thinks there is something strange with the uc_percent variable so SK wants to check the orginal variable for any strange patterns

library(tidyverse)
library(readxl)

#importing the dataset with the raw and z variables (created the z-scores in R last fall)
Z_and_Final_Variables_10.28.22 <- read.csv("~/Desktop/R Directory/ResiliencyProject/Z_and_Final_Variables_10.28.22.csv")

View(Z_and_Final_Variables_10.28.22)

unique(Z_and_Final_Variables_10.28.22$uc_percent)
summary(Z_and_Final_Variables_10.28.22$uc_percent)


#arranged alpha by state
Z_and_Final_Variables_10.28.22 %>%
  group_by(State.x) %>%
  summarize(state_level_ucp = sum(uc_percent)/n()) %>%
  arrange(State.x)

#arranged descending by state uc_percent
Z_and_Final_Variables_10.28.22 %>%
  group_by(State.x) %>%
  summarize(state_level_ucp = sum(uc_percent)/n()) %>%
  arrange(desc(state_level_ucp)) 

## Note: See the statewide_uc_percent quarto file for continuation
