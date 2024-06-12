# Re: the Food Model, the child lunch percent variable is garbage
# Cafer send over a file with 2018 data; just IL county and 2018 child percent (both character data)
# 6.12.24

library(tidyverse)
library(readxl)

#importing the dataset with the raw and z variables (created the z-scores in R last fall)
Z_and_Final_Variables_10.28.22 <- read.csv("~/Desktop/R Directory/ResiliencyProject/Z_and_Final_Variables_10.28.22.csv")
glimpse(Z_and_Final_Variables_10.28.22)

data <- Z_and_Final_Variables_10.28.22

# First I'm just creating a smaller dataset with the IL child percent garbage data from the main data file that we've used since 2022

IL_replace <- data %>% 
  filter(State.x == "il") %>% 
  select(county, childelig_lunch_perc)

write_csv(IL_replace, "IL_replace.csv")

# I put the IL_replace.csv and the Cafer csv into the data folder

# You'll need to import the Cafer data for child lunch (it's currently a character because of %), strip the % with stringer, and then change the character value to numeric, then try to merge the two datasets based on the county names; you'll also have to lowercase the county names (of course, can't be too easy

# yeah, but the bigger problem is whether I'm going to overwrite the original data file that I've been using since 2002 (Z and raw scores, etc.); I mean I can merge these two smaller files, but then what

# and don't forget that I'll have to z-score it too afterward