# School Lunch Illinois Data Cleanup and Merge
# June 17, 2024

library(tidyverse)
library(stringr)

IL_childelig <- read_csv("data/Cafer_IL school lunch FY18.csv")

str(IL_childelig)
names(IL_childelig)
dim(IL_childelig) #103, 3

# Need to remove the second row, which is a statewide measure, not county, and no FIPS
IL_childelig <- IL_childelig[-1, ]
dim(IL_childelig) #102, 3
head(IL_childelig)

# Lowercase the county name in order to merge later to the main df
IL_childelig$County <- tolower(IL_childelig$County)
head(IL_childelig)

# Need to strip the % from county value
head(IL_childelig)
str(IL_childelig$`County Value`)
# Using a stringr function: str_sub
# I have to be careful because there's a 100 along with the other two-digit numbers
IL_childelig$`County Value`<- str_sub(IL_childelig$`County Value`, end = -2)
IL_childelig$`County Value`
str_length(IL_childelig$`County Value`)

# Now that the % is gone, need to change from character to numeric vector
str(IL_childelig$`County Value`)
IL_childelig$`County Value` <- as.numeric(IL_childelig$`County Value`)
str(IL_childelig$`County Value`)

glimpse(IL_childelig) # yup

range(IL_childelig$FIPS) # FIBS is from 17001 and 17203

# Renaming the variables before the merge
IL_childelig <- rename(IL_childelig, county = County, 
         childelig = `County Value`)
glimpse(IL_childelig)
str(IL_childelig)

# Just pulling out the IL column from the main df to make sure the FIPS look right before we merge
# Most the IL_childelig and the Z_and_Final_Variables datframes have 102 values for Illinois with FIPS 17001 (adams) to 17203 (woodford)
quick_check <- Z_and_Final_Variables_10.28.22 %>% 
  filter(State.x == "il") %>% 
  select(FIPS, county)

# Now the question is how to merge when you simply need to replace part of the vector and not the entire vector

attempt1 <- Z_and_Final_Variables_10.28.22 %>% 
  select(FIPS, State.x, county, childelig_lunch_perc)

head(attempt1)

middle_object <- left_join(attempt1, IL_childelig, by = "FIPS") #3142 observations, yup

# Okay...this is finally doing what I want...

glimpse(middle_object)

# Create a new variable with mutate and case_when; when it's missing in the column that only has Illinois, use the original variable, then when it's not missing, use the IL only column
attempt2 <- middle_object %>% 
  mutate(newvar = case_when(
    is.na(childelig) ~ childelig_lunch_perc, 
    TRUE ~ childelig
  )) 

attempt2 %>% 
  filter(State.x == "il")
