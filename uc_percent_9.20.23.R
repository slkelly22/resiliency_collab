#9.20.23
#Checking uc_percent variable
#AC thinks there is something strange with the uc_percent variable so SK wants to check the original variable for any strange patterns

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
  arrange(desc(state_level_ucp)) #update: this is not correct; turns out Illinois is higher but R is ignoring that state since it has missing data (see details below)

## Note: See the statewide_uc_percent quarto file for continuation

#Came back to add a boxplot here before I add it to the qmd file; I already have a statelevel average of uc plotting in the qmd but
# I want to see the variability within and across uc_percent and a boxplot would be better for that internal validity 

Z_and_Final_Variables_10.28.22 %>%
  ggplot(mapping = aes(x = State.x, y = uc_percent)) + geom_boxplot() + labs(x = "State", y = "UC Percent (Raw Metric")
#oh crap. Look at Illinois. Where is that coming from? R didn't plot Illinois in the scatter from yesterday. 
#apparently this is plotting even with missing underlying data, but when I created the scatterplot yesterday, I created a new variable off the underlying data and plotted the new variable, BUT that new variable didn't get created with the underlying state had missing values, which IL and HI did; KS was empty and shown empty; IL and HI were shown empty but they were not; they just had missing data

Just_IL <- Z_and_Final_Variables_10.28.22 %>%
  filter(State.x == "il") %>%
  select(uc_percent) 
View(Just_IL) #okay, so Illinois HAS data and isn't empty
str(Just_IL) #and it's numeric data, so why didn't R plot it in the scatter from yesterday? 
sum(is.na(Just_IL)) #it has 51 NAs

na.omit(Just_IL) #this shows all the non-NA values; these are wayyyyy out of line with the other variables
sum(is.na(Just_IL$uc_percent)) #there's 51 out of 102 that are NAs

mean(Just_IL$uc_percent, na.rm = TRUE) #that's working; the average is 37.56

unique(Z_and_Final_Variables_10.28.22$uc_percent) #you see these crazy large numbers here; must be from IL

Just_KS <- Z_and_Final_Variables_10.28.22 %>%
  filter(State.x == "ks") %>%
  select(uc_percent) 
View(Just_KS) #yup, this is all NAs; Kansas doesn't have data for uc_percent

Just_HI <- Z_and_Final_Variables_10.28.22 %>%
  filter(State.x == "hi") %>%
  select(uc_percent) 
View(Just_HI) #so...Hawaii also has data; 4 observations and 1 NA
na.omit(Just_HI)
sum(Just_HI, na.rm = TRUE)#there is data for Hawaii
10.77842/5 #why is it dividing by 5, that 5th number is a NA
mean(Just_HI$uc_percent, na.rm = TRUE) #this is doing what we want...dividing by 4 not 5

str(Just_HI[1, 1]) #yup
str(Just_HI[3, 1]) #this says NA is a number.....hmmmm not right

#na.rm doesn't go with the summarize function, it lives within the parentheses of the sum function
Z_and_Final_Variables_10.28.22 %>%
  group_by(State.x) %>%
  summarize(state_level_ucp = sum(uc_percent, na.rm = TRUE)/n()) %>% 
  arrange(desc(state_level_ucp)) %>%
  head(10) #Illinois is still coming up at 18.8 percent but it should be much higher than that given the numerical values; what is going on....

#here's what's going on: 
#summarize(state_level_ucp = sum(uc_percent, na.rm = TRUE)/n()) 
# the n() is counting all the observations, including the NAs, and dividing by THAT NUMBER; it doesn't matter that na.rm is there because that argument is for sum, you'd need to add na.rm to the n() function and R/Tidy/summarize doesn't allow you to do that
# solution: don't use n() which counts the NAs as observations, just use mean() rather than sum / n()

#this is what we want; use the MEAN not create the mean through sum and /n
Z_and_Final_Variables_10.28.22 %>%
  group_by(State.x) %>%
  summarize(state_level_ucp = mean(uc_percent, na.rm = TRUE)) %>%
  arrange(desc(state_level_ucp)) %>%
  head(10) 

#Watch the difference in sum vs. mean in the two code blocks below: 
#lowest ten; nope
Z_and_Final_Variables_10.28.22 %>%
  group_by(State.x) %>%
  summarize(state_level_ucp = sum(uc_percent, na.rm = TRUE)/n()) %>%
  arrange(desc(state_level_ucp)) %>%
  tail(10) #nope, KS should not be zero; that sum function is adding 0s and then diving by 105, which is zero

Z_and_Final_Variables_10.28.22 %>%
  group_by(State.x) %>%
  summarize(state_level_ucp = mean(uc_percent, na.rm = TRUE)) %>%
  arrange(desc(state_level_ucp)) %>%
  tail(10) #that correctly recognizes KS as NaN

#Fun challenge! 
#Be sure to also review the qmd file related to uc_percent