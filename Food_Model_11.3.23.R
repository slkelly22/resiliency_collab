#Food Model

library(tidyverse)
library(readxl)
library(car) #for vif analysis

#importing the dataset with the raw and z variables (created the z-scores in R last fall)
Z_and_Final_Variables_10.28.22 <- read.csv("~/Desktop/R Directory/ResiliencyProject/Z_and_Final_Variables_10.28.22.csv")

#food model possible variables
ggplot(data = Z_and_Final_Variables_10.28.22, mapping = aes(x = z_food_afford)) + geom_histogram() #use as DV

ggplot(data = Z_and_Final_Variables_10.28.22, mapping = aes(x = z_fd_enviro_indx)) + geom_histogram() #don't use as DV

ggplot(data = Z_and_Final_Variables_10.28.22, mapping = aes(x = z_fi_percent)) + geom_histogram() #use as DV