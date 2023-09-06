#Date: 9.6.23 Meeting with AC
#Next Step: Government Model

library(tidyverse)
library(readxl)
library(car) #for vif analysis

#importing the dataset with the raw and z variables 
Z_and_Final_Variables_10.28.22 <- read.csv("~/Desktop/R Directory/ResiliencyProject/Z_and_Final_Variables_10.28.22.csv")

summary(Z_and_Final_Variables_10.28.22)

#looking at the possible DVs for a government model
ggplot(data = Z_and_Final_Variables_10.28.22, mapping = aes(x = z_soc_assoc_num)) + geom_histogram() #nope

ggplot(data = Z_and_Final_Variables_10.28.22, mapping = aes(x = z_aeo_perc_w_acc)) + geom_histogram() #yes as DV

ggplot(data = Z_and_Final_Variables_10.28.22, mapping = aes(x = z_dwv_presence)) + geom_histogram() #Nope

ggplot(data = Z_and_Final_Variables_10.28.22, mapping = aes(x = z_ACS_PCT_HH_PUB_ASSIST)) + geom_histogram() #eh...not great but maybe

ggplot(data = Z_and_Final_Variables_10.28.22, mapping = aes(x = z_AHRF_TXC_SITE_NO_CNTRL)) + geom_histogram() #nope

#AC also mentioned maybe using a variable about violent crime; think the var name is vc_ann_avg; not in this dataset but maybe in the early early data
