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

#Building the Government Model with AEO as the DV
#AEO is the Percentage of population with adequate access to locations for physical activity
options(scipen = 999) #turning off scientific notation

#Outcome: Access to Exercise Opportunities
#IVs: Demographics Only (9 variables)
z_aeo_just_demos <- lm(formula = z_aeo_perc_w_acc ~ z_ACS_PCT_BLACK + z_ACS_PCT_HISPAN + z_SVI_RPL_THEME1_SOCIECO + z_SVI_RPL_THEME2_HH_DISB + z_SVI_RPL_THEME3_MINO + z_SVI_RPL_THEME4_HH_TRANS + z_ACS_PCT_ENGL_NOT_WELL + z_ACS_PCT_NON_CITIZEN + z_ACS_MEDIAN_AGE, data = Z_and_Final_Variables_10.28.22) 
summary(z_aeo_just_demos)
confint(z_aeo_just_demos)
vif(z_aeo_just_demos)

#Outcome: Access to Exercise Opportunities
#IVs: Food & Housing Only (14 variables)
z_aeo_just_foodhouse <- lm(formula = z_aeo_perc_w_acc ~ z_food_afford + z_fd_enviro_indx + z_fi_percent + z_ACS_PCT_FOOD_STAMP + z_childelig_lunch_perc + z_shp_percent + z_shcb_percent + z_ACS_PCT_NO_VEH + z_ACS_PCT_RENTER_HH_CHILD + z_ACS_PCT_RENT_COST_30PCT + z_ACS_PCT_MOBILE_HOME + z_ACS_PCT_OWNED_HH + z_ACS_PCT_OWNER_HH_CHILD + z_ACS_MEDIAN_HOME_VALUE, data = Z_and_Final_Variables_10.28.22)
summary(z_aeo_just_foodhouse)
confint(z_aeo_just_foodhouse)
vif(z_aeo_just_foodhouse)

#Outcome: Access to Exercise Opportunities
#IVs: Education (3 variables)
z_aeo_just_ed <- lm(formula = z_aeo_perc_w_acc ~ z_hsg_rate + z_sc_percent + z_ms_average, data = Z_and_Final_Variables_10.28.22)
summary(z_aeo_just_ed)
confint(z_aeo_just_ed)
vif(z_aeo_just_ed)

#Outcome: Access to Exercise Opportunities
#IVs: Technology (3 variables)
z_aeo_just_tech <- lm(formula = z_aeo_perc_w_acc ~ z_ACS_PCT_BROADBAND + z_ACS_PCT_NO_PC + z_ACS_PCT_SMARTPHONE, data = Z_and_Final_Variables_10.28.22)
summary(z_aeo_just_tech)
confint(z_aeo_just_tech)
vif(z_aeo_just_tech)

#Outcome: Access to Exercise Opportunities
#IVs: HWCC (7 variables)
z_aeo_just_HWCC <- lm(formula = z_aeo_perc_w_acc ~ z_pphd_avgnum + z_pmhd_avgnum + z_lbw_percent_lbw + z_pi_phy_inact + z_cip_perc + z_csph_perc + z_ACS_PCT_CHILD_DISAB, data = Z_and_Final_Variables_10.28.22)
summary(z_aeo_just_HWCC)
confint(z_aeo_just_HWCC)
vif(z_aeo_just_HWCC)

#Outcome: Access to Exercise Opportunities
#IVs: Health A (7 variables)
z_aeo_just_healthA <- lm(formula = z_aeo_perc_w_acc ~ z_phys_per_cap + z_mhp_per_cap + z_dents_per_cap + z_uninsur_perc + z_uc_percent + z_opcp_rate + z_TeenBirths, data = Z_and_Final_Variables_10.28.22)
summary(z_aeo_just_healthA)
confint(z_aeo_just_healthA)
vif(z_aeo_just_healthA)

#Outcome: Access to Exercise Opportunities
#IVs: Health B (5 variables)
z_aeo_just_healthB <- lm(formula = z_aeo_perc_w_acc ~ z_phs_rate + z_ms_perc_ann + z_AMFAR_AMATFAC + z_MP_PERCPEN + z_AHRF_RURAL_H_CLINIC, data = Z_and_Final_Variables_10.28.22)
summary(z_aeo_just_healthB)
confint(z_aeo_just_healthB)
vif(z_aeo_just_healthB)

#Outcome: Access to Exercise Opportunities
#IVs: Business (4 variables)
z_aeo_just_bus <- lm(formula = z_aeo_perc_w_acc ~ z_unemp_perc + z_ACS_GINI_INDEX + z_ACS_MEDIAN_HH_INCOME + z_ACS_PCT_WORK_NO_CAR, data = Z_and_Final_Variables_10.28.22)
summary(z_aeo_just_bus)
confint(z_aeo_just_bus)
vif(z_aeo_just_bus)

#Outcome: Access to Exercise Opportunities
#IVs: Government (4 variables; removed AEO)
z_aeo_just_gov <- lm(formula = z_aeo_perc_w_acc ~ z_soc_assoc_num + z_dwv_presence + z_ACS_PCT_HH_PUB_ASSIST + z_AHRF_TXC_SITE_NO_CNTRL, data = Z_and_Final_Variables_10.28.22)
summary(z_aeo_just_gov)
confint(z_aeo_just_gov)
vif(z_aeo_just_gov)

#Final Government Model
#Outcome: Access to Exercise Opportunities
#IVs: 56
z_aeo_levels_12345678 <- lm(formula = z_aeo_perc_w_acc ~ z_ACS_PCT_BLACK + z_ACS_PCT_HISPAN + z_SVI_RPL_THEME1_SOCIECO + z_SVI_RPL_THEME2_HH_DISB + z_SVI_RPL_THEME3_MINO + z_SVI_RPL_THEME4_HH_TRANS + z_ACS_PCT_ENGL_NOT_WELL + z_ACS_PCT_NON_CITIZEN + z_ACS_MEDIAN_AGE + z_food_afford + z_fd_enviro_indx + z_fi_percent + z_ACS_PCT_FOOD_STAMP + z_childelig_lunch_perc + z_shp_percent + z_shcb_percent + z_ACS_PCT_NO_VEH + z_ACS_PCT_RENTER_HH_CHILD + z_ACS_PCT_RENT_COST_30PCT + z_ACS_PCT_MOBILE_HOME + z_ACS_PCT_OWNED_HH + z_ACS_PCT_OWNER_HH_CHILD + z_ACS_MEDIAN_HOME_VALUE + z_hsg_rate + z_sc_percent + z_ms_average + z_ACS_PCT_BROADBAND + z_ACS_PCT_NO_PC + z_ACS_PCT_SMARTPHONE + z_pphd_avgnum + z_pmhd_avgnum + z_lbw_percent_lbw + z_pi_phy_inact + z_cip_perc + z_csph_perc + z_ACS_PCT_CHILD_DISAB + z_phys_per_cap + z_mhp_per_cap + z_dents_per_cap + z_uninsur_perc + z_uc_percent + z_opcp_rate + z_TeenBirths + z_phs_rate + z_ms_perc_ann + z_AMFAR_AMATFAC + z_MP_PERCPEN + z_AHRF_RURAL_H_CLINIC + z_unemp_perc + z_ACS_GINI_INDEX + z_ACS_MEDIAN_HH_INCOME + z_ACS_PCT_WORK_NO_CAR + z_soc_assoc_num + z_dwv_presence + z_ACS_PCT_HH_PUB_ASSIST + z_AHRF_TXC_SITE_NO_CNTRL, data = Z_and_Final_Variables_10.28.22)
summary(z_aeo_levels_12345678)
confint(z_aeo_levels_12345678)
vif(z_aeo_levels_12345678)


