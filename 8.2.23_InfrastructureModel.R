#8.2.23 
#Infrastructure Model
#DV: Broadband (z_ACS_PCT_BROADBAND)

#let's take a look at that potential DV

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

#Building the Infrastructure Model with broadband as the DV
options(scipen = 999) #turning off scientific notation

#Outcome: Broadband
#IVs: Demographics Only (9 variables)
z_bb_just_demos <- lm(formula = z_ACS_PCT_BROADBAND ~ z_ACS_PCT_BLACK + z_ACS_PCT_HISPAN + z_SVI_RPL_THEME1_SOCIECO + z_SVI_RPL_THEME2_HH_DISB + z_SVI_RPL_THEME3_MINO + z_SVI_RPL_THEME4_HH_TRANS + z_ACS_PCT_ENGL_NOT_WELL + z_ACS_PCT_NON_CITIZEN + z_ACS_MEDIAN_AGE, data = Z_and_Final_Variables_10.28.22) 
summary(z_bb_just_demos)
confint(z_bb_just_demos)
vif(z_bb_just_demos)

#Outcome: Broadband
#IVs: Food & Housing Only (14 variables)
z_bb_just_foodhouse <- lm(formula = z_ACS_PCT_BROADBAND ~ z_food_afford + z_fd_enviro_indx + z_fi_percent + z_ACS_PCT_FOOD_STAMP + z_childelig_lunch_perc + z_shp_percent + z_shcb_percent + z_ACS_PCT_NO_VEH + z_ACS_PCT_RENTER_HH_CHILD + z_ACS_PCT_RENT_COST_30PCT + z_ACS_PCT_MOBILE_HOME + z_ACS_PCT_OWNED_HH + z_ACS_PCT_OWNER_HH_CHILD + z_ACS_MEDIAN_HOME_VALUE, data = Z_and_Final_Variables_10.28.22)
summary(z_bb_just_foodhouse)
confint(z_bb_just_foodhouse)
vif(z_bb_just_foodhouse)

#Outcome: Broadband
#IVs: Education (3 variables)
z_bb_just_ed <- lm(formula = z_ACS_PCT_BROADBAND ~ z_hsg_rate + z_sc_percent + z_ms_average, data = Z_and_Final_Variables_10.28.22)
summary(z_bb_just_ed)
confint(z_bb_just_ed)
vif(z_bb_just_ed)

#Outcome: Broadband
#IVs: Technology (*only 2 variables because the third is the DV in this model)
z_bb_just_tech <- lm(formula = z_ACS_PCT_BROADBAND ~ z_ACS_PCT_NO_PC + z_ACS_PCT_SMARTPHONE, data = Z_and_Final_Variables_10.28.22)
summary(z_bb_just_tech)
confint(z_bb_just_tech)
vif(z_bb_just_tech)

#Outcome: Broadband
#IVs: HWCC (7 variables)
z_bb_just_HWCC <- lm(formula = z_ACS_PCT_BROADBAND ~ z_pphd_avgnum + z_pmhd_avgnum + z_lbw_percent_lbw + z_pi_phy_inact + z_cip_perc + z_csph_perc + z_ACS_PCT_CHILD_DISAB, data = Z_and_Final_Variables_10.28.22)
summary(z_bb_just_HWCC)
confint(z_bb_just_HWCC)
vif(z_bb_just_HWCC)

#Outcome: Broadband
#IVs: Health A (7 variables)
z_bb_just_healthA <- lm(formula = z_ACS_PCT_BROADBAND ~ z_phys_per_cap + z_mhp_per_cap + z_dents_per_cap + z_uninsur_perc + z_uc_percent + z_opcp_rate + z_TeenBirths, data = Z_and_Final_Variables_10.28.22)
summary(z_bb_just_healthA)
confint(z_bb_just_healthA)
vif(z_bb_just_healthA)

#Outcome: Broadband
#IVs: Health B (5 variables)
z_bb_just_healthB <- lm(formula = z_ACS_PCT_BROADBAND ~ z_phs_rate + z_ms_perc_ann + z_AMFAR_AMATFAC + z_MP_PERCPEN + z_AHRF_RURAL_H_CLINIC, data = Z_and_Final_Variables_10.28.22)
summary(z_bb_just_healthB)
confint(z_bb_just_healthB)
vif(z_bb_just_healthB)

#Outcome: Broadband
#IVs: Business (4 variables)
z_bb_just_bus <- lm(formula = z_ACS_PCT_BROADBAND ~ z_unemp_perc + z_ACS_GINI_INDEX + z_ACS_MEDIAN_HH_INCOME + z_ACS_PCT_WORK_NO_CAR, data = Z_and_Final_Variables_10.28.22)
summary(z_bb_just_bus)
confint(z_bb_just_bus)
vif(z_bb_just_bus)

#Outcome: Broadband
#IVs: Government (5 variables)
z_bb_just_gov <- lm(formula = z_ACS_PCT_BROADBAND ~ z_soc_assoc_num + z_aeo_perc_w_acc + z_dwv_presence + z_ACS_PCT_HH_PUB_ASSIST + z_AHRF_TXC_SITE_NO_CNTRL, data = Z_and_Final_Variables_10.28.22)
summary(z_bb_just_gov)
confint(z_bb_just_gov)
vif(z_bb_just_gov)

#Final Infrastructure Model
#Outcome: Broadband
#IVs: 56
z_bb_levels_12345678 <- lm(formula = z_ACS_PCT_BROADBAND ~ z_ACS_PCT_BLACK + z_ACS_PCT_HISPAN + z_SVI_RPL_THEME1_SOCIECO + z_SVI_RPL_THEME2_HH_DISB + z_SVI_RPL_THEME3_MINO + z_SVI_RPL_THEME4_HH_TRANS + z_ACS_PCT_ENGL_NOT_WELL + z_ACS_PCT_NON_CITIZEN + z_ACS_MEDIAN_AGE + z_food_afford + z_fd_enviro_indx + z_fi_percent + z_ACS_PCT_FOOD_STAMP + z_childelig_lunch_perc + z_shp_percent + z_shcb_percent + z_ACS_PCT_NO_VEH + z_ACS_PCT_RENTER_HH_CHILD + z_ACS_PCT_RENT_COST_30PCT + z_ACS_PCT_MOBILE_HOME + z_ACS_PCT_OWNED_HH + z_ACS_PCT_OWNER_HH_CHILD + z_ACS_MEDIAN_HOME_VALUE + z_hsg_rate + z_sc_percent + z_ms_average + z_ACS_PCT_NO_PC + z_ACS_PCT_SMARTPHONE + z_pphd_avgnum + z_pmhd_avgnum + z_lbw_percent_lbw + z_pi_phy_inact + z_cip_perc + z_csph_perc + z_ACS_PCT_CHILD_DISAB + z_phys_per_cap + z_mhp_per_cap + z_dents_per_cap + z_uninsur_perc + z_uc_percent + z_opcp_rate + z_TeenBirths + z_phs_rate + z_ms_perc_ann + z_AMFAR_AMATFAC + z_MP_PERCPEN + z_AHRF_RURAL_H_CLINIC + z_unemp_perc + z_ACS_GINI_INDEX + z_ACS_MEDIAN_HH_INCOME + z_ACS_PCT_WORK_NO_CAR + z_soc_assoc_num + z_aeo_perc_w_acc + z_dwv_presence + z_ACS_PCT_HH_PUB_ASSIST + z_AHRF_TXC_SITE_NO_CNTRL, data = Z_and_Final_Variables_10.28.22)
summary(z_bb_levels_12345678)
confint(z_bb_levels_12345678)
vif(z_bb_levels_12345678)
