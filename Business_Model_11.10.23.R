#11.10.23
#Business Model

library(tidyverse)
library(readxl)
library(car) #for vif analysis
options(scipen = 999)

#importing the dataset with the raw and z variables (created the z-scores in R last fall)
Z_and_Final_Variables_10.28.22 <- read.csv("~/Desktop/R Directory/ResiliencyProject/Z_and_Final_Variables_10.28.22.csv")

#DV 1: GINI
ggplot(data = Z_and_Final_Variables_10.28.22, mapping = aes(x = z_ACS_GINI_INDEX)) + geom_histogram() 

#DV 2: Unemploy Perc
ggplot(data = Z_and_Final_Variables_10.28.22, mapping = aes(x = z_unemp_perc)) + geom_histogram() 

#just checking the RAW variables
unique(Z_and_Final_Variables_10.28.22$ACS_GINI_INDEX)
summary(Z_and_Final_Variables_10.28.22$ACS_GINI_INDEX) #1 NA
#Where is that NA? 
Z_and_Final_Variables_10.28.22 %>%
  filter(is.na(z_ACS_GINI_INDEX)) #it's FIPS 35039, New Mexico, Rio Arriba county

unique(Z_and_Final_Variables_10.28.22$unemp_perc)
summary(Z_and_Final_Variables_10.28.22$unemp_perc) #few high (max: 19.9, mean: 4.1, Median 3.8); #1 NA
#Where is that NA? 
Z_and_Final_Variables_10.28.22 %>%
  filter(is.na(z_unemp_perc)) #it's FIPS 15005, Hawaii, Kalawao county

# Business Model: Gini Index (DV)

## IV: Demographics (9 variables), DV: Gini Index
z_gi_just_demos <- lm(formula = z_ACS_GINI_INDEX ~ z_ACS_PCT_BLACK + z_ACS_PCT_HISPAN + z_SVI_RPL_THEME1_SOCIECO + z_SVI_RPL_THEME2_HH_DISB + z_SVI_RPL_THEME3_MINO + z_SVI_RPL_THEME4_HH_TRANS + z_ACS_PCT_ENGL_NOT_WELL + z_ACS_PCT_NON_CITIZEN + z_ACS_MEDIAN_AGE, data = Z_and_Final_Variables_10.28.22) 

summary(z_gi_just_demos)

## Confidence Intervals for Demographic IVs
confint(z_gi_just_demos)

## VIF for Demographic IVs
vif(z_gi_just_demos)

## IV: Food & Housing (14 variables), DV: Gini Index
z_gi_just_foodhouse <- lm(formula = z_ACS_GINI_INDEX ~ z_food_afford + z_fd_enviro_indx + z_fi_percent + z_ACS_PCT_FOOD_STAMP + z_childelig_lunch_perc + z_shp_percent + z_shcb_percent + z_ACS_PCT_NO_VEH + z_ACS_PCT_RENTER_HH_CHILD + z_ACS_PCT_RENT_COST_30PCT + z_ACS_PCT_MOBILE_HOME + z_ACS_PCT_OWNED_HH + z_ACS_PCT_OWNER_HH_CHILD + z_ACS_MEDIAN_HOME_VALUE, data = Z_and_Final_Variables_10.28.22)

summary(z_gi_just_foodhouse)

## Confidence Intervals for Food & Housing IVs
confint(z_gi_just_foodhouse)

## VIF for Food & Housing IVs
vif(z_gi_just_foodhouse)

## IV: Education (3 variables), DV: Gini Index
z_gi_just_ed <- lm(formula = z_ACS_GINI_INDEX ~ z_hsg_rate + z_sc_percent + z_ms_average, data = Z_and_Final_Variables_10.28.22)

summary(z_gi_just_ed)

## Confidence Intervals for Education IVs
confint(z_gi_just_ed)

## VIF for Education IVs
vif(z_gi_just_ed)

## IV: Technology (3 variables), DV: Gini Index
z_gi_just_tech <- lm(formula = z_ACS_GINI_INDEX ~ z_ACS_PCT_NO_PC + z_ACS_PCT_SMARTPHONE + z_ACS_PCT_BROADBAND, data = Z_and_Final_Variables_10.28.22)

summary(z_gi_just_tech)

## Confidence Intervals for Technology IVs
confint(z_gi_just_tech)

## VIF for Technology IVs
vif(z_gi_just_tech)

## IV: HWCC (7 variables), DV: Gini Index
z_gi_just_HWCC <- lm(formula = z_ACS_GINI_INDEX ~ z_pphd_avgnum + z_pmhd_avgnum + z_lbw_percent_lbw + z_pi_phy_inact + z_cip_perc + z_csph_perc + z_ACS_PCT_CHILD_DISAB, data = Z_and_Final_Variables_10.28.22)

summary(z_gi_just_HWCC)

## Confidence Intervals for HWCC IVs
confint(z_gi_just_HWCC)

## VIF for HWCC IVs
vif(z_gi_just_HWCC)

## IV: Health A (6 variables), DV: Gini Index
z_gi_just_healthA <- lm(formula = z_ACS_GINI_INDEX ~ z_phys_per_cap + z_mhp_per_cap + z_dents_per_cap + z_uninsur_perc + z_opcp_rate + z_TeenBirths, data = Z_and_Final_Variables_10.28.22)

summary(z_gi_just_healthA)

## Confidence Intervals for Health A IVs
confint(z_gi_just_healthA)

## VIF for Health A IVs
vif(z_gi_just_healthA)

## IV: Health B (5 variables), DV: Gini Index
z_gi_just_healthB <- lm(formula = z_ACS_GINI_INDEX ~ z_phs_rate + z_ms_perc_ann + z_AMFAR_AMATFAC + z_MP_PERCPEN + z_AHRF_RURAL_H_CLINIC, data = Z_and_Final_Variables_10.28.22)

summary(z_gi_just_healthB)

## Confidence Intervals for Health B IVs
confint(z_gi_just_healthB)

## VIF for Health B IVs
vif(z_gi_just_healthB)

## IV: Business (2 variables), DV: Gini Index
## NOTE: both DVs for Business (Gini and unemp_perc) have been removed as IVs here and in the final Business model
z_gi_just_bus <- lm(formula = z_ACS_GINI_INDEX ~ z_ACS_MEDIAN_HH_INCOME + z_ACS_PCT_WORK_NO_CAR, data = Z_and_Final_Variables_10.28.22)

summary(z_gi_just_bus)

## Confidence Intervals for Business IVs
confint(z_gi_just_bus)

## VIF for Business IVs
vif(z_gi_just_bus)

## IV: Government (5 variables), DV: Gini Index
z_gi_just_gov <- lm(formula = z_ACS_GINI_INDEX ~ z_soc_assoc_num + z_aeo_perc_w_acc + z_dwv_presence + z_ACS_PCT_HH_PUB_ASSIST + z_AHRF_TXC_SITE_NO_CNTRL, data = Z_and_Final_Variables_10.28.22)

summary(z_gi_just_gov)

## Confidence Intervals for Government IVs
confint(z_gi_just_gov)

## VIF for Government IVs
vif(z_gi_just_gov)

# Final Government Model, DV: Gini Index; IVs: 54

z_gi_levels_12345678 <- lm(formula = z_ACS_GINI_INDEX ~ z_ACS_PCT_BLACK + z_ACS_PCT_HISPAN + z_SVI_RPL_THEME1_SOCIECO + z_SVI_RPL_THEME2_HH_DISB + z_SVI_RPL_THEME3_MINO + z_SVI_RPL_THEME4_HH_TRANS + z_ACS_PCT_ENGL_NOT_WELL + z_ACS_PCT_NON_CITIZEN + z_ACS_MEDIAN_AGE + z_food_afford + z_fd_enviro_indx + z_fi_percent + z_ACS_PCT_FOOD_STAMP + z_childelig_lunch_perc + z_shp_percent + z_shcb_percent + z_ACS_PCT_NO_VEH + z_ACS_PCT_RENTER_HH_CHILD + z_ACS_PCT_RENT_COST_30PCT + z_ACS_PCT_MOBILE_HOME + z_ACS_PCT_OWNED_HH + z_ACS_PCT_OWNER_HH_CHILD + z_ACS_MEDIAN_HOME_VALUE + z_hsg_rate + z_sc_percent + z_ms_average + z_ACS_PCT_NO_PC + z_ACS_PCT_SMARTPHONE + z_ACS_PCT_BROADBAND + z_pphd_avgnum + z_pmhd_avgnum + z_lbw_percent_lbw + z_pi_phy_inact + z_cip_perc + z_csph_perc + z_ACS_PCT_CHILD_DISAB + z_phys_per_cap + z_mhp_per_cap + z_dents_per_cap + z_uninsur_perc + z_opcp_rate + z_TeenBirths + z_phs_rate + z_ms_perc_ann + z_AMFAR_AMATFAC + z_MP_PERCPEN + z_AHRF_RURAL_H_CLINIC + z_ACS_MEDIAN_HH_INCOME + z_ACS_PCT_WORK_NO_CAR + z_soc_assoc_num + z_aeo_perc_w_acc + z_dwv_presence + z_ACS_PCT_HH_PUB_ASSIST + z_AHRF_TXC_SITE_NO_CNTRL, data = Z_and_Final_Variables_10.28.22)

summary(z_gi_levels_12345678)

## Confidence Intervals for Final Model
confint(z_gi_levels_12345678)

## VIF for Final Model
vif(z_gi_levels_12345678)

#12.11.23
#Business Model - Adding 2nd DV: z_unemp_perc

# Business Model: Unemp Perc (DV)

## IV: Demographics (9 variables), DV: Unemp Perc
z_up_just_demos <- lm(formula = z_unemp_perc ~ z_ACS_PCT_BLACK + z_ACS_PCT_HISPAN + z_SVI_RPL_THEME1_SOCIECO + z_SVI_RPL_THEME2_HH_DISB + z_SVI_RPL_THEME3_MINO + z_SVI_RPL_THEME4_HH_TRANS + z_ACS_PCT_ENGL_NOT_WELL + z_ACS_PCT_NON_CITIZEN + z_ACS_MEDIAN_AGE, data = Z_and_Final_Variables_10.28.22) 

summary(z_up_just_demos)

## Confidence Intervals for Demographic IVs
confint(z_up_just_demos)

## VIF for Demographic IVs
vif(z_up_just_demos)

## IV: Food & Housing (14 variables), DV: Unemp Perc
z_up_just_foodhouse <- lm(formula = z_unemp_perc ~ z_food_afford + z_fd_enviro_indx + z_fi_percent + z_ACS_PCT_FOOD_STAMP + z_childelig_lunch_perc + z_shp_percent + z_shcb_percent + z_ACS_PCT_NO_VEH + z_ACS_PCT_RENTER_HH_CHILD + z_ACS_PCT_RENT_COST_30PCT + z_ACS_PCT_MOBILE_HOME + z_ACS_PCT_OWNED_HH + z_ACS_PCT_OWNER_HH_CHILD + z_ACS_MEDIAN_HOME_VALUE, data = Z_and_Final_Variables_10.28.22)

summary(z_up_just_foodhouse)

## Confidence Intervals for Food & Housing IVs
confint(z_up_just_foodhouse)

## VIF for Food & Housing IVs
vif(z_up_just_foodhouse)

## IV: Education (3 variables), DV: Unemp Perc
z_up_just_ed <- lm(formula = z_unemp_perc ~ z_hsg_rate + z_sc_percent + z_ms_average, data = Z_and_Final_Variables_10.28.22)

summary(z_up_just_ed)

## Confidence Intervals for Education IVs
confint(z_up_just_ed)

## VIF for Education IVs
vif(z_up_just_ed)

## IV: Technology (3 variables), DV: Unemp Perc
z_up_just_tech <- lm(formula = z_unemp_perc ~ z_ACS_PCT_NO_PC + z_ACS_PCT_SMARTPHONE + z_ACS_PCT_BROADBAND, data = Z_and_Final_Variables_10.28.22)

summary(z_up_just_tech)

## Confidence Intervals for Technology IVs
confint(z_up_just_tech)

## VIF for Technology IVs
vif(z_up_just_tech)

## IV: HWCC (7 variables), DV: Unemp Perc
z_up_just_HWCC <- lm(formula = z_unemp_perc ~ z_pphd_avgnum + z_pmhd_avgnum + z_lbw_percent_lbw + z_pi_phy_inact + z_cip_perc + z_csph_perc + z_ACS_PCT_CHILD_DISAB, data = Z_and_Final_Variables_10.28.22)

summary(z_up_just_HWCC)

## Confidence Intervals for HWCC IVs
confint(z_up_just_HWCC)

## VIF for HWCC IVs
vif(z_up_just_HWCC)

## IV: Health A (6 variables), DV: Unemp Perc
z_up_just_healthA <- lm(formula = z_unemp_perc ~ z_phys_per_cap + z_mhp_per_cap + z_dents_per_cap + z_uninsur_perc + z_opcp_rate + z_TeenBirths, data = Z_and_Final_Variables_10.28.22)

summary(z_up_just_healthA)

## Confidence Intervals for Health A IVs
confint(z_up_just_healthA)

## VIF for Health A IVs
vif(z_up_just_healthA)

## IV: Health B (5 variables), DV: Unemp Perc
z_up_just_healthB <- lm(formula = z_unemp_perc ~ z_phs_rate + z_ms_perc_ann + z_AMFAR_AMATFAC + z_MP_PERCPEN + z_AHRF_RURAL_H_CLINIC, data = Z_and_Final_Variables_10.28.22)

summary(z_up_just_healthB)

## Confidence Intervals for Health B IVs
confint(z_up_just_healthB)

## VIF for Health B IVs
vif(z_up_just_healthB)

## IV: Business (2 variables), DV: Unemp Perc
## NOTE: both DVs for Business (Gini and unemp_perc) have been removed as IVs here and in the final Business model
z_up_just_bus <- lm(formula = z_unemp_perc ~ z_ACS_MEDIAN_HH_INCOME + z_ACS_PCT_WORK_NO_CAR, data = Z_and_Final_Variables_10.28.22)

summary(z_up_just_bus)

## Confidence Intervals for Business IVs
confint(z_up_just_bus)

## VIF for Business IVs
vif(z_up_just_bus)

## IV: Government (5 variables), DV: Unemp Perc
z_up_just_gov <- lm(formula = z_unemp_perc ~ z_soc_assoc_num + z_aeo_perc_w_acc + z_dwv_presence + z_ACS_PCT_HH_PUB_ASSIST + z_AHRF_TXC_SITE_NO_CNTRL, data = Z_and_Final_Variables_10.28.22)

summary(z_up_just_gov)

## Confidence Intervals for Government IVs
confint(z_up_just_gov)

## VIF for Government IVs
vif(z_up_just_gov)

# Final Government Model, DV: Unemp Perc; IVs: 54

z_up_levels_12345678 <- lm(formula = z_unemp_perc ~ z_ACS_PCT_BLACK + z_ACS_PCT_HISPAN + z_SVI_RPL_THEME1_SOCIECO + z_SVI_RPL_THEME2_HH_DISB + z_SVI_RPL_THEME3_MINO + z_SVI_RPL_THEME4_HH_TRANS + z_ACS_PCT_ENGL_NOT_WELL + z_ACS_PCT_NON_CITIZEN + z_ACS_MEDIAN_AGE + z_food_afford + z_fd_enviro_indx + z_fi_percent + z_ACS_PCT_FOOD_STAMP + z_childelig_lunch_perc + z_shp_percent + z_shcb_percent + z_ACS_PCT_NO_VEH + z_ACS_PCT_RENTER_HH_CHILD + z_ACS_PCT_RENT_COST_30PCT + z_ACS_PCT_MOBILE_HOME + z_ACS_PCT_OWNED_HH + z_ACS_PCT_OWNER_HH_CHILD + z_ACS_MEDIAN_HOME_VALUE + z_hsg_rate + z_sc_percent + z_ms_average + z_ACS_PCT_NO_PC + z_ACS_PCT_SMARTPHONE + z_ACS_PCT_BROADBAND + z_pphd_avgnum + z_pmhd_avgnum + z_lbw_percent_lbw + z_pi_phy_inact + z_cip_perc + z_csph_perc + z_ACS_PCT_CHILD_DISAB + z_phys_per_cap + z_mhp_per_cap + z_dents_per_cap + z_uninsur_perc + z_opcp_rate + z_TeenBirths + z_phs_rate + z_ms_perc_ann + z_AMFAR_AMATFAC + z_MP_PERCPEN + z_AHRF_RURAL_H_CLINIC + z_ACS_MEDIAN_HH_INCOME + z_ACS_PCT_WORK_NO_CAR + z_soc_assoc_num + z_aeo_perc_w_acc + z_dwv_presence + z_ACS_PCT_HH_PUB_ASSIST + z_AHRF_TXC_SITE_NO_CNTRL, data = Z_and_Final_Variables_10.28.22)

summary(z_up_levels_12345678)

## Confidence Intervals for Final Model
confint(z_up_levels_12345678)

## VIF for Final Model
vif(z_up_levels_12345678)


