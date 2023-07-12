#7.12.23
#To do: To address VIF issues, delete three IVs from all the previous models -- z_SVI_RPL_THEMES_ALL, z_ACS_PER_CAPITA_INCOME, z_pfh_percent -- and then rerun the models
#Full models to rerun: education hsg, education sc, health lbw, health pud, health mud, housing rent, housing median, housing owned

library(tidyverse)
library(readxl)
library(car) #for vif analysis

#importing the dataset with the raw and z variables (created the z-scores in R last fall)
Z_and_Final_Variables_10.28.22 <- read.csv("~/Desktop/R Directory/ResiliencyProject/Z_and_Final_Variables_10.28.22.csv")

dim(Z_and_Final_Variables_10.28.22)
#3141 rows, 129 columns

summary(Z_and_Final_Variables_10.28.22)

#Modeling
options(scipen = 999)

#Final Education Model
#DV: High School Rate
#IVs: Demographics, Food/Housing, Technology, Health 1, (DOES NOT INCLUDE health 2), HWCC, Business, Government 

z_hsg_levels_1234678 <- lm(formula = z_hsg_rate ~ z_ACS_PCT_BLACK + z_ACS_PCT_HISPAN + z_SVI_RPL_THEME1_SOCIECO + z_SVI_RPL_THEME2_HH_DISB + z_SVI_RPL_THEME3_MINO + z_SVI_RPL_THEME4_HH_TRANS + z_ACS_PCT_ENGL_NOT_WELL + z_ACS_PCT_NON_CITIZEN + z_ACS_MEDIAN_AGE + z_food_afford + z_fd_enviro_indx + z_fi_percent + z_ACS_PCT_FOOD_STAMP + z_childelig_lunch_perc + z_shp_percent + z_shcb_percent + z_ACS_PCT_NO_VEH + z_ACS_PCT_RENTER_HH_CHILD + z_ACS_PCT_RENT_COST_30PCT + z_ACS_PCT_MOBILE_HOME + z_ACS_PCT_OWNED_HH + z_ACS_PCT_OWNER_HH_CHILD + z_ACS_MEDIAN_HOME_VALUE + z_ACS_PCT_BROADBAND + z_ACS_PCT_NO_PC + z_ACS_PCT_SMARTPHONE + z_phys_per_cap + z_mhp_per_cap + z_dents_per_cap + z_uninsur_perc + z_uc_percent + z_opcp_rate + z_TeenBirths + z_pphd_avgnum + z_pmhd_avgnum + z_lbw_percent_lbw + z_pi_phy_inact + z_cip_perc + z_csph_perc + z_ACS_PCT_CHILD_DISAB + z_unemp_perc + z_ACS_GINI_INDEX + z_ACS_MEDIAN_HH_INCOME + z_ACS_PCT_WORK_NO_CAR + z_soc_assoc_num + z_aeo_perc_w_acc + z_dwv_presence + z_ACS_PCT_HH_PUB_ASSIST + z_AHRF_TXC_SITE_NO_CNTRL, data = Z_and_Final_Variables_10.28.22) 

summary(z_hsg_levels_1234678)
confint(z_hsg_levels_1234678)
vif(z_hsg_levels_1234678)

#Final Education Model
#DV: Some College
#IVs: Demographics, Food/Housing, Technology, Health 1, Health 2, HWCC, Business, Government 

z_sc_levels_12345678 <- lm(formula = z_sc_percent ~ z_ACS_PCT_BLACK + z_ACS_PCT_HISPAN + z_SVI_RPL_THEME1_SOCIECO + z_SVI_RPL_THEME2_HH_DISB + z_SVI_RPL_THEME3_MINO + z_SVI_RPL_THEME4_HH_TRANS + z_ACS_PCT_ENGL_NOT_WELL + z_ACS_PCT_NON_CITIZEN + z_ACS_MEDIAN_AGE + z_food_afford + z_fd_enviro_indx + z_fi_percent + z_ACS_PCT_FOOD_STAMP + z_childelig_lunch_perc + z_shp_percent + z_shcb_percent + z_ACS_PCT_NO_VEH + z_ACS_PCT_RENTER_HH_CHILD + z_ACS_PCT_RENT_COST_30PCT + z_ACS_PCT_MOBILE_HOME + z_ACS_PCT_OWNED_HH + z_ACS_PCT_OWNER_HH_CHILD + z_ACS_MEDIAN_HOME_VALUE + z_ACS_PCT_BROADBAND + z_ACS_PCT_NO_PC + z_ACS_PCT_SMARTPHONE + z_phys_per_cap + z_mhp_per_cap + z_dents_per_cap + z_uninsur_perc + z_uc_percent + z_opcp_rate + z_TeenBirths + z_phs_rate + z_ms_perc_ann + z_AMFAR_AMATFAC + z_MP_PERCPEN + z_AHRF_RURAL_H_CLINIC + z_pphd_avgnum + z_pmhd_avgnum + z_lbw_percent_lbw + z_pi_phy_inact + z_cip_perc + z_csph_perc + z_ACS_PCT_CHILD_DISAB + z_unemp_perc + z_ACS_GINI_INDEX + z_ACS_MEDIAN_HH_INCOME + z_ACS_PCT_WORK_NO_CAR + z_soc_assoc_num + z_aeo_perc_w_acc + z_dwv_presence + z_ACS_PCT_HH_PUB_ASSIST + z_AHRF_TXC_SITE_NO_CNTRL, data = Z_and_Final_Variables_10.28.22) 

summary(z_sc_levels_12345678)
confint(z_sc_levels_12345678)
vif(z_sc_levels_12345678)

#Final Health Model
#DV: Low Birth Weight
#IVs: Demographics, Health 1, Health 2, Food/Housing, Technology, HWCC, Business, Government

z_lbw_levels_12345678 <- lm(formula = z_lbw_percent_lbw ~ z_ACS_PCT_BLACK + z_ACS_PCT_HISPAN + z_SVI_RPL_THEME1_SOCIECO + z_SVI_RPL_THEME2_HH_DISB + z_SVI_RPL_THEME3_MINO + z_SVI_RPL_THEME4_HH_TRANS + z_ACS_PCT_ENGL_NOT_WELL + z_ACS_PCT_NON_CITIZEN + z_ACS_MEDIAN_AGE + z_phys_per_cap + z_mhp_per_cap + z_dents_per_cap + z_uninsur_perc + z_uc_percent + z_opcp_rate + z_TeenBirths + z_phs_rate + z_ms_perc_ann + z_AMFAR_AMATFAC + z_MP_PERCPEN + z_AHRF_RURAL_H_CLINIC + z_food_afford + z_fd_enviro_indx + z_fi_percent + z_ACS_PCT_FOOD_STAMP + z_childelig_lunch_perc + z_shp_percent + z_shcb_percent + z_ACS_PCT_NO_VEH + z_ACS_PCT_RENTER_HH_CHILD + z_ACS_PCT_RENT_COST_30PCT + z_ACS_PCT_MOBILE_HOME + z_ACS_PCT_OWNED_HH + z_ACS_PCT_OWNER_HH_CHILD + z_ACS_MEDIAN_HOME_VALUE + z_ACS_PCT_BROADBAND + z_ACS_PCT_NO_PC + z_ACS_PCT_SMARTPHONE + z_pi_phy_inact + z_cip_perc + z_csph_perc + z_ACS_PCT_CHILD_DISAB + z_unemp_perc + z_ACS_GINI_INDEX + z_ACS_MEDIAN_HH_INCOME + z_ACS_PCT_WORK_NO_CAR + z_soc_assoc_num + z_aeo_perc_w_acc + z_dwv_presence + z_ACS_PCT_HH_PUB_ASSIST + z_AHRF_TXC_SITE_NO_CNTRL, data = Z_and_Final_Variables_10.28.22) 

summary(z_lbw_levels_12345678)
confint(z_lbw_levels_12345678)
vif(z_lbw_levels_12345678)

#Final Health Model
#DV: Physically Unhealthy Days
#IVs: Demographics, Health 1, Health 2, Food/Housing, Technology, HWCC, Business, Government
#Note: z_pfh_percent was previously excluded from the final physically unhealthy days model

z_pphd_levels_12345678 <- lm(formula = z_pphd_avgnum ~ z_ACS_PCT_BLACK + z_ACS_PCT_HISPAN + z_SVI_RPL_THEME1_SOCIECO + z_SVI_RPL_THEME2_HH_DISB + z_SVI_RPL_THEME3_MINO + z_SVI_RPL_THEME4_HH_TRANS + z_ACS_PCT_ENGL_NOT_WELL + z_ACS_PCT_NON_CITIZEN + z_ACS_MEDIAN_AGE + z_phys_per_cap + z_mhp_per_cap + z_dents_per_cap + z_uninsur_perc + z_uc_percent + z_opcp_rate + z_TeenBirths + z_phs_rate + z_ms_perc_ann + z_AMFAR_AMATFAC + z_MP_PERCPEN + z_AHRF_RURAL_H_CLINIC + z_food_afford + z_fd_enviro_indx + z_fi_percent + z_ACS_PCT_FOOD_STAMP + z_childelig_lunch_perc + z_shp_percent + z_shcb_percent + z_ACS_PCT_NO_VEH + z_ACS_PCT_RENTER_HH_CHILD + z_ACS_PCT_RENT_COST_30PCT + z_ACS_PCT_MOBILE_HOME + z_ACS_PCT_OWNED_HH + z_ACS_PCT_OWNER_HH_CHILD + z_ACS_MEDIAN_HOME_VALUE + z_ACS_PCT_BROADBAND + z_ACS_PCT_NO_PC + z_ACS_PCT_SMARTPHONE + z_pi_phy_inact + z_cip_perc + z_csph_perc + z_ACS_PCT_CHILD_DISAB + z_unemp_perc + z_ACS_GINI_INDEX + z_ACS_MEDIAN_HH_INCOME + z_ACS_PCT_WORK_NO_CAR + z_soc_assoc_num + z_aeo_perc_w_acc + z_dwv_presence + z_ACS_PCT_HH_PUB_ASSIST + z_AHRF_TXC_SITE_NO_CNTRL, data = Z_and_Final_Variables_10.28.22) 

summary(z_pphd_levels_12345678)
confint(z_pphd_levels_12345678)
vif(z_pphd_levels_12345678)

#Final Health Model
#DV: Mentally Unhealthy Days
#IVs: Demographics, Health 1, Health 2, Food/Housing, Technology, HWCC, Business, Government
#Note: z_pfh_percent was previously excluded from the final mentally unhealthy days model

z_pmhd_levels_12345678 <- lm(formula = z_pmhd_avgnum ~ z_ACS_PCT_BLACK + z_ACS_PCT_HISPAN + z_SVI_RPL_THEME1_SOCIECO + z_SVI_RPL_THEME2_HH_DISB + z_SVI_RPL_THEME3_MINO + z_SVI_RPL_THEME4_HH_TRANS + z_ACS_PCT_ENGL_NOT_WELL + z_ACS_PCT_NON_CITIZEN + z_ACS_MEDIAN_AGE + z_phys_per_cap + z_mhp_per_cap + z_dents_per_cap + z_uninsur_perc + z_uc_percent + z_opcp_rate + z_TeenBirths + z_phs_rate + z_ms_perc_ann + z_AMFAR_AMATFAC + z_MP_PERCPEN + z_AHRF_RURAL_H_CLINIC + z_food_afford + z_fd_enviro_indx + z_fi_percent + z_ACS_PCT_FOOD_STAMP + z_childelig_lunch_perc + z_shp_percent + z_shcb_percent + z_ACS_PCT_NO_VEH + z_ACS_PCT_RENTER_HH_CHILD + z_ACS_PCT_RENT_COST_30PCT + z_ACS_PCT_MOBILE_HOME + z_ACS_PCT_OWNED_HH + z_ACS_PCT_OWNER_HH_CHILD + z_ACS_MEDIAN_HOME_VALUE + z_ACS_PCT_BROADBAND + z_ACS_PCT_NO_PC + z_ACS_PCT_SMARTPHONE + z_pi_phy_inact + z_cip_perc + z_csph_perc + z_ACS_PCT_CHILD_DISAB + z_unemp_perc + z_ACS_GINI_INDEX + z_ACS_MEDIAN_HH_INCOME + z_ACS_PCT_WORK_NO_CAR + z_soc_assoc_num + z_aeo_perc_w_acc + z_dwv_presence + z_ACS_PCT_HH_PUB_ASSIST + z_AHRF_TXC_SITE_NO_CNTRL, data = Z_and_Final_Variables_10.28.22) 

summary(z_pmhd_levels_12345678)
confint(z_pmhd_levels_12345678)
vif(z_pmhd_levels_12345678)

#Final Housing Model
#DV: Rent 30 Percent
#IVs: All IVs except those that serve as the DVs in the housing models -- z_ACS_PCT_RENT_COST_30PCT, z_ACS_MEDIAN_HOME_VALUE, z_ACS_PCT_OWNED_HH -- as well as the three problematic VIF IVs
#Note: Building final model below (R markdown housing doc had groupings only)

z_rent30_levels_12345678 <-lm(formula = z_ACS_PCT_RENT_COST_30PCT ~ z_ACS_PCT_BLACK + z_ACS_PCT_HISPAN + z_SVI_RPL_THEME1_SOCIECO + z_SVI_RPL_THEME2_HH_DISB + z_SVI_RPL_THEME3_MINO + z_SVI_RPL_THEME4_HH_TRANS + z_ACS_PCT_ENGL_NOT_WELL + z_ACS_PCT_NON_CITIZEN + z_ACS_MEDIAN_AGE + z_food_afford + z_fd_enviro_indx + z_fi_percent + z_ACS_PCT_FOOD_STAMP + z_childelig_lunch_perc + z_shp_percent + z_shcb_percent + z_ACS_PCT_NO_VEH + z_ACS_PCT_RENTER_HH_CHILD + z_ACS_PCT_MOBILE_HOME + z_ACS_PCT_OWNER_HH_CHILD + z_ACS_PCT_BROADBAND + z_ACS_PCT_NO_PC + z_ACS_PCT_SMARTPHONE + z_phys_per_cap + z_mhp_per_cap + z_dents_per_cap + z_uninsur_perc + z_uc_percent + z_opcp_rate + z_TeenBirths + z_phs_rate + z_ms_perc_ann + z_AMFAR_AMATFAC + z_MP_PERCPEN + z_AHRF_RURAL_H_CLINIC + z_pphd_avgnum + z_pmhd_avgnum + z_lbw_percent_lbw + z_pi_phy_inact + z_cip_perc + z_csph_perc + z_ACS_PCT_CHILD_DISAB + z_unemp_perc + z_ACS_GINI_INDEX + z_ACS_MEDIAN_HH_INCOME + z_ACS_PCT_WORK_NO_CAR + z_soc_assoc_num + z_aeo_perc_w_acc + z_dwv_presence + z_ACS_PCT_HH_PUB_ASSIST + z_AHRF_TXC_SITE_NO_CNTRL, data = Z_and_Final_Variables_10.28.22) 

summary(z_rent30_levels_12345678)
confint(z_rent30_levels_12345678)
vif(z_rent30_levels_12345678)

#Final Housing Model
#DV: Median Home Value
#IVs: All IVs except those that serve as the DVs in the housing models -- z_ACS_PCT_RENT_COST_30PCT, z_ACS_MEDIAN_HOME_VALUE, z_ACS_PCT_OWNED_HH -- as well as the three problematic VIF IVs
#Note: Building final model below (R markdown housing doc had groupings only)

z_median_levels_12345678 <-lm(formula = z_ACS_MEDIAN_HOME_VALUE ~ z_ACS_PCT_BLACK + z_ACS_PCT_HISPAN + z_SVI_RPL_THEME1_SOCIECO + z_SVI_RPL_THEME2_HH_DISB + z_SVI_RPL_THEME3_MINO + z_SVI_RPL_THEME4_HH_TRANS + z_ACS_PCT_ENGL_NOT_WELL + z_ACS_PCT_NON_CITIZEN + z_ACS_MEDIAN_AGE + z_food_afford + z_fd_enviro_indx + z_fi_percent + z_ACS_PCT_FOOD_STAMP + z_childelig_lunch_perc + z_shp_percent + z_shcb_percent + z_ACS_PCT_NO_VEH + z_ACS_PCT_RENTER_HH_CHILD + z_ACS_PCT_MOBILE_HOME + z_ACS_PCT_OWNER_HH_CHILD + z_ACS_PCT_BROADBAND + z_ACS_PCT_NO_PC + z_ACS_PCT_SMARTPHONE + z_phys_per_cap + z_mhp_per_cap + z_dents_per_cap + z_uninsur_perc + z_uc_percent + z_opcp_rate + z_TeenBirths + z_phs_rate + z_ms_perc_ann + z_AMFAR_AMATFAC + z_MP_PERCPEN + z_AHRF_RURAL_H_CLINIC + z_pphd_avgnum + z_pmhd_avgnum + z_lbw_percent_lbw + z_pi_phy_inact + z_cip_perc + z_csph_perc + z_ACS_PCT_CHILD_DISAB + z_unemp_perc + z_ACS_GINI_INDEX + z_ACS_MEDIAN_HH_INCOME + z_ACS_PCT_WORK_NO_CAR + z_soc_assoc_num + z_aeo_perc_w_acc + z_dwv_presence + z_ACS_PCT_HH_PUB_ASSIST + z_AHRF_TXC_SITE_NO_CNTRL, data = Z_and_Final_Variables_10.28.22) 

summary(z_median_levels_12345678)
confint(z_median_levels_12345678)
vif(z_median_levels_12345678)

#Final Housing Model
#DV: Owned HH
#IVs: All IVs except those that serve as the DVs in the housing models -- z_ACS_PCT_RENT_COST_30PCT, z_ACS_MEDIAN_HOME_VALUE, z_ACS_PCT_OWNED_HH -- as well as the three problematic VIF IVs
#Note: Building final model below (R markdown housing doc had groupings only)

z_owned_levels_12345678 <-lm(formula = z_ACS_PCT_OWNED_HH ~ z_ACS_PCT_BLACK + z_ACS_PCT_HISPAN + z_SVI_RPL_THEME1_SOCIECO + z_SVI_RPL_THEME2_HH_DISB + z_SVI_RPL_THEME3_MINO + z_SVI_RPL_THEME4_HH_TRANS + z_ACS_PCT_ENGL_NOT_WELL + z_ACS_PCT_NON_CITIZEN + z_ACS_MEDIAN_AGE + z_food_afford + z_fd_enviro_indx + z_fi_percent + z_ACS_PCT_FOOD_STAMP + z_childelig_lunch_perc + z_shp_percent + z_shcb_percent + z_ACS_PCT_NO_VEH + z_ACS_PCT_RENTER_HH_CHILD + z_ACS_PCT_MOBILE_HOME + z_ACS_PCT_OWNER_HH_CHILD + z_ACS_PCT_BROADBAND + z_ACS_PCT_NO_PC + z_ACS_PCT_SMARTPHONE + z_phys_per_cap + z_mhp_per_cap + z_dents_per_cap + z_uninsur_perc + z_uc_percent + z_opcp_rate + z_TeenBirths + z_phs_rate + z_ms_perc_ann + z_AMFAR_AMATFAC + z_MP_PERCPEN + z_AHRF_RURAL_H_CLINIC + z_pphd_avgnum + z_pmhd_avgnum + z_lbw_percent_lbw + z_pi_phy_inact + z_cip_perc + z_csph_perc + z_ACS_PCT_CHILD_DISAB + z_unemp_perc + z_ACS_GINI_INDEX + z_ACS_MEDIAN_HH_INCOME + z_ACS_PCT_WORK_NO_CAR + z_soc_assoc_num + z_aeo_perc_w_acc + z_dwv_presence + z_ACS_PCT_HH_PUB_ASSIST + z_AHRF_TXC_SITE_NO_CNTRL, data = Z_and_Final_Variables_10.28.22) 

summary(z_owned_levels_12345678)
confint(z_owned_levels_12345678)
vif(z_owned_levels_12345678)
