#Statistical analysis for Alistipes senegalensis_IPA/PPA_obesity manuscripts#
#******************************************************************************#
#Metagenomic analysis#
##Covariates statistics##
data<-read.csv('US_LC_metadata.csv')
cor.test(data$BMI,data$PFAT,method='spearman')
names(data)
mean(data$BMI)
sd(data$BMI)
mean(data$PFAT)
sd(data$PFAT)
BMI_data<-data[,-c(1,3)]
PFAT_data<-data[,-c(1,2)]
names(BMI_data)
names(PFAT_data)
table(data$Gender)
prop.table(table(data$Gender))
fit_gender_1<-lm(data$BMI~data$Gender)
summary(fit_gender_1)
fit_gender_2<-lm(data$PFAT~data$Gender)
summary(fit_gender_2)
table(data$Race)
prop.table(table(data$Race))
fit_race_1<-lm(data$BMI~data$Race)
summary(fit_race_1)
fit_race_2<-lm(data$PFAT~data$Race)
summary(fit_race_2)
mean(data$Age)
sd(data$Age)
fit_age_1<-lm(data$BMI~data$Age)
summary(fit_age_1)
fit_age_2<-lm(data$PFAT~data$Age)
summary(fit_age_2)
table(data$Smoke)
prop.table(table(data$Smoke))
fit_smoke_1<-lm(data$BMI~data$Smoke)
summary(fit_smoke_1)
fit_smoke_2<-lm(data$PFAT~data$Smoke)
summary(fit_smoke_2)
table(data$Alcohol_Drinking)
prop.table(table(data$Alcohol_Drinking))
fit_alcohol_1<-lm(data$BMI~data$Alcohol_Drinking)
summary(fit_alcohol_1)
fit_alcohol_2<-lm(data$PFAT~data$Alcohol_Drinking)
summary(fit_alcohol_2)
table(data$Taking_Antibiotics)
prop.table(table(data$Taking_Antibiotics))
fit_anti_1<-lm(data$BMI~data$Taking_Antibiotics)
summary(fit_anti_1)
fit_anti_2<-lm(data$PFAT~data$Taking_Antibiotics)
summary(fit_anti_2)
table(data$Taking_Probiotics)
prop.table(table(data$Taking_Probiotics))
fit_pro_1<-lm(data$BMI~data$Taking_Probiotics)
summary(fit_pro_1)
fit_pro_2<-lm(data$PFAT~data$Taking_Probiotics)
summary(fit_pro_2)
mean(data$Economic.level)
sd(data$Economic.level)
fit_eco_1<-lm(data$BMI~data$Economic.level)
summary(fit_eco_1)
fit_eco_2<-lm(data$PFAT~data$Economic.level)
summary(fit_eco_2)
table(data$Exercise_Regular)
prop.table(table(data$Exercise_Regular))
fit_exe_1<-lm(data$BMI~data$Exercise_Regular)
summary(fit_exe_1)
fit_exe_2<-lm(data$PFAT~data$Exercise_Regular)
summary(fit_exe_2)
mean(data$V_Total)
sd(data$V_Total)
fit_v_1<-lm(data$BMI~data$V_Total)
summary(fit_v_1)
fit_v_2<-lm(data$PFAT~data$V_Total)
summary(fit_v_2)
mean(data$F_Total)
sd(data$F_Total)
fit_f_1<-lm(data$BMI~data$F_Total)
summary(fit_f_1)
fit_f_2<-lm(data$PFAT~data$F_Total)
summary(fit_f_2)
mean(data$Steak)
sd(data$Steak)
fit_steak_1<-lm(data$BMI~data$Steak)
summary(fit_steak_1)
fit_steak_2<-lm(data$PFAT~data$Steak)
summary(fit_steak_2)
mean(data$Pork)
sd(data$Pork)
fit_pork_1<-lm(data$BMI~data$Pork)
summary(fit_pork_1)
fit_pork_2<-lm(data$PFAT~data$Pork)
summary(fit_pork_2)
mean(data$Fish.food)
sd(data$Fish.food)
fit_fish_1<-lm(data$BMI~data$Fish.food)
summary(fit_fish_1)
fit_fish_2<-lm(data$PFAT~data$Fish.food)
summary(fit_fish_2)
mean(data$Egg)
sd(data$Egg)
fit_egg_1<-lm(data$BMI~data$Egg)
summary(fit_egg_1)
fit_egg_2<-lm(data$PFAT~data$Egg)
summary(fit_egg_2)
table(data$Milk)
prop.table(table(data$Milk))
fit_milk_1<-lm(data$BMI~data$Milk)
summary(fit_milk_1)
fit_milk_2<-lm(data$PFAT~data$Milk)
summary(fit_milk_2)
table(data$Yogurt)
prop.table(table(data$Yogurt))
fit_yogurt_1<-lm(data$BMI~data$Yogurt)
summary(fit_yogurt_1)
fit_yogurt_2<-lm(data$PFAT~data$Yogurt)
summary(fit_yogurt_2)
table(data$Cheese)
prop.table(table(data$Cheese))
fit_cheese_1<-lm(data$BMI~data$Cheese)
summary(fit_cheese_1)
fit_cheese_2<-lm(data$PFAT~data$Cheese)
summary(fit_cheese_2)
mean(data$Cookies)
sd(data$Cookies)
fit_cookies_1<-lm(data$BMI~data$Cookies)
summary(fit_cookies_1)
fit_cookies_2<-lm(data$PFAT~data$Cookies)
summary(fit_cookies_2)
mean(data$Chocolate.candy)
sd(data$Chocolate.candy)
fit_cho_1<-lm(data$BMI~data$Chocolate.candy)
summary(fit_cho_1)
fit_cho_2<-lm(data$PFAT~data$Chocolate.candy)
summary(fit_cho_2)
mean(data$Cold.cereal)
sd(data$Cold.cereal)
fit_cold_1<-lm(data$BMI~data$Cold.cereal)
summary(fit_cold_1)
fit_cold_2<-lm(data$PFAT~data$Cold.cereal)
summary(fit_cold_2)
mean(data$Fiber.suplement)
sd(data$Fiber.suplement)
fit_fiber_1<-lm(data$BMI~data$Fiber.suplement)
summary(fit_fiber_1)
fit_fiber_2<-lm(data$PFAT~data$Fiber.suplement)
summary(fit_fiber_2)
mean(data$Water)
sd(data$Water)
fit_water_1<-lm(data$BMI~data$Water)
summary(fit_water_1)
fit_water_2<-lm(data$PFAT~data$Water)
summary(fit_water_2)
mean(data$Coffee)
sd(data$Coffee)
fit_coffee_1<-lm(data$BMI~data$Coffee)
summary(fit_coffee_1)
fit_coffee_2<-lm(data$PFAT~data$Coffee)
summary(fit_coffee_2)
mean(data$Hot.tea)
sd(data$Hot.tea)
fit_tea_1<-lm(data$BMI~data$Hot.tea)
summary(fit_tea_1)
fit_tea_2<-lm(data$PFAT~data$Hot.tea)
summary(fit_tea_2)
mean(data$Pickle.food)
sd(data$Pickle.food)
fit_pickle_1<-lm(data$BMI~data$Pickle.food)
summary(fit_pickle_1)
fit_pickle_2<-lm(data$PFAT~data$Pickle.food)
summary(fit_pickle_2)
mean(data$Fat.oil)
sd(data$Fat.oil)
fit_oil_1<-lm(data$BMI~data$Fat.oil)
summary(fit_oil_1)
fit_oil_2<-lm(data$PFAT~data$Fat.oil)
summary(fit_oil_2)
mean(data$Meals)
sd(data$Meals)
fit_meal_1<-lm(data$BMI~data$Meals)
summary(fit_meal_1)
fit_meal_2<-lm(data$PFAT~data$Meals)
summary(fit_meal_2)
data<-read.csv('CHN_LC_metadata.csv')
names(data)
mean(data$BMI)
sd(data$BMI)
mean(data$PFAT)
sd(data$PFAT)
cor.test(data$BMI,data$PFAT,method='spearman')
mean(data$Age)
sd(data$Age)
fit_age_1<-lm(data$BMI~data$Age)
summary(fit_age_1)
fit_age_2<-lm(data$PFAT~data$Age)
summary(fit_age_2)
table(data$Calcium_history)
prop.table(table(data$Calcium_history))
fit_cal_1<-lm(data$BMI~data$Calcium_history)
summary(fit_cal_1)
fit_cal_2<-lm(data$PFAT~data$Calcium_history)
summary(fit_cal_2)
table(data$Vitamin_history)
prop.table(table(data$Vitamin_history))
fit_vat_1<-lm(data$BMI~data$Vitamin_history)
summary(fit_vat_1)
fit_vat_2<-lm(data$PFAT~data$Vitamin_history)
summary(fit_vat_2)
table(data$Alcohol_Drink)
prop.table(table(data$Alcohol_Drink))
fit_alcohol_1<-lm(data$BMI~data$Alcohol_Drink)
summary(fit_alcohol_1)
fit_alcohol_2<-lm(data$PFAT~data$Alcohol_Drink)
summary(fit_alcohol_2)
table(data$Secondhand_Smoke)
prop.table(table(data$Secondhand_Smoke))
fit_smoke_1<-lm(data$BMI~data$Secondhand_Smoke)
summary(fit_smoke_1)
fit_smoke_2<-lm(data$PFAT~data$Secondhand_Smoke)
summary(fit_smoke_2)
table(data$Milk_Drink)
prop.table(table(data$Milk_Drink))
fit_milk_1<-lm(data$BMI~data$Milk_Drink)
summary(fit_milk_1)
fit_milk_2<-lm(data$PFAT~data$Milk_Drink)
summary(fit_milk_2)
table(data$SoybeanMilk_Drink)
prop.table(table(data$SoybeanMilk_Drink))
fit_soy_1<-lm(data$BMI~data$SoybeanMilk_Drink)
summary(fit_soy_1)
fit_soy_2<-lm(data$PFAT~data$SoybeanMilk_Drink)
summary(fit_soy_2)
table(data$Coffee_Drink)
prop.table(table(data$Coffee_Drink))
fit_coffee_1<-lm(data$BMI~data$Coffee_Drink)
summary(fit_coffee_1)
fit_coffee_2<-lm(data$PFAT~data$Coffee_Drink)
summary(fit_coffee_2)
table(data$Tea)
prop.table(table(data$Tea))
fit_tea_1<-lm(data$BMI~data$Tea)
summary(fit_tea_1)
fit_tea_2<-lm(data$PFAT~data$Tea)
summary(fit_tea_2)
table(data$MeatPreference)
prop.table(table(data$MeatPreference))
fit_meat_1<-lm(data$BMI~data$MeatPreference)
summary(fit_meat_1)
fit_meat_2<-lm(data$PFAT~data$MeatPreference)
summary(fit_meat_2)
table(data$Excercise)
prop.table(table(data$Excercise))
fit_exe_1<-lm(data$BMI~data$Excercise)
summary(fit_exe_1)
fit_exe_2<-lm(data$PFAT~data$Excercise)
summary(fit_exe_2)
mean(data$DailySleepTime_hr)
sd(data$DailySleepTime_hr)
fit_sleep_1<-lm(data$BMI~data$DailySleepTime_hr)
summary(fit_sleep_1)
fit_sleep_2<-lm(data$PFAT~data$DailySleepTime_hr)
summary(fit_sleep_2)
table(data$Nap)
prop.table(table(data$Nap))
fit_nap_1<-lm(data$BMI~data$Nap)
summary(fit_nap_1)
fit_nap_2<-lm(data$PFAT~data$Nap)
summary(fit_nap_2)
table(data$GreasyFood)
prop.table(table(data$GreasyFood))
fit_greasy_1<-lm(data$BMI~data$GreasyFood)
summary(fit_greasy_1)
fit_greasy_2<-lm(data$PFAT~data$GreasyFood)
summary(fit_greasy_2)
table(data$CholesterolIntake_amount)
prop.table(table(data$CholesterolIntake_amount))
fit_chol_1<-lm(data$BMI~data$CholesterolIntake_amount)
summary(fit_chol_1)
fit_chol_2<-lm(data$PFAT~data$CholesterolIntake_amount)
summary(fit_chol_2)
table(data$MeatIntake_RedMeat)
prop.table(table(data$MeatIntake_RedMeat))
fit_red_1<-lm(data$BMI~data$MeatIntake_RedMeat)
summary(fit_red_1)
fit_red_2<-lm(data$PFAT~data$MeatIntake_RedMeat)
summary(fit_red_2)
table(data$MeatIntake_WhiteMeat)
prop.table(table(data$MeatIntake_WhiteMeat))
fit_white_1<-lm(data$BMI~data$MeatIntake_WhiteMeat)
summary(fit_white_1)
fit_white_2<-lm(data$PFAT~data$MeatIntake_WhiteMeat)
summary(fit_white_2)
table(data$Vegetables)
prop.table(table(data$Vegetables))
fit_v_1<-lm(data$BMI~data$Vegetables)
summary(fit_v_1)
fit_v_2<-lm(data$PFAT~data$Vegetables)
summary(fit_v_2)
table(data$Fruits)
prop.table(table(data$Fruits))
fit_f_1<-lm(data$BMI~data$Fruits)
summary(fit_f_1)
fit_f_2<-lm(data$PFAT~data$Fruits)
summary(fit_f_2)
table(data$PickledOrFermentedFoods)
prop.table(table(data$PickledOrFermentedFoods))
fit_pickle_1<-lm(data$BMI~data$PickledOrFermentedFoods)
summary(fit_pickle_1)
fit_pickle_2<-lm(data$PFAT~data$PickledOrFermentedFoods)
summary(fit_pickle_2)
table(data$Yogurt)
prop.table(table(data$Yogurt))
fit_yogurt_1<-lm(data$BMI~data$Yogurt)
summary(fit_yogurt_1)
fit_yogurt_2<-lm(data$PFAT~data$Yogurt)
summary(fit_yogurt_2)
table(data$Water)
prop.table(table(data$Water))
fit_water_1<-lm(data$BMI~data$Water)
summary(fit_water_1)
fit_water_2<-lm(data$PFAT~data$Water)
summary(fit_water_2)
table(data$Stress)
prop.table(table(data$Stress))
fit_stress_1<-lm(data$BMI~data$Stress)
summary(fit_stress_1)
fit_stress_2<-lm(data$PFAT~data$Stress)
summary(fit_stress_2)
table(data$Anxiety)
prop.table(table(data$Anxiety))
fit_anx_1<-lm(data$BMI~data$Anxiety)
summary(fit_anx_1)
fit_anx_2<-lm(data$PFAT~data$Anxiety)
summary(fit_anx_2)
table(data$Drepression)
prop.table(table(data$Drepression))
fit_dep_1<-lm(data$BMI~data$Drepression)
summary(fit_dep_1)
fit_dep_2<-lm(data$PFAT~data$Drepression)
summary(fit_dep_2)
mean(data$FSH)
sd(data$FSH)
fit_FSH_1<-lm(data$BMI~data$FSH)
summary(fit_FSH_1)
fit_FSH_2<-lm(data$PFAT~data$FSH)
summary(fit_FSH_2)
##Alpha diversity association analysis##
library(vegan)
#Phylum
US_LC_P<-read.csv('US_LC_P.csv')
US_BGI_P<-read.csv('US_BGI_P.csv')
CHN_LC_P<-read.csv('CHN_LC_P.csv')
ID<-US_LC_P[,1]
US_LC_P<-US_LC_P[,-c(1)]
c1_alpha<-diversity(US_LC_P,index='shannon')
US_LC_P_alpha<-cbind(ID,US_LC_P,c1_alpha)
write.csv(US_LC_P_alpha,'US_LC_P_alpha.csv')
ID<-US_BGI_P[,1]
US_BGI_P<-US_BGI_P[,-c(1)]
c2_alpha<-diversity(US_BGI_P,index='shannon')
US_BGI_P_alpha<-cbind(ID,US_BGI_P,c2_alpha)
write.csv(US_BGI_P_alpha,'US_BGI_P_alpha.csv')
ID<-CHN_LC_P[,1]
CHN_LC_P<-CHN_LC_P[,-c(1)]
c3_alpha<-diversity(CHN_LC_P,index='shannon')
CHN_LC_P_alpha<-cbind(ID,CHN_LC_P,c3_alpha)
write.csv(CHN_LC_P_alpha,'CHN_LC_P_alpha.csv')
#Genus
US_LC_G<-read.csv('US_LC_G.csv')
US_BGI_G<-read.csv('US_BGI_G.csv')
CHN_LC_G<-read.csv('CHN_LC_G.csv')
ID<-US_LC_G[,1]
US_LC_G<-US_LC_G[,-c(1)]
c1_alpha<-diversity(US_LC_G,index='shannon')
US_LC_G_alpha<-cbind(ID,US_LC_G,c1_alpha)
write.csv(US_LC_G_alpha,'US_LC_G_alpha.csv')
ID<-US_BGI_G[,1]
US_BGI_G<-US_BGI_G[,-c(1)]
c2_alpha<-diversity(US_BGI_G,index='shannon')
US_BGI_G_alpha<-cbind(ID,US_BGI_G,c2_alpha)
write.csv(US_BGI_G_alpha,'US_BGI_G_alpha.csv')
ID<-CHN_LC_G[,1]
CHN_LC_G<-CHN_LC_G[,-c(1)]
c3_alpha<-diversity(CHN_LC_G,index='shannon')
CHN_LC_G_alpha<-cbind(ID,CHN_LC_G,c3_alpha)
write.csv(CHN_LC_G_alpha,'CHN_LC_G_alpha.csv')
#Species
US_LC_S<-read.csv('US_LC_S.csv')
US_BGI_S<-read.csv('US_BGI_S.csv')
CHN_LC_S<-read.csv('CHN_LC_S.csv')
ID<-US_LC_S[,1]
US_LC_S<-US_LC_S[,-c(1)]
c1_alpha<-diversity(US_LC_S,index='shannon')
US_LC_S_alpha<-cbind(ID,US_LC_S,c1_alpha)
write.csv(US_LC_S_alpha,'US_LC_S_alpha.csv')
ID<-US_BGI_S[,1]
US_BGI_S<-US_BGI_S[,-c(1)]
c2_alpha<-diversity(US_BGI_S,index='shannon')
US_BGI_S_alpha<-cbind(ID,US_BGI_S,c2_alpha)
write.csv(US_BGI_S_alpha,'US_BGI_S_alpha.csv')
ID<-CHN_LC_S[,1]
CHN_LC_S<-CHN_LC_S[,-c(1)]
c3_alpha<-diversity(CHN_LC_S,index='shannon')
CHN_LC_S_alpha<-cbind(ID,CHN_LC_S,c3_alpha)
write.csv(CHN_LC_S_alpha,'CHN_LC_S_alpha.csv')
#Association
#Phylum
C1_P_BMI<-read.csv('C1_P_BMI.csv')
C1_P_BMI<-C1_P_BMI[,-1]
fit<-lm(C1_P_BMI$BMI~.,data=C1_P_BMI)
summary(fit)
C1_P_PFAT<-read.csv('C1_P_PFAT.csv')
C1_P_PFAT<-C1_P_PFAT[,-1]
fit<-lm(C1_P_PFAT$PFAT~.,data=C1_P_PFAT)
summary(fit)
C2_P_BMI<-read.csv('C2_P_BMI.csv')
C2_P_BMI<-C2_P_BMI[,-1]
fit<-lm(C2_P_BMI$BMI~.,data=C2_P_BMI)
summary(fit)
C2_P_PFAT<-read.csv('C2_P_PFAT.csv')
C2_P_PFAT<-C2_P_PFAT[,-1]
fit<-lm(C2_P_PFAT$PFAT~.,data=C2_P_PFAT)
summary(fit)
C3_P_BMI<-read.csv('C3_P_BMI.csv')
C3_P_BMI<-C3_P_BMI[,-1]
fit<-lm(C3_P_BMI$BMI~.,data=C3_P_BMI)
summary(fit)
C3_P_PFAT<-read.csv('C3_P_PFAT.csv')
C3_P_PFAT<-C3_P_PFAT[,-1]
fit<-lm(C3_P_PFAT$PFAT~.,data=C3_P_PFAT)
summary(fit)
#Genus
C1_G_BMI<-read.csv('C1_G_BMI.csv')
C1_G_BMI<-C1_G_BMI[,-1]
fit<-lm(C1_G_BMI$BMI~.,data=C1_G_BMI)
summary(fit)
C1_G_PFAT<-read.csv('C1_G_PFAT.csv')
C1_G_PFAT<-C1_G_PFAT[,-1]
fit<-lm(C1_G_PFAT$PFAT~.,data=C1_G_PFAT)
summary(fit)
C2_G_BMI<-read.csv('C2_G_BMI.csv')
C2_G_BMI<-C2_G_BMI[,-1]
fit<-lm(C2_G_BMI$BMI~.,data=C2_G_BMI)
summary(fit)
C2_G_PFAT<-read.csv('C2_G_PFAT.csv')
C2_G_PFAT<-C2_G_PFAT[,-1]
fit<-lm(C2_G_PFAT$PFAT~.,data=C2_G_PFAT)
summary(fit)
C3_G_BMI<-read.csv('C3_G_BMI.csv')
C3_G_BMI<-C3_G_BMI[,-1]
fit<-lm(C3_G_BMI$BMI~.,data=C3_G_BMI)
summary(fit)
C3_G_PFAT<-read.csv('C3_G_PFAT.csv')
C3_G_PFAT<-C3_G_PFAT[,-1]
fit<-lm(C3_G_PFAT$PFAT~.,data=C3_G_PFAT)
summary(fit)
#Species
C1_S_BMI<-read.csv('C1_S_BMI.csv')
C1_S_BMI<-C1_S_BMI[,-1]
fit<-lm(C1_S_BMI$BMI~.,data=C1_S_BMI)
summary(fit)
C1_S_PFAT<-read.csv('C1_S_PFAT.csv')
C1_S_PFAT<-C1_S_PFAT[,-1]
fit<-lm(C1_S_PFAT$PFAT~.,data=C1_S_PFAT)
summary(fit)
C2_S_BMI<-read.csv('C2_S_BMI.csv')
C2_S_BMI<-C2_S_BMI[,-1]
fit<-lm(C2_S_BMI$BMI~.,data=C2_S_BMI)
summary(fit)
C2_S_PFAT<-read.csv('C2_S_PFAT.csv')
C2_S_PFAT<-C2_S_PFAT[,-1]
fit<-lm(C2_S_PFAT$PFAT~.,data=C2_S_PFAT)
summary(fit)
C3_S_BMI<-read.csv('C3_S_BMI.csv')
C3_S_BMI<-C3_S_BMI[,-1]
fit<-lm(C3_S_BMI$BMI~.,data=C3_S_BMI)
summary(fit)
C3_S_PFAT<-read.csv('C3_S_PFAT.csv')
C3_S_PFAT<-C3_S_PFAT[,-1]
fit<-lm(C3_S_PFAT$PFAT~.,data=C3_S_PFAT)
summary(fit)
##MaAsLin2
#US_LC cohort
setwd('C:/Users/19809/Desktop')
OTU_genus<-read.csv('LC_PFAT_genus_OTU_data.csv')
metadata<-read.csv('PFAT_metadata.csv')
row_name<-OTU_genus[,1]
OTU_genus<-OTU_genus[,c(-1)]
metadata<-metadata[,c(-1)]
rownames(OTU_genus)<-row_name
rownames(metadata)<-row_name
names(metadata)
fit_data<-Maaslin2( input_data = OTU_genus, input_metadata = metadata, 
                    output = "PFAT_OUTPUT_genus",
                    fixed_effects = c('PFAT', 'PC1','PC2','PC3','PC4','PC5','Race','Age','Smoke','Alcohol_Drinking',
                                      'Taking_Antibiotics','Taking_Probiotics','Taking_Acid_Medicine','Economic.level',
                                      'Exercise_Regular','V_Total',
                                      'F_Total','Steak','Pork','Fish.food','Egg',
                                      'Milk','Yogurt','Cheese','Cookies','Chocolate.candy',
                                      'Cold.cereal','Fiber.suplement','Water','Coffee',
                                      'Hot.tea','Pickle.food','Fat.oil','Meals'), 
                    analysis_method ='NEGBIN',min_abundance = 0,min_prevalence = 0,
                    min_variance = 0,normalization = 'CSS',transform = 'NONE'
)
write.csv(fit_data$results,'1_5 LC_continuous_genus_result.csv')
#US_BGI cohort
setwd('C:/Users/19809/Desktop')
OTU_genus<-read.csv('BGI_PFAT_genus_OTU.csv')
metadata<-read.csv('BGI_PFAT_metadata.csv')
row_name<-OTU_genus[,1]
OTU_genus<-OTU_genus[,c(-1)]
metadata<-metadata[,c(-1)]
rownames(OTU_genus)<-row_name
rownames(metadata)<-row_name
names(metadata)
fit_data<-Maaslin2( input_data = OTU_genus, input_metadata = metadata, 
                    output = "PFAT_OUTPUT_genus",
                    fixed_effects = c('PFAT','BGI_Batch','Gender','PC1','PC2','PC3','PC4','PC5','Race','Age','Smoke','Alcohol_Drinking',
                                      'Taking_Antibiotics','Taking_Probiotics','Economic.level','Exercise_Regular',
                                      'Milk','Yogurt','Cheese'), 
                    analysis_method ='NEGBIN',min_abundance = 0,min_prevalence = 0,
                    min_variance = 0,normalization = 'CSS',transform = 'NONE'
)
write.csv(fit_data$results,'1_5 BGI_PFAT_genus_ava_cov_results.csv')
#CHN cohort
OTU_genus<-read.csv('CHN_maaslin_genus_OTU.csv')
metadata<-read.csv('CHN_PFAT_metadata.csv')
row_name<-OTU_genus[,1]
OTU_genus<-OTU_genus[,c(-1)]
metadata<-metadata[,c(-1)]
rownames(OTU_genus)<-row_name
rownames(metadata)<-row_name
names(metadata)
fit_data<-Maaslin2( input_data = OTU_genus, input_metadata = metadata, 
                    output = "PFAT_OUTPUT_genus",
                    fixed_effects = c('PFAT','Age','Calcium_history',
                                      'Vitamin_history','Alcohol_Drink','Secondhand_Smoke',
                                      'Milk_Drink','SoybeanMilk_Drink','Coffee_Drink',
                                      'Tea','MeatPreference','Excercise','DailySleepTime_hr',
                                      'Nap','GreasyFood','CholesterolIntake_amount',
                                      'MeatIntake_RedMeat','MeatIntake_WhiteMeat','Vegetables',
                                      'Fruits','PickledOrFermentedFoods','Yogurt','Water',
                                      'Stress','Anxiety','Drepression','FSH'), 
                    analysis_method ='NEGBIN',min_abundance = 0,min_prevalence = 0,
                    min_variance = 0,normalization = 'CSS',transform = 'NONE'
)
write.csv(fit_data$results,'1_5 CHN_PFAT_genus_continuous.csv')
##Meta_analysis##
library(meta)
setwd('C:/Users/19809/Desktop')
data<-read.csv('5 BMI_continuous_data.csv')
Species<-data[,1]
data<-data[,-1]
n<-c(495,1917,498)
coef<-c()
lower_coef<-c()
upper_coef<-c()
se_coef<-c()
zval<-c()
p_coef<-c()
tau2<-c()
I2<-c()
p_Q<-c()
p_maaslin2<-c()
library(meta)
dim(data)
for (i in 1:119){
  Coef<-c(data[i,1],data[i,5],data[i,9])
  se <- c(data[i,2],data[i,6],data[i,10])
  P<-c(data[i,3],data[i,7],data[i,11])
  try_data<-data.frame(Coef,se,n,P)
  
  result<-metagen(TE=Coef,seTE = se,n.e=n,data=try_data,model='random')
  p_model<-sumlog(P)
  
  coef<-c(coef,result$TE.random)
  lower_coef<-c(lower_coef,result$lower.random)
  upper_coef<-c(upper_coef,result$upper.random)
  se_coef<-c(se_coef,result$seTE.random)
  zval<-c(zval,result$zval.random)
  p_coef<-c(p_coef,result$pval.random)
  tau2<-c(tau2,result$tau2)
  I2<-c(I2,result$I2)
  p_Q<-c(p_Q,result$pval.Q)
  p_maaslin2<-c(p_maaslin2,p_model$p)
}

FDR_maaslin2<-p.adjust(p_maaslin2)
results<-cbind(Species,data,coef,lower_coef,upper_coef,se_coef,zval,p_coef,tau2,I2,p_Q,p_maaslin2,FDR_maaslin2)
write.csv(results,'6 BMI_continuous_all_meta_result.csv')
BMI<-read.csv('6 BMI_continuous_all_meta_result.csv')
fdr<-p.adjust(BMI$P_SSBWZ)
BMI<-cbind(BMI,fdr)
write.csv(BMI,'6 BMI_continuous_all_meta_result.csv')
BMI_final_results<-read.csv('BMI_metaanalysis_final_results.csv')
BMI_final_results<-read.csv('BMI_metaanalysis_final_results.csv')
Obesity_final<-merge(BMI_final_results,BMI_final_results,by='Species')
dim(Obesity_final)
write.csv(Obesity_final,'Obesity_BMI_BMI_final_results.csv')
#******************************************************************************#
#Metabolomic analysis#
#UTM
#PLS regression (Partial least squares regression)
setwd('C:/Users/19809/Desktop')
library(ropls)
UTM_PLS_data<-read.csv('US_UTM_PLS.csv')
names(UTM_PLS_data)
##BMI
y<-UTM_PLS_data[,2]
x<-as.matrix(UTM_PLS_data[,4:332])
BMI_UTM_plsda<- opls(x, y, orthoI = 0)
vipVn <-BMI_UTM_plsda@vipVn  # getVipVn()
write.csv(vipVn,'UTM_BMI_vip.csv')
##PFAT
y<-UTM_PLS_data[,3]
x<-as.matrix(UTM_PLS_data[,4:332])
PFAT_UTM_plsda<- opls(x, y, orthoI = 0)
vipVn <-PFAT_UTM_plsda@vipVn
write.csv(vipVn,'UTM_PFAT_vip.csv')

#Linear regression
UTM_LM_data<-read.csv('US_UTM_LM.csv')
##BMI
names(UTM_LM_data)
final_result<-data.frame()
library(broom)
for (i in 18:346){
  reg<-lm(UTM_LM_data$BMI~.,data = UTM_LM_data[,c(2,5:17,i)])
  result<-tidy(reg)[15,]
  final_result<-rbind(final_result,result)
  i<-i+1
}
p<-final_result$p.value
q<-p.adjust(p)
final_result<-cbind(final_result,q)
write.csv(final_result,'UTM_BMI_LM.csv')
##PFAT
final_result<-data.frame()
library(broom)
for (i in 18:346){
  reg<-lm(UTM_LM_data$WBTOT_PFAT~.,data = UTM_LM_data[,c(3:17,i)])
  result<-tidy(reg)[16,]
  final_result<-rbind(final_result,result)
  i<-i+1
}
p<-final_result$p.value
q<-p.adjust(p)
final_result<-cbind(final_result,q)
write.csv(final_result,'UTM_PFAT_LM.csv')

#SCFA
#PLS regression (Partial least squares regression)
setwd('C:/Users/19809/Desktop')
library(ropls)
SCFA_PLS_data<-read.csv('US_SCFA_PLS.csv')
names(SCFA_PLS_data)
##BMI
y<-SCFA_PLS_data[,2]
x<-as.matrix(SCFA_PLS_data[,c(4:11)])
BMI_SCFA_plsda<- opls(x, y, orthoI = 1)
vipVn <-BMI_SCFA_plsda@vipVn  # getVipVn()
write.csv(vipVn,'SCFA_BMI_vip_2.csv')
##PFAT
y<-SCFA_PLS_data[,3]
x<-as.matrix(SCFA_PLS_data[,4:11])
PFAT_SCFA_plsda<- opls(x, y, orthoI = 1)
vipVn <-PFAT_SCFA_plsda@vipVn
write.csv(vipVn,'SCFA_PFAT_vip_cv.csv')
#Linear regression
SCFA_LM_data<-read.csv('US_SCFA_LM.csv')
##BMI
names(SCFA_LM_data)
final_result<-data.frame()
library(broom)
for (i in 18:25){
  reg<-lm(SCFA_LM_data$BMI~.,data = SCFA_LM_data[,c(2,5:17,i)])
  result<-tidy(reg)[15,]
  final_result<-rbind(final_result,result)
  i<-i+1
}
p<-final_result$p.value
q<-p.adjust(p)
final_result<-cbind(final_result,q)
write.csv(final_result,'SCFA_BMI_LM.csv')
##PFAT
final_result<-data.frame()
library(broom)
for (i in 18:25){
  reg<-lm(SCFA_LM_data$WBTOT_PFAT~.,data = SCFA_LM_data[,c(3:17,i)])
  result<-tidy(reg)[16,]
  final_result<-rbind(final_result,result)
  i<-i+1
}
p<-final_result$p.value
q<-p.adjust(p)
final_result<-cbind(final_result,q)
write.csv(final_result,'SCFA_PFAT_LM.csv')
#LIPID
#PLS regression (Partial least squares regression)
setwd('C:/Users/19809/Desktop')
library(ropls)
LIPID_PLS_data<-read.csv('US_LIPID_PLS.csv')
names(LIPID_PLS_data)
##BMI
y<-LIPID_PLS_data[,2]
x<-as.matrix(LIPID_PLS_data[,4:17])
BMI_LIPID_plsda<- opls(x, y, orthoI = 0)
vipVn <-BMI_LIPID_plsda@vipVn  # getVipVn()
write.csv(vipVn,'LIPID_BMI_vip.csv')
##PFAT
y<-LIPID_PLS_data[,3]
x<-as.matrix(LIPID_PLS_data[,4:17])
PFAT_LIPID_plsda<- opls(x, y, orthoI = 0)
vipVn <-PFAT_LIPID_plsda@vipVn
write.csv(vipVn,'LIPID_PFAT_vip.csv')
#Linear regression
LIPID_LM_data<-read.csv('US_LIPID_LM.csv')
##BMI
names(LIPID_LM_data)
final_result<-data.frame()
library(broom)
for (i in 18:31){
  reg<-lm(LIPID_LM_data$BMI~.,data = LIPID_LM_data[,c(2,5:17,i)])
  result<-tidy(reg)[15,]
  final_result<-rbind(final_result,result)
  i<-i+1
}
p<-final_result$p.value
q<-p.adjust(p)
final_result<-cbind(final_result,q)
write.csv(final_result,'LIPID_BMI_LM.csv')
##PFAT
final_result<-data.frame()
library(broom)
for (i in 18:31){
  reg<-lm(LIPID_LM_data$WBTOT_PFAT~.,data = LIPID_LM_data[,c(3:17,i)])
  result<-tidy(reg)[16,]
  final_result<-rbind(final_result,result)
  i<-i+1
}
p<-final_result$p.value
q<-p.adjust(p)
final_result<-cbind(final_result,q)
write.csv(final_result,'LIPID_PFAT_LM.csv')
#******************************************************************************#
#Metagenomic-metabolomic association analysis
#Multi-omics, pcor
#GM_UTM
##BMI
###C1
data<-read.csv('C1_BMI_GM_UTM_final.csv')
metag<-c()
metab<-c()
coef<-c()
p<-c()
data<-data[,-1]
names(data)
library(ppcor)
for (i in 1:26){
  for (j in 27:141){
    test<-pcor.test(data[,i],data[,j],data[,142:173],method='spearman')
    metag<-c(metag,names(data)[i])
    metab<-c(metab,names(data)[j])
    coef<-c(coef,test$estimate)
    p<-c(p,test$p.value)
    j<-j+1
  }
  i<-i+1
}
fdr<-p.adjust(p)
result<-data.frame(metag,metab,coef,p,fdr)
write.csv(result,'C1_BMI_GM_UTM_asso_result.csv')
###C2
data<-read.csv('C2_BMI_GM_UTM_final.csv')
metag<-c()
metab<-c()
coef<-c()
p<-c()
data<-data[,-1]
names(data)
library(ppcor)
for (i in 1:26){
  for (j in 27:141){
    test<-pcor.test(data[,i],data[,j],data[,142:154],method='spearman')
    metag<-c(metag,names(data)[i])
    metab<-c(metab,names(data)[j])
    coef<-c(coef,test$estimate)
    p<-c(p,test$p.value)
    j<-j+1
  }
  i<-i+1
}
fdr<-p.adjust(p)
result<-data.frame(metag,metab,coef,p,fdr)
write.csv(result,'C2_BMI_GM_UTM_asso_result.csv')
##PFAT
###C1
data<-read.csv('C1_PFAT_GM_UTM_final.csv')
metag<-c()
metab<-c()
coef<-c()
p<-c()
data<-data[,-1]
names(data)
library(ppcor)
for (i in 1:23){
  for (j in 24:101){
    test<-pcor.test(data[,i],data[,j],data[,102:134],method='spearman')
    metag<-c(metag,names(data)[i])
    metab<-c(metab,names(data)[j])
    coef<-c(coef,test$estimate)
    p<-c(p,test$p.value)
    j<-j+1
  }
  i<-i+1
}
fdr<-p.adjust(p)
result<-data.frame(metag,metab,coef,p,fdr)
write.csv(result,'C1_PFAT_GM_UTM_asso_result.csv')
###C2
data<-read.csv('C2_PFAT_GM_UTM_final.csv')
metag<-c()
metab<-c()
coef<-c()
p<-c()
data<-data[,-1]
names(data)
library(ppcor)
for (i in 1:23){
  for (j in 24:101){
    test<-pcor.test(data[,i],data[,j],data[,102:115],method='spearman')
    metag<-c(metag,names(data)[i])
    metab<-c(metab,names(data)[j])
    coef<-c(coef,test$estimate)
    p<-c(p,test$p.value)
    j<-j+1
  }
  i<-i+1
}
fdr<-p.adjust(p)
result<-data.frame(metag,metab,coef,p,fdr)
write.csv(result,'C2_PFAT_GM_UTM_asso_result.csv')

#GM_SCFA
##BMI
###C1
data<-read.csv('C1_BMI_GM_SCFA_final.csv')
metag<-c()
metab<-c()
coef<-c()
p<-c()
data<-data[,-1]
names(data)
library(ppcor)
for (i in 1:26){
  for (j in 27:34){
    test<-pcor.test(data[,i],data[,j],data[,35:66],method='spearman')
    metag<-c(metag,names(data)[i])
    metab<-c(metab,names(data)[j])
    coef<-c(coef,test$estimate)
    p<-c(p,test$p.value)
    j<-j+1
  }
  i<-i+1
}
fdr<-p.adjust(p)
result<-data.frame(metag,metab,coef,p,fdr)
write.csv(result,'C1_BMI_GM_SCFA_asso_result.csv')
###C2
data<-read.csv('C2_BMI_GM_SCFA_final.csv')
metag<-c()
metab<-c()
coef<-c()
p<-c()
data<-data[,-1]
names(data)
library(ppcor)
for (i in 1:26){
  for (j in 27:34){
    test<-pcor.test(data[,i],data[,j],data[,35:47],method='spearman')
    metag<-c(metag,names(data)[i])
    metab<-c(metab,names(data)[j])
    coef<-c(coef,test$estimate)
    p<-c(p,test$p.value)
    j<-j+1
  }
  i<-i+1
}
fdr<-p.adjust(p)
result<-data.frame(metag,metab,coef,p,fdr)
write.csv(result,'C2_BMI_GM_SCFA_asso_result.csv')
##PFAT
###C1
data<-read.csv('C1_PFAT_GM_SCFA_final.csv')
metag<-c()
metab<-c()
coef<-c()
p<-c()
data<-data[,-1]
names(data)
library(ppcor)
for (i in 1:23){
  for (j in 24:31){
    test<-pcor.test(data[,i],data[,j],data[,32:64],method='spearman')
    metag<-c(metag,names(data)[i])
    metab<-c(metab,names(data)[j])
    coef<-c(coef,test$estimate)
    p<-c(p,test$p.value)
    j<-j+1
  }
  i<-i+1
}
fdr<-p.adjust(p)
result<-data.frame(metag,metab,coef,p,fdr)
write.csv(result,'C1_PFAT_GM_SCFA_asso_result.csv')
###C2
data<-read.csv('C2_PFAT_GM_SCFA_final.csv')
metag<-c()
metab<-c()
coef<-c()
p<-c()
data<-data[,-1]
names(data)
library(ppcor)
for (i in 1:23){
  for (j in 24:31){
    test<-pcor.test(data[,i],data[,j],data[,32:45],method='spearman')
    metag<-c(metag,names(data)[i])
    metab<-c(metab,names(data)[j])
    coef<-c(coef,test$estimate)
    p<-c(p,test$p.value)
    j<-j+1
  }
  i<-i+1
}
fdr<-p.adjust(p)
result<-data.frame(metag,metab,coef,p,fdr)
write.csv(result,'C2_PFAT_GM_SCFA_asso_result.csv')

#GM_LIPID
##BMI
###C1
data<-read.csv('C1_BMI_GM_LIPID_final.csv')
metag<-c()
metab<-c()
coef<-c()
p<-c()
data<-data[,-1]
names(data)
library(ppcor)
for (i in 1:26){
  for (j in 27:40){
    test<-pcor.test(data[,i],data[,j],data[,41:72],method='spearman')
    metag<-c(metag,names(data)[i])
    metab<-c(metab,names(data)[j])
    coef<-c(coef,test$estimate)
    p<-c(p,test$p.value)
    j<-j+1
  }
  i<-i+1
}
fdr<-p.adjust(p)
result<-data.frame(metag,metab,coef,p,fdr)
write.csv(result,'C1_BMI_GM_LIPID_asso_result.csv')
###C2
data<-read.csv('C2_BMI_GM_LIPID_final.csv')
metag<-c()
metab<-c()
coef<-c()
p<-c()
data<-data[,-1]
names(data)
library(ppcor)
for (i in 1:26){
  for (j in 27:40){
    test<-pcor.test(data[,i],data[,j],data[,41:53],method='spearman')
    metag<-c(metag,names(data)[i])
    metab<-c(metab,names(data)[j])
    coef<-c(coef,test$estimate)
    p<-c(p,test$p.value)
    j<-j+1
  }
  i<-i+1
}
fdr<-p.adjust(p)
result<-data.frame(metag,metab,coef,p,fdr)
write.csv(result,'C2_BMI_GM_LIPID_asso_result.csv')
##PFAT
###C1
data<-read.csv('C1_PFAT_GM_LIPID_final.csv')
metag<-c()
metab<-c()
coef<-c()
p<-c()
data<-data[,-1]
names(data)
library(ppcor)
for (i in 1:23){
  for (j in 24:37){
    test<-pcor.test(data[,i],data[,j],data[,38:70],method='spearman')
    metag<-c(metag,names(data)[i])
    metab<-c(metab,names(data)[j])
    coef<-c(coef,test$estimate)
    p<-c(p,test$p.value)
    j<-j+1
  }
  i<-i+1
}
fdr<-p.adjust(p)
result<-data.frame(metag,metab,coef,p,fdr)
write.csv(result,'C1_PFAT_GM_LIPID_asso_result.csv')
###C2
data<-read.csv('C2_PFAT_GM_LIPID_final.csv')
metag<-c()
metab<-c()
coef<-c()
p<-c()
data<-data[,-1]
names(data)
library(ppcor)
for (i in 1:23){
  for (j in 24:37){
    test<-pcor.test(data[,i],data[,j],data[,38:51],method='spearman')
    metag<-c(metag,names(data)[i])
    metab<-c(metab,names(data)[j])
    coef<-c(coef,test$estimate)
    p<-c(p,test$p.value)
    j<-j+1
  }
  i<-i+1
}
fdr<-p.adjust(p)
result<-data.frame(metag,metab,coef,p,fdr)
write.csv(result,'C2_PFAT_GM_LIPID_asso_result.csv')
#******************************************************************************#
#MR
exposure_dat <-read.csv('MR1_B_exposure.csv')
outcome_dat <-read.csv('MR1_B_outcome.csv')
dat <- harmonise_data(
  exposure_dat = exposure_dat,
  outcome_dat = outcome_dat
)
dim(dat)
res<-mr(dat)
res
# Calculate R2 and F stat for exposure data
exposure_dat$N<-3135
exposure_dat$r2 <- (2 * (exposure_dat$beta.exposure^2) * exposure_dat$eaf.exposure * (1 - exposure_dat$eaf.exposure)) /
  (2 * (exposure_dat$beta.exposure^2) * exposure_dat$eaf.exposure * (1 - exposure_dat$eaf.exposure) +
     2 * exposure_dat$N * exposure_dat$eaf.exposure * (1 - exposure_dat$eaf.exposure) * exposure_dat$se.exposure^2)
exposure_dat$F <- exposure_dat$r2 * (exposure_dat$N - 2) / (1 - exposure_dat$r2)
exposure_dat_meanF <- mean(exposure_dat$F)
exposure_dat_meanF
mr_heterogeneity(dat)
mr_pleiotropy_test(dat)
res_loo <- mr_leaveoneout(dat)
mr_leaveoneout_plot(res_loo)
p1 <- mr_scatter_plot(res, dat)
p1
res_single <- mr_singlesnp(dat)
mr_forest_plot(res_single)
mr_funnel_plot(res_single)
#******************************************************************************#
#Animal and cell experiments
library(PMCMRplus)
data<-read.csv('Serum_bio_Bs.csv')
data$Group<-factor(data$Group)
kruskal.test(data$TG~data$Group,data=data)
library(rstatix)
pairwise_wilcox_test (data,TG~Group, 
                      p.adjust.method = "bonf") 

View(data)
#******************************************************************************#