library(lattice)
library(rms)
library(car)
load("vitd.RData")
describe(VitD)
str(VitD)
#include predictor of exposure, not of outcome.........?
#season may be effect modifier. B/c effects the effect of freeplay on vitD b/c outdoor. 
VitD.dd <- datadist(VitD)
options(datadist = "VitD.dd")
### Effect modifier: season (Categorical)
### Continouous Confounders: age, Milk, screen, zBMI???
### Categorical Confoudners: gender, skin, currentBrstFed, childTakeVD, parentEdu, zBMIclass

hist(VitD$freePlay)
hist(VitD$age)
hist(VitD$Milk)
hist(VitD$screen)
hist(VitD$zBMI)

rcorr (VitD$VITD, VitD$freePlay)
xyplot (VITD~freePlay, data=VitD, xlab="Outdoor Play Time (min)",ylab="Vitamin D Level")

rcorr (VitD$VITD, VitD$age)
xyplot (VITD~age, data=VitD, xlab="age (months)",ylab="Vitamin D Level")
xyplot (VITD~ageYear, data=VitD, xlab="age (years)",ylab="Vitamin D Level")

rcorr (VitD$VITD, VitD$Milk)
xyplot (VITD~Milk, data=VitD, xlab="Milk intake (mL)",ylab="Vitamin D Level")

rcorr (VitD$VITD, VitD$Milk)
xyplot (VITD~Milk, data=VitD, xlab="Milk intake (mL)",ylab="Vitamin D Level")

rcorr (VitD$VITD, VitD$screen)
xyplot (VITD~screen, data=VitD, xlab="Daily TV Screen time (min)",ylab="Vitamin D Level")

rcorr (VitD$VITD, VitD$zBMI)
xyplot (VITD~zBMI, data=VitD, xlab="BMI z-scores",ylab="Vitamin D Level")

#bivariate association of CATEGORICAL confounders (X is categorical, Y is continuous), modifiers
#box and whisker plots make for quick visual inspection

by (VitD$VITD, VitD$season, summary)
by (VitD$VITD, VitD$gender, summary)
by (VitD$VITD, VitD$skin, summary)
by (VitD$VITD, VitD$currentBrstFed, summary)
by (VitD$VITD, VitD$childTakeVD, summary)
by (VitD$VITD, VitD$parentEdu, summary)
by (VitD$VITD, VitD$zBMIclass, summary)

tapply (VitD$VITD, VitD$season, sd)
tapply (VitD$VITD, VitD$gender, sd)
tapply (VitD$VITD, VitD$skin, sd)
tapply (VitD$VITD, VitD$currentBrstFed, sd)
tapply (VitD$VITD, VitD$childTakeVD, sd)
tapply (VitD$VITD, VitD$parentEdu, sd)
tapply (VitD$VITD, VitD$zBMIclass, sd)

bwplot (VITD~season, data=VitD)
bwplot (VITD~gender, data=VitD)
bwplot (VITD~skin, data=VitD)
bwplot (VITD~currentBrstFed, data=VitD)
bwplot (VITD~childTakeVD, data=VitD)
bwplot (VITD~parentEdu, data=VitD)
bwplot (VITD~zBMIclass, data=VitD)
bwplot (VITD~bottle, data=VitD)

#crude association of focal variables
crude.DFreePlay <-ols(VITD~freePlay, data=VitD)
crude.DFreePlay
#Note when running crude.ex, the residuals data should have a mean more or less = 0. i.e normal residual distribution
#look at R2, fit is poor here. 
summary(crude.DFreePlay)
summary(crude.DFreePlay, freePlay=c(100,101))
anova(crude.DFreePlay)

#plot of focal variables

xyplot(VITD~freePlay,data=VitD,xlab="Outdoor Play Time (min)",ylab="Vitamin D Level",type=c('p','r'))
qqnorm(resid(crude.DFreePlay), main="normalqq") 
qqline(as.numeric(resid(crude.DFreePlay)))


#prediction for group means; can also input multiple conditions
Predict(crude.DFreePlay,freePlay=48)

### Prediction for an individual
Predict(crude.DFreePlay,freePlay=48,conf.type="individual")



#####crude association of other variables with outcome
### Effect modifier: season (Categorical)
### Continouous Confounders: age, Milk, screen, zBMI???
### Categorical Confoudners: gender, skin, currentBrstFed, childTakeVD, parentEdu, zBMIclass
crude.season <-ols (VITD~season, data=VitD)
crude.season
summary(crude.season)
anova(crude.season)

crude.age <-ols (VITD~age, data=VitD)
crude.age
summary(crude.age, age=c(30,31))
xyplot(VITD~age,data=VitD,xlab="age (months)",ylab="Vitamin D Level",type=c('p','r'))
anova(crude.age)

crude.Milk <-ols (VITD~Milk, data=VitD)
crude.Milk
summary(crude.Milk, Milk=c(30,31))
xyplot(VITD~Milk,data=VitD,xlab="Milk (mL)",ylab="Vitamin D Level",type=c('p','r'))
anova(crude.Milk)

crude.screen <-ols (VITD~screen, data=VitD)
crude.screen
summary(crude.screen, screen=c(30,31))
xyplot(VITD~screen,data=VitD,xlab="screen time (min) ",ylab="Vitamin D Level",type=c('p','r'))
anova(crude.screen)

crude.gender <-ols (VITD~gender, data=VitD)
crude.gender
summary(crude.gender)
anova(crude.gender)

crude.skin <-ols (VITD~skin, data=VitD)
crude.skin
summary(crude.skin)
anova(crude.skin)

crude.currentBrstFed <-ols (VITD~currentBrstFed, data=VitD)
crude.currentBrstFed
summary(crude.currentBrstFed)
anova(crude.currentBrstFed)

crude.childTakeVD <-ols (VITD~childTakeVD, data=VitD)
crude.childTakeVD
summary(crude.childTakeVD)
anova(crude.childTakeVD)

crude.parentEdu <-ols (VITD~parentEdu, data=VitD)
crude.parentEdu
summary(crude.parentEdu)
anova(crude.parentEdu)

crude.zBMI <-ols (VITD~zBMI, data=VitD)
crude.zBMI
summary(crude.zBMI)
anova(crude.zBMI)


#####crude association of other variables with exposure
### Effect modifier: season (Categorical)
### Continouous Confounders: age, Milk, screen, zBMI???
### Categorical Confoudners: gender, skin, currentBrstFed, childTakeVD, parentEdu, zBMIclass
crudexp.season <-ols (freePlay~season, data=VitD)
crudexp.season
anova (crudexp.season)

crudexp.age <-ols (freePlay~age, data=VitD)
crudexp.age
anova (crudexp.age)

crudexp.Milk <-ols (freePlay~Milk, data=VitD)
crudexp.Milk
anova (crudexp.Milk)

crudexp.screen <-ols (freePlay~screen, data=VitD)
crudexp.screen
anova (crudexp.screen)

crudexp.gender <-ols (freePlay~gender, data=VitD)
crudexp.gender
anova (crudexp.gender)

crudexp.skin <-ols (freePlay~skin, data=VitD)
crudexp.skin
anova (crudexp.skin)

crudexp.currentBrstFed <-ols (freePlay~currentBrstFed, data=VitD)
crudexp.currentBrstFed
anova (crudexp.currentBrstFed)

crudexp.childTakeVD <-ols (freePlay~childTakeVD, data=VitD)
crudexp.childTakeVD
anova (crudexp.childTakeVD)

crudexp.parentEdu <-ols (freePlay~parentEdu, data=VitD)
crudexp.parentEdu
anova (crudexp.parentEdu)

crudexp.zBMIclass <-ols (freePlay~zBMIclass, data=VitD)
crudexp.zBMIclass
anova (crudexp.zBMIclass)




### checking for interaction

int.freeplay.season <-ols(VITD~freePlay*season, data=VitD)
int.freeplay.season
anova (int.freeplay.season)
int.freeplay.childTakeVD <-ols(VITD~freePlay*childTakeVD, data=VitD)
int.freeplay.childTakeVD
anova (int.freeplay.childTakeVD)

int.freeplay.zBMIclass <-ols(VITD~freePlay*zBMIclass, data=VitD)
int.freeplay.zBMIclass
anova (int.freeplay.zBMI)

int.freeplay.currentBrstFed <-ols(VITD~freePlay*currentBrstFed, data=VitD)
int.freeplay.currentBrstFed
anova (int.freeplay.currentBrstFed)

int.freeplay.parentEdu <-ols(VITD~freePlay*parentEdu, data=VitD)
int.freeplay.parentEdu
anova (int.freeplay.parentEdu)

int.freeplay.skin <-ols(VITD~freePlay*skin, data=VitD)
int.freeplay.skin
anova (int.freeplay.skin)

#run model without interaction term for partial F test example
int2.freeplay.season<-ols(VITD~freePlay+season, data=VitD)
int2.freeplay.season
anova (int2.freeplay.season)


##### plot interaction

plot(Predict(int.freeplay.season,freePlay,season))

plot(Predict(int.freeplay.skin,freePlay,skin))
###### Check for interaction using stratified approach - are conclusions the same?

str1.season <- ols(VITD~freePlay,data=VitD,subset=season=="Oct-Apr")
str1.season
summary(str1.season,freePlay=c(50,51),conf.int=0.90)
anova(str1.season)

str2.season <- ols(VITD~freePlay,data=VitD,subset=season=="May-Sep")
str2.season
summary(str2.season,freePlay=c(50,51),conf.int=0.90)
anova(str2.season)



########
###############Understanding confounding
############## Three approaches: Full model, forward, and stepwise
############## 


##################Starting with full model approach
###############
############## Remove one variable at a time from the full model. 
##############  Explore % change in freePlay beta from full model.  
################# consider 10% confounding
### Effect modifier: season (Categorical)
### Continouous Confounders: age, Milk, screen, zBMI???
### Categorical Confoudners: gender, skin, currentBrstFed, childTakeVD, parentEdu, zBMIclass

full.VITD <-ols (VITD~freePlay*season+childTakeVD+age+zBMI+screen+gender+currentBrstFed+parentEdu+Milk+skin, data=VitD)
full.VITD
anova (full.VITD)
summary (full.VITD)

red.childTakeVD <-ols (VITD~freePlay*season+age+zBMI+screen+gender+currentBrstFed+parentEdu+Milk+skin, data=VitD)
red.childTakeVD

red.age <-ols (VITD~freePlay*season+childTakeVD+zBMI+screen+gender+currentBrstFed+parentEdu+Milk+skin, data=VitD)
red.age

red.zBMI <-ols (VITD~freePlay*season+childTakeVD+age+screen+gender+currentBrstFed+parentEdu+Milk+skin, data=VitD)
red.zBMI

red.screen <-ols (VITD~freePlay*season+age+zBMI+childTakeVD+gender+currentBrstFed+parentEdu+Milk+skin, data=VitD)
red.screen

red.gender <-ols (VITD~freePlay*season+age+zBMI+screen+childTakeVD+currentBrstFed+parentEdu+Milk+skin, data=VitD)
red.gender

red.currentBrstFed <-ols (VITD~freePlay*season+age+zBMI+screen+gender+childTakeVD+parentEdu+Milk+skin, data=VitD)
red.currentBrstFed

red.parentEdu <-ols (VITD~freePlay*season+age+zBMI+screen+gender+currentBrstFed+childTakeVD+Milk+skin, data=VitD)
red.parentEdu

red.Milk <-ols (VITD~freePlay*season+age+zBMI+screen+gender+currentBrstFed+parentEdu+childTakeVD+skin, data=VitD)
red.Milk

red.skin <-ols (VITD~freePlay*season+age+zBMI+screen+gender+currentBrstFed+parentEdu+childTakeVD+Milk, data=VitD)
red.skin

##################Trying the forward step apporach
###############  Start with base model and add variables 
##############  one at a time, in terms of their relative rank importance
##############  Importance determined by % change 
################# 

#run baseline model to check "adjusted" estimates for
basel.mt <-ols (gh~bmi*agecat, data=reg)
basel.mt

#check confounding effect
step1.tri <-ols (gh~tri+bmi*agecat, data=reg)
step1.tri
step1.wst <-ols (gh~waist+bmi*agecat, data=reg)
step1.wst
step1.sx <-ols (gh~sex+bmi*agecat, data=reg)
step1.sx
step1.rc <-ols (gh~re+bmi*agecat, data=reg)
step1.rc
step1.inc <-ols (gh~income4+bmi*agecat, data=reg)
step1.inc



# find that waist circumference is largest confounder.  
#add waist to model and check for confounding rank

#baseline model
base2.wst <-ols (gh~waist+bmi*agecat, data=reg)
base2.wst

#evaluate confounding
step2.tri <-ols (gh~waist+tri+bmi*agecat, data=reg)
step2.tri
step2.sx <-ols (gh~sex+bmi*agecat+waist, data=reg)
step2.sx
step2.race <-ols (gh~re+bmi*agecat+waist, data=reg)
step2.race
step2.inc <-ols (gh~income4+bmi*agecat+waist, data=reg)
step2.inc


#find that race next highest confounder
#baseline model
base3.race <-ols (gh~re+bmi*agecat+waist, data=reg)
base3.race

#adding confounders
step3.tri <-ols (gh~re+waist+tri+bmi*agecat, data=reg)
step3.tri
step3.sx <-ols (gh~re+sex+bmi*agecat+waist, data=reg)
step3.sx
step3.inc <-ols (gh~re+income4+bmi*agecat+waist, data=reg)
step3.inc




# find tricep next strongest

#baseline model
base4.tri <-ols (gh~re+waist+tri+bmi*agecat, data=reg)
base4.tri


#adding confouders
step4.sex <-ols (gh~re+sex+bmi*agecat+waist+tri, data=reg)
step4.sex
step4.inc <-ols (gh~re+income4+bmi*agecat+waist+tri, data=reg)
step4.inc


#none of other variables meet the 10% change criteria. 
#lets add next strongest (age)

base5.sex <-ols(gh~re+sex+bmi*agecat+waist+tri, data=reg)
base5.sex

step5.inc <-ols(gh~re+sex+bmi*agecat+waist+tri+income4, data=reg)
step5.inc


####################################
################################ finally - let's run stepwise - for fun!
###############################
###########################
################################


stepw.ols <-lm(gh~income4+re+sex+tri+waist+bmi+agecat+bmi*agecat, data=reg)
stepw.ols <-stepAIC(stepw.ols, direction="both")
stepw.ols$anova
summary (stepw.ols$anova)
steps.ols<-ols(gh~re+sex+tri+waist+bmi+agecat+bmi*agecat, data=reg)
steps.ols



##########
###########
############THIS IS OUR FINAL PRELIMINARY MODEL!
############
#############
############
###########

full.VITD <-ols (VITD~freePlay*season+childTakeVD+age+zBMI+screen+gender+currentBrstFed+parentEdu+Milk+skin, data=VitD, x=TRUE)
full.VITD

summary (full.VITD)
plot(summary(full.VITD))
write.csv(full.VITD$Pr, file="final model p")
confint(full.VITD)
summary(full.VITD, freePlay=c(100,101), age=c(20,21), zBMI=c(0,1), screen=c(40,41), Milk=c(250,251),est.all=TRUE)
#Can also do prediction, use Predict() function and input parameters as needed for each independent variable in model.
anova (full.VITD)

#Residuals normality check
resVitD <- resid(full.VITD)
qqnorm(resid(full.VITD), main="normalqq") 
qqline(as.numeric(resid(full.VITD)))
sum(resVitD)
summary(resVitD)
hist(resVitD)

#homoscedasticity check
#Residuals against fitted/predicted values
xyplot(resid(full.VITD)~fitted(full.VITD), xlim=c(68.75,103))

#Residuals against predictor value freePlay
xyplot(resid(full.VITD)~freePlay, data=VitD, xlim=c(0,400))
xyplot(resid(full.VITD)~zBMI, data=VitD)

#Checking for linearity with lowess smooth line
xyplot(resid(full.VITD)~freePlay,data=VitD,xlab="Outdoor Free Play (min)",
       ylab="Residuals",main="Residuals versus freeplay", type="smooth")

resid2<-lm(VITD~freePlay+childTakeVD+age+zBMI+screen+gender+currentBrstFed+parentEdu+Milk+skin, data=VitD, x=TRUE, subset=season=="Oct-Apr") 
crPlots(resid2)

#### see what happens to CI if include:  nothing!  keep sex, income out of model!
pre1a.ols <-ols (gh~re++bmi*agecat+waist+tri+sex+income4, data=reg7)
pre1a.ols
summary (pre1a.ols, bmi=c(20,21), tri=c(14,15), waist=c(90,91))
anova (pre1a.ols)

summary (con1.ols, bmi=c(20,21), tri=c(14,15))

confint(pre1a.ols, bmi=c(20,21), tri=c(14,15), waist=c(90,91))



#### Check for multicollinearity

mc1<-vif(test.VITD)
mc1

## No multicolinearity!!

## check for influential observations

dfbval <- 2/sqrt(1392)
dfbval



test.VITD <-lm(VITD~freePlay*season+childTakeVD+age+zBMI+screen+gender+currentBrstFed+parentEdu+Milk+skin, data=VitD, x=TRUE)
test.VITD

test.VITD <- glm(VITD~freePlay*season+childTakeVD+age+zBMI+screen+gender+currentBrstFed+parentEdu+Milk+skin, data=VitD, x=TRUE, y=TRUE)
infl.db <- which.influence(test.VITD, 0.053606)
show.influence(infl.db,test.VITD)

dfbetaPlots(test.VITD, ~freePlay*season, id.n=4, xlab="index", ylab="dfbeta for freePlay")
#observation 1324, 1315, 1503, 1967, 627, 1782, 1085, 760, 541.
VitD["1324",]
VitD["1315",]
VitD["1503",]
VitD["1967",]
VitD["1627",]
VitD["1782",]
VitD["1085",]
VitD["760",]
VitD["541",]

influencePlot(test.VITD)
plot(test.VITD, which=4, cook.levels=3)



