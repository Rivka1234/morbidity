#morb analysis PVD PSL Anameia - prevalnce data KK

rm(list = ls())

library(mgcv)
library(DHARMa)
library(tidyverse)
library(mgcViz)
library(ggplot2)
library(gratia)  # tidy tools for mgcv
library(lmtest)
library(broom)


data <- read.csv("data_FINAL.csv")

#make sure Malria and HW are factors

data$MalariaYN <- factor(data$MalariaYN)
data$HW_pos <- factor(data$HW_pos)
data$schisto_positive <-as.factor(data$schisto_positive)
#data$MalariaYN
#data$schisto_positive
#data$age
#data$HW_pos



data$PSL_scoreBIN
data$AnaemiaYN

mean(data$schisto_positive)
mean(data$age)



#####PVD#########################################################################
#################################################################################

#Select columns I need
PHmodData <- 
  data %>%
  dplyr::select(MalariaYN, schisto_positive, 
                age, sex, HW_pos, PH_scoreBIN)

#Remove rows with NAs
PHmodData <- PHmodData[which(complete.cases(PHmodData)==T),]

#Try differnt gam families and compare, to compre need to use method =ML, once
#we have chosen the best model should go back to using method-REML or choose only smoothing parameter
#Thin splines - default, no knots
mod1 <- gam(PH_scoreBIN ~ 
              s(age, bs="tp") + HW_pos + MalariaYN+schisto_positive+sex,
            data = PHmodData, family = "binomial", method = "ML")


#Cubic regression - less complex than thin, with knots
mod2 <- gam(PH_scoreBIN ~ 
              s(age, bs="cr") + HW_pos + MalariaYN+schisto_positive+sex,
            data = PHmodData, family = "binomial", method = "ML")
#P splines - smoothing splines, good for evenly spaced knots
mod3 <- gam(PH_scoreBIN ~ 
              s(age, bs="ps") + HW_pos + MalariaYN+schisto_positive+sex,
            data = PHmodData, family = "binomial", method = "ML")

#use AIC test to compare models
AIC(mod1,mod2,mod3) #mod 2 wins

#Now test the smoothing metric

#REML
mod4 <- gam(PH_scoreBIN ~ 
              s(age, bs="tp") + HW_pos + MalariaYN+schisto_positive+sex,
            data = PHmodData, family = "binomial", method = "REML")
#sp=0.1
mod5 <- gam(PH_scoreBIN ~ 
              s(age, bs="cr", sp=0.1) + HW_pos + MalariaYN+schisto_positive+sex,
            data = PHmodData, family = "binomial")
#sp=1
mod6 <- gam(PH_scoreBIN ~ 
              s(age, bs="cr", sp=1) + HW_pos + MalariaYN+schisto_positive+sex,
            data = PHmodData, family = "binomial")

#sp=2
mod7 <- gam(PH_scoreBIN ~ 
              s(age, bs="cr", sp=2) + HW_pos + MalariaYN+schisto_positive+sex,
            data = PHmodData, family = "binomial")

#use AIC test to compare models
AIC(mod4,mod5,mod6, mod7) #mod 6 wins



plot(simulateResiduals(mod6)) #yes!

#check if with or without sex is best

mod6.s <- gam(PH_scoreBIN ~ 
              s(age, bs="cr", sp=1) + HW_pos + MalariaYN+schisto_positive,
            data = PHmodData, family = "binomial")
AIC(mod6,mod6.s) #with sex

plot(simulateResiduals(mod6)) #yes!

#best fit model
PH_mod_cr <- gam(PH_scoreBIN ~ 
                   s(age, bs="cr", sp=1) + 
                   HW_pos + 
                   MalariaYN+
                   sex+
                   schisto_positive,
                 data = PHmodData, 
                 family = "binomial")

par(mfrow = c(1,1))
plot(PH_mod_cr, residuals = T, pch = 1, cex = 1, se=T, shade =T)

draw(PH_mod_cr)
summary(PH_mod_cr)
coef(PH_mod_cr)
.

#Use gam check to check model fit too PVD
gam.check(PH_mod_cr)
concurvity(PH_mod_cr) 
#Concurvity diagnostics were performed to assess the degree of non-linear dependency among predictors in the GAM model. Concurvity is analogous to multicollinearity in linear models but applies to smooth terms, indicating how much one predictor (or smooth) can be approximated by others. Values close to 1 suggest high dependency, while values near 0 indicate independence. In this model, concurvity estimates were low for both smooth terms: s(schisto_positive) had an estimated concurvity of 0.08 (observed = 0.04), and s(age) had an estimated concurvity of 0.09 (observed = 0.06). The worst-case concurvity values (maximum at any point in the data) were also acceptably low (0.31–0.34). The parametric terms showed slightly higher concurvity (0.41), but this remained well below commonly used thresholds for concern (e.g., >0.7). These results suggest that the predictors in the model are sufficiently independent, and there is no evidence of problematic concurvity affecting model stability or interpretability.

#calculate probabilty of PH based on the model PVD
PHmodData$PROB<- mgcv::predict.gam(PH_mod_cr, type = "response")

# use the model to predict the trial wins and looses
PHmodData$SIM <- rbinom(n = nrow(PHmodData), size = 1, prob = PHmodData$PROB)

#make paper quality graphs of the dats and smooths for age PVD


draw(PH_mod_cr, select = "s(age)", rug = TRUE, residuals = T) +
  labs(title = NULL,
       x = "Age (years)",
       y = "Effect on log-odds of PVD")+
  theme_bw()


#####PSL#########################################################################
#################################################################################

#Select columns I need
PSLmodData <- 
  data %>%
  dplyr::select(MalariaYN, schisto_positive, 
                age, sex, HW_pos, PSL_scoreBIN)

#Remove rows with NAs
PSLmodData <- PSLmodData[which(complete.cases(PSLmodData)==T),]

#Try differnt gam families and compare, to compre need to use method =ML, once
#we have chosen the best model should go back to using method-REML or choose only smoothing parameter
#Thin splines - default, no knots
mod1 <- gam(PSL_scoreBIN ~ 
              s(age, bs="tp") + HW_pos + MalariaYN+schisto_positive+sex,
            data = PSLmodData, family = "binomial", method = "ML")
#Cubic regression - less complex than thin, with knots
mod2 <- gam(PSL_scoreBIN ~ 
              s(age, bs="cr") + HW_pos + MalariaYN+schisto_positive+sex,
            data = PSLmodData, family = "binomial", method = "ML")
#P splines - smoothing splines, good for evenly spaced knots
mod3 <- gam(PSL_scoreBIN ~ 
              s(age, bs="ps") + HW_pos + MalariaYN+schisto_positive+sex,
            data = PSLmodData, family = "binomial", method = "ML")

#use AIC test to compare models
AIC(mod1,mod2,mod3) #mod 3 wins

#Now test the smoothing metric

#REML
mod4 <- gam(PSL_scoreBIN ~ 
              s(age, bs="ps") + HW_pos + MalariaYN+schisto_positive+sex,
            data = PSLmodData, family = "binomial", method = "REML")
#sp=0.1
mod5 <- gam(PSL_scoreBIN ~ 
              s(age, bs="ps", sp=0.1) + HW_pos + MalariaYN+schisto_positive+sex,
            data = PSLmodData, family = "binomial")
#sp=1
mod6 <- gam(PSL_scoreBIN ~ 
              s(age, bs="ps", sp=1) + HW_pos + MalariaYN+schisto_positive+sex,
            data = PSLmodData, family = "binomial")

#sp=2
mod7 <- gam(PSL_scoreBIN ~ 
              s(age, bs="ps", sp=2) + HW_pos + MalariaYN+schisto_positive+sex,
            data = PSLmodData, family = "binomial")

#use AIC test to compare models
AIC(mod4,mod5,mod6, mod7) #mod 4 wins

#try dome interactions
mod9 <- gam(PSL_scoreBIN ~ 
              s(age, bs="ps", by=MalariaYN) + HW_pos + MalariaYN+schisto_positive+sex,
            data = PSLmodData, family = "binomial", method="ML")


mod10 <- gam(PSL_scoreBIN ~ 
               s(age, bs="ps", by=schisto_positive) + HW_pos + MalariaYN+schisto_positive+sex,
             data = PSLmodData, family = "binomial", method="ML")

#use AIC test to compare models
AIC(mod9,mod10) #mod 9 wins

#use dharma to check the model fit PVD
plot(simulateResiduals(mod9)) #yes

# Make sure these are factors
PSLmodData$MalariaYN <- factor(PSLmodData$MalariaYN)
PSLmodData$sex <- factor(PSLmodData$sex)


#best fit model
PSL_mod_ps <- gam(PSL_scoreBIN ~ 
                    s(age, bs = "ps") +
                    HW_pos +
                    schisto_positive +
                    sex +
                    MalariaYN,
                  data = PSLmodData, family = "binomial", method = "REML")


par(mfrow = c(2, 2))
plot(PSL_mod_ps, residuals = T, pch = 1, cex = 1, se=T, shade =T)

draw(PSL_mod_ps)
summary(PSL_mod_ps)
coef(PSL_mod_ps)

#use dharma to check the model fit PVD
plot(simulateResiduals(PSL_mod_ps))
# Model diagnostics using DHARMa showed no significant deviation from uniformity in the residuals (KS test p = 0.32), no overdispersion (p = 0.992), and no outliers. No problem detected between residual and predicted.

#Use gam check to check model fit too PVD
gam.check(PSL_mod_ps)
concurvity(PSL_mod_ps) 
# model shows no serious concurvity issues. It's safe to interpret the smooths independently, and your model structure looks solid.

#calculate probabilty of PH based on the model PVD
PSLmodData$PROB<- mgcv::predict.gam(PSL_mod_ps, type = "response")

# use the model to predict the trial wins and looses
PSLmodData$SIM <- rbinom(n = nrow(PSLmodData), size = 1, prob = PSLmodData$PROB)

#make paper quality graphs of the dats and smooths for age PVD


draw(PSL_mod_ps, select = "s(age)", rug = TRUE, residuals = T) +
  labs(title = NULL,
       x = "Age (years)",
       y = "Effect on log-odds of PSL")+
  theme_bw()



#ANAEMIA################################################################

#Select columns I need
ANmodData <- 
  data %>%
  dplyr::select(MalariaYN, schisto_positive, 
                age, sex,HW_pos, AnaemiaYN)

#Remove rows with NAs
ANmodData <- ANmodData[which(complete.cases(ANmodData)==T),]

#Try differnt gam families and compare, to compre need to use method =ML, once
#we have chosen the best model should go back to using method-REML or choose only smoothing parameter
#Thin splines - default, no knots
mod1 <- gam(AnaemiaYN ~ 
              s(age, bs="tp") + HW_pos + MalariaYN+schisto_positive+sex,
            data = ANmodData, family = "binomial", method = "ML")
#Cubic regression - less complex than thin, with knots
mod2 <- gam(AnaemiaYN ~ 
              s(age, bs="cr") + HW_pos + MalariaYN+schisto_positive+sex,
            data = ANmodData, family = "binomial", method = "ML")
#P splines - smoothing splines, good for evenly spaced knots
mod3 <- gam(AnaemiaYN ~ 
              s(age, bs="ps") + HW_pos + MalariaYN+schisto_positive+sex,
            data = ANmodData, family = "binomial", method = "ML")

#use AIC test to compare models
AIC(mod1,mod2,mod3) #all the same! use cr

#Now test the smoothing metric

#REML
mod4 <- gam(AnaemiaYN ~ 
              s(age, bs="cr") + HW_pos + MalariaYN+schisto_positive+sex,
            data = ANmodData, family = "binomial", method = "REML")
#sp=0.1
mod5 <- gam(AnaemiaYN ~ 
              s(age, bs="cr", sp=0.1) + HW_pos + MalariaYN+schisto_positive+sex,
            data = ANmodData, family = "binomial")
#sp=1
mod6 <- gam(AnaemiaYN ~ 
              s(age, bs="cr", sp=1) + HW_pos + MalariaYN+schisto_positive+sex,
            data = ANmodData, family = "binomial")

#sp=2
mod7 <- gam(AnaemiaYN ~ 
              s(age, bs="cr", sp=2) + HW_pos + MalariaYN+schisto_positive+sex,
            data = ANmodData, family = "binomial")

#use AIC test to compare models
AIC(mod4,mod5,mod6, mod7) #mod 4 wins

#now see if we need an interaction between malaria and age or mean intensity and age or both
mod8 <- gam(AnaemiaYN ~ 
              #interaction term
              s(age, bs="cr", by=schisto_positive) +
              #parametric effects
              HW_pos + MalariaYN+schisto_positive,
            data = ANmodData, family = "binomial", method="ML")

mod9 <- gam(AnaemiaYN ~ s(age,  bs="cr", by=MalariaYN) +
              HW_pos + MalariaYN+schisto_positive,
            data = ANmodData, family = "binomial", method="ML")


#use AIC test to compare models
AIC(mod8, mod9, mod4) #mod 9 wins



plot.gam(mod8)
plot.gam(AN_mod2) 
plot.gam(mod9)
plot.gam(mod10)

#use dharma to check the model fit PVD

plot(simulateResiduals(mod9)) #yes!

#But when I look at model output, age and malaria do not have a significant interation so will do model without

mod4s<-gam(AnaemiaYN ~ 
      s(age, bs="cr") + HW_pos + MalariaYN+schisto_positive+sex,
    data = ANmodData, family = "binomial", method = "REML")
#
#
#try without sex
mod4<-gam(AnaemiaYN ~ 
            s(age, bs="cr") + HW_pos + MalariaYN+schisto_positive,
          data = ANmodData, family = "binomial", method = "REML")

AIC(mod4, mod4s) #without sex is a better model fit
#model we will use based on dharma and residual plots

#
plot(simulateResiduals(mod4))


par(mfrow = c(2, 2))
plot(mod4, residuals = T, pch = 1, cex = 1, se=T, shade =T)

draw(mod4)
summary(mod4)
coef(mod4)


#Use gam check to check model fit too PVD
gam.check(mod4)
concurvity(mod4) 
# model shows no serious concurvity issues. It's safe to interpret the smooths independently, and your model structure looks solid.

#calculate probabilty of PH based on the model PVD
ANmodData$PROB<- mgcv::predict.gam(mod4, type = "response")

# use the model to predict the trial wins and looses
ANmodData$SIM <- rbinom(n = nrow(ANmodData), size = 1, prob = ANmodData$PROB)

#make paper quality graphs of the dats and smooths for age PVD


draw(mod4, select = "s(age)", rug = TRUE, residuals = T) +
  labs(title = NULL,
       x = "Age (years)",
       y = "Effect on log-odds of Anameia")+
  theme_bw()







