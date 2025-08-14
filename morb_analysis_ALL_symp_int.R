

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

#add in self reported symptoms#################################################

"symptoms.diarrhoea" ############################


#Select columns I need
Symp <- 
  data %>%
  dplyr::select(MalariaYN, mean_intensity_noNA, 
                age, HW_pos, "symptoms.diarrhoea", 
                "symptoms.blood_stool",
                "symptoms.pain_urine", 
                "symptoms.abdominal_pain", 
                "symptoms.nausea", 
                "symptoms.vomiting",
                "symptoms.fever",
                "symptoms.chills",  
                "symptoms.headache", 
                "symptoms.rash", 
                "symptoms.muscle_pain",
                "symptoms.diff_breath", 
                "symptoms.dizziness",
                "symptoms.weakness", 
                "symptoms.body_swelling" )

#Remove rows with NAs
Symp <- Symp[which(complete.cases(Symp)==T),]


#
#"symptoms.diarrhoea", ##########################

#Thin splines - default, no knots
mod1 <- gam(symptoms.diarrhoea ~ s(mean_intensity_noNA, bs="tp") +
              s(age, bs="tp") + HW_pos + MalariaYN,
            data = Symp, family = "binomial", method = "ML")
#Cubic regression - less complex than thin, with knots
mod2 <- gam(symptoms.diarrhoea ~ s(mean_intensity_noNA, bs="cr") +
              s(age, bs="cr") + HW_pos + MalariaYN,
            data = Symp, family = "binomial", method = "ML")
#P splines - smoothing splines, good for evenly spaced knots
mod3 <- gam(symptoms.diarrhoea ~ s(mean_intensity_noNA, bs="ps") +
              s(age, bs="ps") + HW_pos + MalariaYN,
            data = Symp, family = "binomial", method = "ML")

#use AIC test to compare models
AIC(mod1,mod2,mod3) #mod 3 wins

#Now test the smoothing metric

#REML
mod4 <- gam(symptoms.diarrhoea ~ s(mean_intensity_noNA, bs="ps") +
              s(age, bs="ps") + HW_pos + MalariaYN,
            data = Symp, family = "binomial", method = "REML")
#sp=0.1
mod5 <- gam(symptoms.diarrhoea ~ s(mean_intensity_noNA, bs="ps", sp=0.1) +
              s(age, bs="ps", sp=0.1) + HW_pos + MalariaYN,
            data = Symp, family = "binomial")
#sp=1
mod6 <- gam(symptoms.diarrhoea ~ s(mean_intensity_noNA, bs="ps", sp=1) +
              s(age, bs="ps", sp=1) + HW_pos + MalariaYN,
            data = Symp, family = "binomial")

#sp=2
mod7 <- gam(symptoms.diarrhoea ~ s(mean_intensity_noNA, bs="ps", sp=2) +
              s(age, bs="ps", sp=2) + HW_pos + MalariaYN,
            data = Symp, family = "binomial")

#use AIC test to compare models
AIC(mod4,mod5,mod6, mod7) #mod 4 wins, but not much differnce, check the graphs and mod5 looks better



plot(simulateResiduals(mod5)) #yes!

summary(mod5) #nothing


gam.check(mod5)
concurvity(mod5) 


#make paper quality graphs of the dats and smooths 


draw(mod5, select = "s(age)", rug = TRUE, residuals = T) +
  labs(title = NULL,
       x = "Age (years)",
       y = "Effect on log-odds of symptoms.diarrhoea")+
  theme_bw()

draw(mod5, select = "s(mean_intensity_noNA)", rug = TRUE, residuals = T) +
  labs(title = NULL,
       x = "Mean intensity",
       y = "Effect on log-odds of symptoms.diarrhoea")+
  theme_bw()



"symptoms.blood_stool" ###############################


#Thin splines - default, no knots
mod1 <- gam(symptoms.blood_stool ~ s(mean_intensity_noNA, bs="tp") +
              s(age, bs="tp") + HW_pos + MalariaYN,
            data = Symp, family = "binomial", method = "ML")
#Cubic regression - less complex than thin, with knots
mod2 <- gam(symptoms.blood_stool ~ s(mean_intensity_noNA, bs="cr") +
              s(age, bs="cr") + HW_pos + MalariaYN,
            data = Symp, family = "binomial", method = "ML")
#P splines - smoothing splines, good for evenly spaced knots
mod3 <- gam(symptoms.blood_stool ~ s(mean_intensity_noNA, bs="ps") +
              s(age, bs="ps") + HW_pos + MalariaYN,
            data = Symp, family = "binomial", method = "ML")

#use AIC test to compare models
AIC(mod1,mod2,mod3) #mod 3 wins

#Now test the smoothing metric

#REML
mod4 <- gam(symptoms.blood_stool ~ s(mean_intensity_noNA, bs="ps") +
              s(age, bs="ps") + HW_pos + MalariaYN,
            data = Symp, family = "binomial", method = "REML")
#sp=0.1
mod5 <- gam(symptoms.blood_stool ~ s(mean_intensity_noNA, bs="ps", sp=0.1) +
              s(age, bs="ps", sp=0.1, k=25) + HW_pos + MalariaYN,
            data = Symp, family = "binomial")
#sp=1
mod6 <- gam(symptoms.blood_stool ~ s(mean_intensity_noNA, bs="ps", sp=1) +
              s(age, bs="ps", sp=1, k=20) + HW_pos + MalariaYN,
            data = Symp, family = "binomial")

#sp=2
mod7 <- gam(symptoms.blood_stool ~ s(mean_intensity_noNA, bs="ps", sp=2) +
              s(age, bs="ps", sp=2) + HW_pos + MalariaYN,
            data = Symp, family = "binomial")

#use AIC test to compare models
AIC(mod4,mod5,mod6, mod7) #mod 4 wins, but gam check doesnt have enough knots for age, used mod5 with extra knots





plot(simulateResiduals(mod5)) #yes!

summary(mod5)


gam.check(mod5)
concurvity(mod5) 


#make paper quality graphs of the dats and smooths 


draw(mod5, select = "s(age)", rug = TRUE, residuals = T) +
  labs(title = NULL,
       x = "Age (years)",
       y = "Effect on log-odds of symptoms.blood_stool")+
  theme_bw()

draw(mod5, select = "s(mean_intensity_noNA)", rug = TRUE, residuals = T) +
  labs(title = NULL,
       x = "Mean intensity",
       y = "Effect on log-odds of symptoms.blood_stool")+
  theme_bw()

"symptoms.pain_urine" #######################  

#Thin splines - default, no knots
mod1 <- gam(symptoms.pain_urine ~ s(mean_intensity_noNA, bs="tp") +
              s(age, bs="tp") + HW_pos + MalariaYN,
            data = Symp, family = "binomial", method = "ML")
#Cubic regression - less complex than thin, with knots
mod2 <- gam(symptoms.pain_urine ~ s(mean_intensity_noNA, bs="cr") +
              s(age, bs="cr") + HW_pos + MalariaYN,
            data = Symp, family = "binomial", method = "ML")
#P splines - smoothing splines, good for evenly spaced knots
mod3 <- gam(symptoms.pain_urine ~ s(mean_intensity_noNA, bs="ps") +
              s(age, bs="ps") + HW_pos + MalariaYN,
            data = Symp, family = "binomial", method = "ML")

#use AIC test to compare models
AIC(mod1,mod2,mod3) #mod 3 wins

#Now test the smoothing metric

#REML
mod4 <- gam(symptoms.pain_urine ~ s(mean_intensity_noNA, bs="ps") +
              s(age, bs="ps") + HW_pos + MalariaYN,
            data = Symp, family = "binomial", method = "REML")
#sp=0.1
mod5 <- gam(symptoms.pain_urine ~ s(mean_intensity_noNA, bs="ps", sp=0.1) +
              s(age, bs="ps", sp=0.1) + HW_pos + MalariaYN,
            data = Symp, family = "binomial")
#sp=1
mod6 <- gam(symptoms.pain_urine ~ s(mean_intensity_noNA, bs="ps", sp=1) +
              s(age, bs="ps", sp=1) + HW_pos + MalariaYN,
            data = Symp, family = "binomial")

#sp=2
mod7 <- gam(symptoms.pain_urine ~ s(mean_intensity_noNA, bs="ps", sp=2) +
              s(age, bs="ps", sp=2) + HW_pos + MalariaYN,
            data = Symp, family = "binomial")

#use AIC test to compare models
AIC(mod4,mod5,mod6, mod7) #mod 7 wins



plot(simulateResiduals(mod7)) #yes!


summary(mod7)


gam.check(mod7)
concurvity(mod7) 


#make paper quality graphs of the dats and smooths 


draw(mod7, select = "s(age)", rug = TRUE, residuals = T) +
  labs(title = NULL,
       x = "Age (years)",
       y = "Effect on log-odds of symptoms.pain_urine")+
  theme_bw()

draw(mod4, select = "s(mean_intensity_noNA)", rug = TRUE, residuals = T) +
  labs(title = NULL,
       x = "Mean intensity",
       y = "Effect on log-odds of symptoms.pain_urine")+
  theme_bw()


"symptoms.abdominal_pain" #######################################

#Thin splines - default, no knots
mod1 <- gam(symptoms.abdominal_pain  ~ s(mean_intensity_noNA, bs="tp") +
              s(age, bs="tp") + HW_pos + MalariaYN,
            data = Symp, family = "binomial", method = "ML")
#Cubic regression - less complex than thin, with knots
mod2 <- gam(symptoms.abdominal_pain  ~ s(mean_intensity_noNA, bs="cr") +
              s(age, bs="cr") + HW_pos + MalariaYN,
            data = Symp, family = "binomial", method = "ML")
#P splines - smoothing splines, good for evenly spaced knots
mod3 <- gam(symptoms.abdominal_pain  ~ s(mean_intensity_noNA, bs="ps") +
              s(age, bs="ps") + HW_pos + MalariaYN,
            data = Symp, family = "binomial", method = "ML")

#use AIC test to compare models
AIC(mod1,mod2,mod3) #mod 2 wins

#Now test the smoothing metric

#REML
mod4 <- gam(symptoms.abdominal_pain  ~ s(mean_intensity_noNA, bs="cr") +
              s(age, bs="cr") + HW_pos + MalariaYN,
            data = Symp, family = "binomial", method = "REML")
#sp=0.1
mod5 <- gam(symptoms.abdominal_pain  ~ s(mean_intensity_noNA, bs="cr", sp=0.1) +
              s(age, bs="cr", sp=0.1) + HW_pos + MalariaYN,
            data = Symp, family = "binomial")
#sp=1
mod6 <- gam(symptoms.abdominal_pain  ~ s(mean_intensity_noNA, bs="cr", sp=1) +
              s(age, bs="cr", sp=1) + HW_pos + MalariaYN,
            data = Symp, family = "binomial")

#sp=2
mod7 <- gam(symptoms.abdominal_pain  ~ s(mean_intensity_noNA, bs="cr", sp=2) +
              s(age, bs="cr", sp=2) + HW_pos + MalariaYN,
            data = Symp, family = "binomial")

#use AIC test to compare models
AIC(mod4,mod5,mod6, mod7) #mod 4 wins



plot(simulateResiduals(mod4)) #yes!


summary(mod4)


gam.check(mod4)
concurvity(mod4) 


#make paper quality graphs of the dats and smooths 


draw(mod4, select = "s(age)", rug = TRUE, residuals = T) +
  labs(title = NULL,
       x = "Age (years)",
       y = "Effect on log-odds of symptoms.abdominal_pain ")+
  theme_bw()

draw(mod4, select = "s(mean_intensity_noNA)", rug = TRUE, residuals = T) +
  labs(title = NULL,
       x = "Mean intensity",
       y = "Effect on log-odds of symptoms.abdominal_pain ")+
  theme_bw()


"symptoms.nausea" #######################################
#Thin splines - default, no knots
mod1 <- gam(symptoms.nausea  ~ s(mean_intensity_noNA, bs="tp") +
              s(age, bs="tp") + HW_pos + MalariaYN,
            data = Symp, family = "binomial", method = "ML")
#Cubic regression - less complex than thin, with knots
mod2 <- gam(symptoms.nausea  ~ s(mean_intensity_noNA, bs="cr") +
              s(age, bs="cr") + HW_pos + MalariaYN,
            data = Symp, family = "binomial", method = "ML")
#P splines - smoothing splines, good for evenly spaced knots
mod3 <- gam(symptoms.nausea  ~ s(mean_intensity_noNA, bs="ps") +
              s(age, bs="ps") + HW_pos + MalariaYN,
            data = Symp, family = "binomial", method = "ML")

#use AIC test to compare models
AIC(mod1,mod2,mod3) #mod 3 wins

#Now test the smoothing metric

#REML
mod4 <- gam(symptoms.nausea  ~ s(mean_intensity_noNA, bs="ps") +
              s(age, bs="ps") + HW_pos + MalariaYN,
            data = Symp, family = "binomial", method = "REML")
#sp=0.1
mod5 <- gam(symptoms.nausea  ~ s(mean_intensity_noNA, bs="ps", sp=0.1) +
              s(age, bs="ps", sp=0.1) + HW_pos + MalariaYN,
            data = Symp, family = "binomial")
#sp=1
mod6 <- gam(symptoms.nausea  ~ s(mean_intensity_noNA, bs="ps", sp=1) +
              s(age, bs="ps", sp=1) + HW_pos + MalariaYN,
            data = Symp, family = "binomial")

#sp=2
mod7 <- gam(symptoms.nausea  ~ s(mean_intensity_noNA, bs="ps", sp=2) +
              s(age, bs="ps", sp=2) + HW_pos + MalariaYN,
            data = Symp, family = "binomial")

#use AIC test to compare models
AIC(mod4,mod5,mod6, mod7) #mod 4 wins



plot(simulateResiduals(mod4)) #yes!


summary(mod4)


gam.check(mod4)
concurvity(mod4) 


#make paper quality graphs of the dats and smooths 


draw(mod4, select = "s(age)", rug = TRUE, residuals = T) +
  labs(title = NULL,
       x = "Age (years)",
       y = "Effect on log-odds of symptoms.nausea ")+
  theme_bw()

draw(mod4, select = "s(mean_intensity_noNA)", rug = TRUE, residuals = T) +
  labs(title = NULL,
       x = "Mean intensity",
       y = "Effect on log-odds of symptoms.nausea ")+
  theme_bw()

"symptoms.vomiting" ################################
#Thin splines - default, no knots
mod1 <- gam(symptoms.vomiting  ~ s(mean_intensity_noNA, bs="tp") +
              s(age, bs="tp") + HW_pos + MalariaYN,
            data = Symp, family = "binomial", method = "ML")
#Cubic regression - less complex than thin, with knots
mod2 <- gam(symptoms.vomiting  ~ s(mean_intensity_noNA, bs="cr") +
              s(age, bs="cr") + HW_pos + MalariaYN,
            data = Symp, family = "binomial", method = "ML")
#P splines - smoothing splines, good for evenly spaced knots
mod3 <- gam(symptoms.vomiting  ~ s(mean_intensity_noNA, bs="ps") +
              s(age, bs="ps") + HW_pos + MalariaYN,
            data = Symp, family = "binomial", method = "ML")

#use AIC test to compare models
AIC(mod1,mod2,mod3) #mod 2 wins

#Now test the smoothing metric

#REML
mod4 <- gam(symptoms.vomiting  ~ s(mean_intensity_noNA, bs="cr") +
              s(age, bs="cr") + HW_pos + MalariaYN,
            data = Symp, family = "binomial", method = "REML")
#sp=0.1
mod5 <- gam(symptoms.vomiting  ~ s(mean_intensity_noNA, bs="cr", sp=0.1) +
              s(age, bs="cr", sp=0.1) + HW_pos + MalariaYN,
            data = Symp, family = "binomial")
#sp=1
mod6 <- gam(symptoms.vomiting  ~ s(mean_intensity_noNA, bs="cr", sp=1) +
              s(age, bs="cr", sp=1) + HW_pos + MalariaYN,
            data = Symp, family = "binomial")

#sp=2
mod7 <- gam(symptoms.vomiting  ~ s(mean_intensity_noNA, bs="cr", sp=2) +
              s(age, bs="cr", sp=2) + HW_pos + MalariaYN,
            data = Symp, family = "binomial")

#use AIC test to compare models
AIC(mod4,mod5,mod6, mod7) #mod 4 wins



plot(simulateResiduals(mod4)) #yes!


summary(mod4)


gam.check(mod4)
concurvity(mod4) 


#make paper quality graphs of the dats and smooths 


draw(mod4, select = "s(age)", rug = TRUE, residuals = T) +
  labs(title = NULL,
       x = "Age (years)",
       y = "Effect on log-odds of symptoms.nausea ")+
  theme_bw()

draw(mod4, select = "s(mean_intensity_noNA)", rug = TRUE, residuals = T) +
  labs(title = NULL,
       x = "Mean intensity",
       y = "Effect on log-odds of symptoms.nausea ")+
  theme_bw()



"symptoms.fever" ########################################
#Thin splines - default, no knots
mod1 <- gam(symptoms.fever  ~ s(mean_intensity_noNA, bs="tp") +
              s(age, bs="tp") + HW_pos + MalariaYN,
            data = Symp, family = "binomial", method = "ML")
#Cubic regression - less complex than thin, with knots
mod2 <- gam(symptoms.fever  ~ s(mean_intensity_noNA, bs="cr") +
              s(age, bs="cr") + HW_pos + MalariaYN,
            data = Symp, family = "binomial", method = "ML")
#P splines - smoothing splines, good for evenly spaced knots
mod3 <- gam(symptoms.fever  ~ s(mean_intensity_noNA, bs="ps") +
              s(age, bs="ps") + HW_pos + MalariaYN,
            data = Symp, family = "binomial", method = "ML")

#use AIC test to compare models
AIC(mod1,mod2,mod3) #no differnce choose cr

#Now test the smoothing metric

#REML
mod4 <- gam(symptoms.fever  ~ s(mean_intensity_noNA, bs="cr") +
              s(age, bs="cr") + HW_pos + MalariaYN,
            data = Symp, family = "binomial", method = "REML")
#sp=0.1
mod5 <- gam(symptoms.fever  ~ s(mean_intensity_noNA, bs="cr", sp=0.1) +
              s(age, bs="cr", sp=0.1) + HW_pos + MalariaYN,
            data = Symp, family = "binomial")
#sp=1
mod6 <- gam(symptoms.fever  ~ s(mean_intensity_noNA, bs="cr", sp=1) +
              s(age, bs="cr", sp=1) + HW_pos + MalariaYN,
            data = Symp, family = "binomial")

#sp=2
mod7 <- gam(symptoms.fever  ~ s(mean_intensity_noNA, bs="cr", sp=2) +
              s(age, bs="cr", sp=2) + HW_pos + MalariaYN,
            data = Symp, family = "binomial")

#use AIC test to compare models
AIC(mod4,mod5,mod6, mod7) #mod 4 wins



plot(simulateResiduals(mod4)) #yes!


summary(mod4)


gam.check(mod4)
concurvity(mod4) 


#make paper quality graphs of the dats and smooths 


draw(mod4, select = "s(age)", rug = TRUE, residuals = T) +
  labs(title = NULL,
       x = "Age (years)",
       y = "Effect on log-odds of symptoms.fever ")+
  theme_bw()

draw(mod4, select = "s(mean_intensity_noNA)", rug = TRUE, residuals = T) +
  labs(title = NULL,
       x = "Mean intensity",
       y = "Effect on log-odds of symptoms.fever ")+
  theme_bw()



"symptoms.chills"  ##########################################

#Thin splines - default, no knots
mod1 <- gam(symptoms.chills  ~ s(mean_intensity_noNA, bs="tp") +
              s(age, bs="tp") + HW_pos + MalariaYN,
            data = Symp, family = "binomial", method = "ML")
#Cubic regression - less complex than thin, with knots
mod2 <- gam(symptoms.chills  ~ s(mean_intensity_noNA, bs="cr") +
              s(age, bs="cr") + HW_pos + MalariaYN,
            data = Symp, family = "binomial", method = "ML")
#P splines - smoothing splines, good for evenly spaced knots
mod3 <- gam(symptoms.chills  ~ s(mean_intensity_noNA, bs="ps") +
              s(age, bs="ps") + HW_pos + MalariaYN,
            data = Symp, family = "binomial", method = "ML")

#use AIC test to compare models
AIC(mod1,mod2,mod2) #mod 2 wins

#Now test the smoothing metric

#REML
mod4 <- gam(symptoms.chills  ~ s(mean_intensity_noNA, bs="cr") +
              s(age, bs="cr") + HW_pos + MalariaYN,
            data = Symp, family = "binomial", method = "REML")
#sp=0.1
mod5 <- gam(symptoms.chills  ~ s(mean_intensity_noNA, bs="cr", sp=0.1) +
              s(age, bs="cr", sp=0.1) + HW_pos + MalariaYN,
            data = Symp, family = "binomial")
#sp=1
mod6 <- gam(symptoms.chills  ~ s(mean_intensity_noNA, bs="cr", sp=1) +
              s(age, bs="cr", sp=1) + HW_pos + MalariaYN,
            data = Symp, family = "binomial")

#sp=2
mod7 <- gam(symptoms.chills  ~ s(mean_intensity_noNA, bs="cr", sp=2) +
              s(age, bs="cr", sp=2) + HW_pos + MalariaYN,
            data = Symp, family = "binomial")

#use AIC test to compare models
AIC(mod4,mod5,mod6, mod7) #mod 4 wins



plot(simulateResiduals(mod4)) #yes!


summary(mod4)


gam.check(mod4)
concurvity(mod4) 


#make paper quality graphs of the dats and smooths 


draw(mod4, select = "s(age)", rug = TRUE, residuals = T) +
  labs(title = NULL,
       x = "Age (years)",
       y = "Effect on log-odds of symptoms.chills ")+
  theme_bw()

draw(mod4, select = "s(mean_intensity_noNA)", rug = TRUE, residuals = T) +
  labs(title = NULL,
       x = "Mean intensity",
       y = "Effect on log-odds of symptoms.chills ")+
  theme_bw()


"symptoms.headache" #########################################
#Thin splines - default, no knots
mod1 <- gam(symptoms.headache  ~ s(mean_intensity_noNA, bs="tp") +
              s(age, bs="tp") + HW_pos + MalariaYN,
            data = Symp, family = "binomial", method = "ML")
#Cubic regression - less complex than thin, with knots
mod2 <- gam(symptoms.headache  ~ s(mean_intensity_noNA, bs="cr") +
              s(age, bs="cr") + HW_pos + MalariaYN,
            data = Symp, family = "binomial", method = "ML")
#P splines - smoothing splines, good for evenly spaced knots
mod3 <- gam(symptoms.headache  ~ s(mean_intensity_noNA, bs="ps") +
              s(age, bs="ps") + HW_pos + MalariaYN,
            data = Symp, family = "binomial", method = "ML")

#use AIC test to compare models
AIC(mod1,mod2,mod3) #no differnce choose cr

#Now test the smoothing metric

#REML
mod4 <- gam(symptoms.headache  ~ s(mean_intensity_noNA, bs="cr") +
              s(age, bs="cr") + HW_pos + MalariaYN,
            data = Symp, family = "binomial", method = "REML")
#sp=0.1
mod5 <- gam(symptoms.headache  ~ s(mean_intensity_noNA, bs="cr", sp=0.1) +
              s(age, bs="cr", sp=0.1) + HW_pos + MalariaYN,
            data = Symp, family = "binomial")
#sp=1
mod6 <- gam(symptoms.headache  ~ s(mean_intensity_noNA, bs="cr", sp=1) +
              s(age, bs="cr", sp=1) + HW_pos + MalariaYN,
            data = Symp, family = "binomial")

#sp=2
mod7 <- gam(symptoms.headache  ~ s(mean_intensity_noNA, bs="cr", sp=2) +
              s(age, bs="cr", sp=2) + HW_pos + MalariaYN,
            data = Symp, family = "binomial")

#use AIC test to compare models
AIC(mod4,mod5,mod6, mod7) #mod 4 wins



plot(simulateResiduals(mod4)) #yes!


summary(mod4)


gam.check(mod4)
concurvity(mod4) 


#make paper quality graphs of the dats and smooths 


draw(mod4, select = "s(age)", rug = TRUE, residuals = T) +
  labs(title = NULL,
       x = "Age (years)",
       y = "Effect on log-odds of symptoms.headache ")+
  theme_bw()

draw(mod4, select = "s(mean_intensity_noNA)", rug = TRUE, residuals = T) +
  labs(title = NULL,
       x = "Mean intensity",
       y = "Effect on log-odds of symptoms.headache ")+
  theme_bw()


"symptoms.rash" ################################
#Thin splines - default, no knots
mod1 <- gam(symptoms.rash  ~ s(mean_intensity_noNA, bs="tp") +
              s(age, bs="tp") + HW_pos + MalariaYN,
            data = Symp, family = "binomial", method = "ML")
#Cubic regression - less complex than thin, with knots
mod2 <- gam(symptoms.rash  ~ s(mean_intensity_noNA, bs="cr") +
              s(age, bs="cr") + HW_pos + MalariaYN,
            data = Symp, family = "binomial", method = "ML")
#P splines - smoothing splines, good for evenly spaced knots
mod3 <- gam(symptoms.rash  ~ s(mean_intensity_noNA, bs="ps") +
              s(age, bs="ps") + HW_pos + MalariaYN,
            data = Symp, family = "binomial", method = "ML")

#use AIC test to compare models
AIC(mod1,mod2,mod3) #mod 2 wins

#Now test the smoothing metric

#REML
mod4 <- gam(symptoms.rash  ~ s(mean_intensity_noNA, bs="cr") +
              s(age, bs="cr") + HW_pos + MalariaYN,
            data = Symp, family = "binomial", method = "REML")
#sp=0.1
mod5 <- gam(symptoms.rash  ~ s(mean_intensity_noNA, bs="cr", sp=0.1) +
              s(age, bs="cr", sp=0.1) + HW_pos + MalariaYN,
            data = Symp, family = "binomial")
#sp=1
mod6 <- gam(symptoms.rash  ~ s(mean_intensity_noNA, bs="cr", sp=1) +
              s(age, bs="cr", sp=1) + HW_pos + MalariaYN,
            data = Symp, family = "binomial")

#sp=2
mod7 <- gam(symptoms.rash  ~ s(mean_intensity_noNA, bs="cr", sp=2) +
              s(age, bs="cr", sp=2) + HW_pos + MalariaYN,
            data = Symp, family = "binomial")

#use AIC test to compare models
AIC(mod4,mod5,mod6, mod7) #mod 4 wins



plot(simulateResiduals(mod4)) #yes!


summary(mod4) #age


gam.check(mod4)
concurvity(mod4) 


#make paper quality graphs of the dats and smooths 


draw(mod4, select = "s(age)", rug = TRUE, residuals = T) +
  labs(title = NULL,
       x = "Age (years)",
       y = "Effect on log-odds of symptoms.rash ")+
  theme_bw()

draw(mod4, select = "s(mean_intensity_noNA)", rug = TRUE, residuals = T) +
  labs(title = NULL,
       x = "Mean intensity",
       y = "Effect on log-odds of symptoms.rash ")+
  theme_bw()


"symptoms.muscle_pain" ##########################
#Thin splines - default, no knots
mod1 <- gam(symptoms.muscle_pain  ~ s(mean_intensity_noNA, bs="tp") +
              s(age, bs="tp") + HW_pos + MalariaYN,
            data = Symp, family = "binomial", method = "ML")
#Cubic regression - less complex than thin, with knots
mod2 <- gam(symptoms.muscle_pain  ~ s(mean_intensity_noNA, bs="cr") +
              s(age, bs="cr") + HW_pos + MalariaYN,
            data = Symp, family = "binomial", method = "ML")
#P splines - smoothing splines, good for evenly spaced knots
mod3 <- gam(symptoms.muscle_pain  ~ s(mean_intensity_noNA, bs="ps") +
              s(age, bs="ps") + HW_pos + MalariaYN,
            data = Symp, family = "binomial", method = "ML")

#use AIC test to compare models
AIC(mod1,mod2,mod3) #no differnce choose cr

#Now test the smoothing metric

#REML
mod4 <- gam(symptoms.muscle_pain  ~ s(mean_intensity_noNA, bs="cr") +
              s(age, bs="cr") + HW_pos + MalariaYN,
            data = Symp, family = "binomial", method = "REML")
#sp=0.1
mod5 <- gam(symptoms.muscle_pain  ~ s(mean_intensity_noNA, bs="cr", sp=0.1) +
              s(age, bs="cr", sp=0.1) + HW_pos + MalariaYN,
            data = Symp, family = "binomial")
#sp=1
mod6 <- gam(symptoms.muscle_pain  ~ s(mean_intensity_noNA, bs="cr", sp=1) +
              s(age, bs="cr", sp=1) + HW_pos + MalariaYN,
            data = Symp, family = "binomial")

#sp=2
mod7 <- gam(symptoms.muscle_pain  ~ s(mean_intensity_noNA, bs="cr", sp=2) +
              s(age, bs="cr", sp=2) + HW_pos + MalariaYN,
            data = Symp, family = "binomial")

#use AIC test to compare models
AIC(mod4,mod5,mod6, mod7) #mod 4 wins



plot(simulateResiduals(mod4)) #some issues try others

plot(simulateResiduals(mod1))#good
plot(simulateResiduals(mod5)) #good use this
plot(simulateResiduals(mod6)) #good
plot(simulateResiduals(mod7)) #good

summary(mod5)


gam.check(mod5)
concurvity(mod5) 


#make paper quality graphs of the dats and smooths 


draw(mod5, select = "s(age)", rug = TRUE, residuals = T) +
  labs(title = NULL,
       x = "Age (years)",
       y = "Effect on log-odds of symptoms.muscle_pain ")+
  theme_bw()

draw(mod5, select = "s(mean_intensity_noNA)", rug = TRUE, residuals = T) +
  labs(title = NULL,
       x = "Mean intensity",
       y = "Effect on log-odds of symptoms.muscle_pain ")+
  theme_bw()


"symptoms.diff_breath" #######################################
#Thin splines - default, no knots
mod1 <- gam(symptoms.diff_breath  ~ s(mean_intensity_noNA, bs="tp") +
              s(age, bs="tp") + HW_pos + MalariaYN,
            data = Symp, family = "binomial", method = "ML")
#Cubic regression - less complex than thin, with knots
mod2 <- gam(symptoms.diff_breath  ~ s(mean_intensity_noNA, bs="cr") +
              s(age, bs="cr") + HW_pos + MalariaYN,
            data = Symp, family = "binomial", method = "ML")
#P splines - smoothing splines, good for evenly spaced knots
mod3 <- gam(symptoms.diff_breath  ~ s(mean_intensity_noNA, bs="ps") +
              s(age, bs="ps") + HW_pos + MalariaYN,
            data = Symp, family = "binomial", method = "ML")

#use AIC test to compare models
AIC(mod1,mod2,mod3) #no differnce choose cr

#Now test the smoothing metric

#REML
mod4 <- gam(symptoms.diff_breath  ~ s(mean_intensity_noNA, bs="cr") +
              s(age, bs="cr") + HW_pos + MalariaYN,
            data = Symp, family = "binomial", method = "REML")
#sp=0.1
mod5 <- gam(symptoms.diff_breath  ~ s(mean_intensity_noNA, bs="cr", sp=0.1) +
              s(age, bs="cr", sp=0.1) + HW_pos + MalariaYN,
            data = Symp, family = "binomial")
#sp=1
mod6 <- gam(symptoms.diff_breath  ~ s(mean_intensity_noNA, bs="cr", sp=1) +
              s(age, bs="cr") + HW_pos + MalariaYN,
            data = Symp, family = "binomial")

#sp=2
mod7 <- gam(symptoms.diff_breath  ~ s(mean_intensity_noNA, bs="cr", sp=2) +
              s(age, bs="cr", sp=2) + HW_pos + MalariaYN,
            data = Symp, family = "binomial")


summary(mod7)

#use AIC test to compare models
AIC(mod4,mod5,mod6, mod7) #mod 6 wins but gam check sign for mean intensity so need to add some knots, still not working I think too few people, just se mod6



plot(simulateResiduals(mod6)) #yes!


summary(mod6)


gam.check(mod6)
concurvity(mod6) 


#make paper quality graphs of the dats and smooths 


draw(mod6, select = "s(age)", rug = TRUE, residuals = T) +
  labs(title = NULL,
       x = "Age (years)",
       y = "Effect on log-odds of symptoms.diff_breath ")+
  theme_bw()

draw(mod6, select = "s(mean_intensity_noNA)", rug = TRUE, residuals = T) +
  labs(title = NULL,
       x = "Mean intensity",
       y = "Effect on log-odds of symptoms.diff_breath ")+
  theme_bw()


"symptoms.dizziness" #########################
#Thin splines - default, no knots
mod1 <- gam(symptoms.dizziness  ~ s(mean_intensity_noNA, bs="tp") +
              s(age, bs="tp") + HW_pos + MalariaYN,
            data = Symp, family = "binomial", method = "ML")
#Cubic regression - less complex than thin, with knots
mod2 <- gam(symptoms.dizziness  ~ s(mean_intensity_noNA, bs="cr") +
              s(age, bs="cr") + HW_pos + MalariaYN,
            data = Symp, family = "binomial", method = "ML")
#P splines - smoothing splines, good for evenly spaced knots
mod3 <- gam(symptoms.dizziness  ~ s(mean_intensity_noNA, bs="ps") +
              s(age, bs="ps") + HW_pos + MalariaYN,
            data = Symp, family = "binomial", method = "ML")

#use AIC test to compare models
AIC(mod1,mod2,mod3) #model1

#Now test the smoothing metric

#REML
mod4 <- gam(symptoms.dizziness  ~ s(mean_intensity_noNA, bs="tp") +
              s(age, bs="tp") + HW_pos + MalariaYN,
            data = Symp, family = "binomial", method = "REML")
#sp=0.1
mod5 <- gam(symptoms.dizziness  ~ s(mean_intensity_noNA, bs="tp", sp=0.1) +
              s(age, bs="tp", sp=0.1) + HW_pos + MalariaYN,
            data = Symp, family = "binomial")
#sp=1
mod6 <- gam(symptoms.dizziness  ~ s(mean_intensity_noNA, bs="tp", sp=1) +
              s(age, bs="tp", sp=1) + HW_pos + MalariaYN,
            data = Symp, family = "binomial")

#sp=2
mod7 <- gam(symptoms.dizziness  ~ s(mean_intensity_noNA, bs="tp", sp=2) +
              s(age, bs="tp", sp=2) + HW_pos + MalariaYN,
            data = Symp, family = "binomial")

#use AIC test to compare models
AIC(mod4,mod5,mod6, mod7) #mod 4 wins



plot(simulateResiduals(mod4)) #yes!


summary(mod4) #age


gam.check(mod4)
concurvity(mod4) 


#make paper quality graphs of the dats and smooths 


draw(mod4, select = "s(age)", rug = TRUE, residuals = T) +
  labs(title = NULL,
       x = "Age (years)",
       y = "Effect on log-odds of symptoms.dizziness ")+
  theme_bw()

draw(mod4, select = "s(mean_intensity_noNA)", rug = TRUE, residuals = T) +
  labs(title = NULL,
       x = "Mean intensity",
       y = "Effect on log-odds of symptoms.dizziness ")+
  theme_bw()


"symptoms.weakness" ###########################
#Thin splines - default, no knots
mod1 <- gam(symptoms.weakness  ~ s(mean_intensity_noNA, bs="tp") +
              s(age, bs="tp") + HW_pos + MalariaYN,
            data = Symp, family = "binomial", method = "ML")
#Cubic regression - less complex than thin, with knots
mod2 <- gam(symptoms.weakness  ~ s(mean_intensity_noNA, bs="cr") +
              s(age, bs="cr") + HW_pos + MalariaYN,
            data = Symp, family = "binomial", method = "ML")
#P splines - smoothing splines, good for evenly spaced knots
mod3 <- gam(symptoms.weakness  ~ s(mean_intensity_noNA, bs="ps") +
              s(age, bs="ps") + HW_pos + MalariaYN,
            data = Symp, family = "binomial", method = "ML")

#use AIC test to compare models
AIC(mod1,mod2,mod3) #no differnce choose cr

#Now test the smoothing metric

#REML
mod4 <- gam(symptoms.weakness  ~ s(mean_intensity_noNA, bs="cr") +
              s(age, bs="cr") + HW_pos + MalariaYN,
            data = Symp, family = "binomial", method = "REML")
#sp=0.1
mod5 <- gam(symptoms.weakness  ~ s(mean_intensity_noNA, bs="cr", sp=0.1) +
              s(age, bs="cr", sp=0.1) + HW_pos + MalariaYN,
            data = Symp, family = "binomial")
#sp=1
mod6 <- gam(symptoms.weakness  ~ s(mean_intensity_noNA, bs="cr", sp=1) +
              s(age, bs="cr", sp=1) + HW_pos + MalariaYN,
            data = Symp, family = "binomial")

#sp=2
mod7 <- gam(symptoms.weakness  ~ s(mean_intensity_noNA, bs="cr", sp=2) +
              s(age, bs="cr", sp=2) + HW_pos + MalariaYN,
            data = Symp, family = "binomial")

#use AIC test to compare models
AIC(mod4,mod5,mod6, mod7) #mod 4 wins



plot(simulateResiduals(mod4)) #yes!


summary(mod4) #age


gam.check(mod4)
concurvity(mod4) 


#make paper quality graphs of the dats and smooths 


draw(mod4, select = "s(age)", rug = TRUE, residuals = T) +
  labs(title = NULL,
       x = "Age (years)",
       y = "Effect on log-odds of symptoms.weakness ")+
  theme_bw()

draw(mod4, select = "s(mean_intensity_noNA)", rug = TRUE, residuals = T) +
  labs(title = NULL,
       x = "Mean intensity",
       y = "Effect on log-odds of symptoms.weakness ")+
  theme_bw()



"symptoms.body_swelling"  ########################

#Thin splines - default, no knots
mod1 <- gam(symptoms.body_swelling  ~ s(mean_intensity_noNA, bs="tp") +
              s(age, bs="tp") + HW_pos + MalariaYN,
            data = Symp, family = "binomial", method = "ML")
#Cubic regression - less complex than thin, with knots
mod2 <- gam(symptoms.body_swelling  ~ s(mean_intensity_noNA, bs="cr") +
              s(age, bs="cr") + HW_pos + MalariaYN,
            data = Symp, family = "binomial", method = "ML")
#P splines - smoothing splines, good for evenly spaced knots
mod3 <- gam(symptoms.body_swelling  ~ s(mean_intensity_noNA, bs="ps") +
              s(age, bs="ps") + HW_pos + MalariaYN,
            data = Symp, family = "binomial", method = "ML")

#use AIC test to compare models
AIC(mod1,mod2,mod3) #no differnce choose cr

#Now test the smoothing metric

#REML
mod4 <- gam(symptoms.body_swelling  ~ s(mean_intensity_noNA, bs="cr") +
              s(age, bs="cr") + HW_pos + MalariaYN,
            data = Symp, family = "binomial", method = "REML")
#sp=0.1
mod5 <- gam(symptoms.body_swelling  ~ s(mean_intensity_noNA, bs="cr", sp=0.1) +
              s(age, bs="cr", sp=0.1) + HW_pos + MalariaYN,
            data = Symp, family = "binomial")
#sp=1
mod6 <- gam(symptoms.body_swelling  ~ s(mean_intensity_noNA, bs="cr", sp=1) +
              s(age, bs="cr", sp=1) + HW_pos + MalariaYN,
            data = Symp, family = "binomial")

#sp=2
mod7 <- gam(symptoms.body_swelling  ~ s(mean_intensity_noNA, bs="cr", sp=2) +
              s(age, bs="cr", sp=2) + HW_pos + MalariaYN,
            data = Symp, family = "binomial")

#use AIC test to compare models
AIC(mod4,mod5,mod6, mod7) #mod 4 wins



plot(simulateResiduals(mod4)) #yes!


summary(mod4)


gam.check(mod4)
concurvity(mod4) 


#make paper quality graphs of the dats and smooths 


draw(mod4, select = "s(age)", rug = TRUE, residuals = T) +
  labs(title = NULL,
       x = "Age (years)",
       y = "Effect on log-odds of symptoms.body_swelling ")+
  theme_bw()

draw(mod4, select = "s(mean_intensity_noNA)", rug = TRUE, residuals = T) +
  labs(title = NULL,
       x = "Mean intensity",
       y = "Effect on log-odds of symptoms.body_swelling ")+
  theme_bw()
