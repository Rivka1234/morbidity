
#prev kk only with symtpoms
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
  dplyr::select(MalariaYN, schisto_positive, CCA_pos,
                age, HW_pos,ageCLASS, "symptoms.diarrhoea", 
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
                "symptoms.body_swelling",sex ) 

#Remove rows with NAs
Symp <- Symp[which(complete.cases(Symp)==T),]

#which symtpoms are never seen with malaria and hookworm?

# Define the list of symptom columns
symptom_cols <- c("symptoms.diarrhoea", 
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
                  "symptoms.body_swelling")

# Check for symptoms that never co-occur with malaria
malaria_check <- sapply(symptom_cols, function(symptom) {
  all(Symp[[symptom]][Symp$MalariaYN == 1] == 0)
})

# Check for symptoms that never co-occur with hookworm
hookworm_check <- sapply(symptom_cols, function(symptom) {
  all(Symp[[symptom]][Symp$HW_pos == 1] == 0)
})

# Combine into a data frame
check_df <- data.frame(
  Symptom = symptom_cols,
  NoOverlapWithMalaria = malaria_check,
  NoOverlapWithHookworm = hookworm_check
)

# View the result
print(check_df)

#difficuty breathing and body swell, no one with hookworm said this,checked models with and without and no difference
#

#
#"symptoms.diarrhoea", ##########################

#Thin splines - default, no knots
mod1 <- gam(symptoms.diarrhoea ~ 
              s(age, bs="tp") + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial", method = "ML")
#Cubic regression - less complex than thin, with knots
mod2 <- gam(symptoms.diarrhoea ~ 
              s(age, bs="cr") + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial", method = "ML")
#P splines - smoothing splines, good for evenly spaced knots
mod3 <- gam(symptoms.diarrhoea ~ 
              s(age, bs="ps") + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial", method = "ML")

#use AIC test to compare models
AIC(mod1,mod2,mod3) #no differnce use cr

#Now test the smoothing metric

#REML
mod4s <- gam(symptoms.diarrhoea ~ 
              s(age, bs="ps") + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial", method = "REML")
#sp=0.1
mod5 <- gam(symptoms.diarrhoea ~ 
              s(age, bs="ps", sp=0.1) + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial")
#sp=1
mod6 <- gam(symptoms.diarrhoea ~ 
              s(age, bs="ps", sp=1) + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial")

#sp=2
mod7 <- gam(symptoms.diarrhoea ~ 
              s(age, bs="ps", sp=2) + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial")

#use AIC test to compare models
AIC(mod4,mod5,mod6, mod7) #mod 4 wins, 


plot(simulateResiduals(mod4)) #yes!

summary(mod4) #age


gam.check(mod4)
concurvity(mod4) 


#make paper quality graphs of the dats and smooths 


draw(mod4, select = "s(age)", rug = TRUE, residuals = T) +
  labs(title = NULL,
       x = "Age (years)",
       y = "Effect on log-odds of symptoms.diarrhoea")+
  theme_bw()





#blood_stool" ###############################


#Thin splines - default, no knots
mod1 <- gam(symptoms.blood_stool ~ 
              s(age, bs="tp") + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial", method = "ML")
#Cubic regression - less complex than thin, with knots
mod2 <- gam(symptoms.blood_stool ~ 
              s(age, bs="cr") + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial", method = "ML")
#P splines - smoothing splines, good for evenly spaced knots
mod3 <- gam(symptoms.blood_stool ~ 
              s(age, bs="ps") + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial", method = "ML")

#use AIC test to compare models
AIC(mod1,mod2,mod3) #mod 3 wins

#Now test the smoothing metric

#REML
mod4 <- gam(symptoms.blood_stool ~ 
              s(age, bs="ps") + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial", method = "REML")
#sp=0.1
mod5 <- gam(symptoms.blood_stool ~ 
              s(age, bs="ps", sp=0.1, k=20) + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial")
#sp=1
mod6 <- gam(symptoms.blood_stool ~ 
              s(age, bs="ps", sp=1) + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial")

#sp=2
mod7 <- gam(symptoms.blood_stool ~ 
              s(age, bs="ps", sp=2) + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial")

#use AIC test to compare models
AIC(mod4,mod5,mod6, mod7) #mod 5 wins, but gam check doesnt have enough knots for age, used mod5 with extra knots



gam.check(mod5)
concurvity(mod5) 



plot(simulateResiduals(mod5)) #yes!

summary(mod5) #nothing



#make paper quality graphs of the dats and smooths 


draw(mod5, select = "s(age)", rug = TRUE, residuals = T) +
  labs(title = NULL,
       x = "Age (years)",
       y = "Effect on log-odds of symptoms.blood_stool")+
  theme_bw()


"symptoms.pain_urine" #######################  

#Thin splines - default, no knots
mod1 <- gam(symptoms.pain_urine ~ 
              s(age, bs="tp") + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial", method = "ML")
#Cubic regression - less complex than thin, with knots
mod2 <- gam(symptoms.pain_urine ~ 
              s(age, bs="cr") + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial", method = "ML")
#P splines - smoothing splines, good for evenly spaced knots
mod3 <- gam(symptoms.pain_urine ~ 
              s(age, bs="ps") + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial", method = "ML")

#use AIC test to compare models
AIC(mod1,mod2,mod3) #mod 2 wins

#Now test the smoothing metric

#REML
mod4 <- gam(symptoms.pain_urine ~ 
              s(age, bs="cr") + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial", method = "REML")
#sp=0.1
mod5 <- gam(symptoms.pain_urine ~ 
              s(age, bs="cr", sp=0.1) + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial")
#sp=1
mod6 <- gam(symptoms.pain_urine ~ 
              s(age, bs="cr", sp=1) + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial")

#sp=2
mod7 <- gam(symptoms.pain_urine ~ 
              s(age, bs="cr", sp=2) + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial")

#use AIC test to compare models
AIC(mod4,mod5,mod6, mod7) #mod4 wins but dharma plot horrible, mod 7 same AIC so try that



plot(simulateResiduals(mod7)) #yes!


summary(mod7) #age


gam.check(mod7)
concurvity(mod7) 


#make paper quality graphs of the dats and smooths 


draw(mod7, select = "s(age)", rug = TRUE, residuals = T) +
  labs(title = NULL,
       x = "Age (years)",
       y = "Effect on log-odds of symptoms.pain_urine")+
  theme_bw()



"symptoms.abdominal_pain" #######################################

#Thin splines - default, no knots
mod1 <- gam(symptoms.abdominal_pain  ~ 
              s(age, bs="tp") + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial", method = "ML")
#Cubic regression - less complex than thin, with knots
mod2 <- gam(symptoms.abdominal_pain  ~ 
              s(age, bs="cr") + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial", method = "ML")
#P splines - smoothing splines, good for evenly spaced knots
mod3 <- gam(symptoms.abdominal_pain  ~ 
              s(age, bs="ps") + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial", method = "ML")

#use AIC test to compare models
AIC(mod1,mod2,mod3) #mod 2 wins

#Now test the smoothing metric

#REML
mod4 <- gam(symptoms.abdominal_pain  ~ 
              s(age, bs="cr") + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial", method = "REML")
#sp=0.1
mod5 <- gam(symptoms.abdominal_pain  ~ 
              s(age, bs="cr", sp=0.1) + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial")
#sp=1
mod6 <- gam(symptoms.abdominal_pain  ~ 
              s(age, bs="cr", sp=1) + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial")

#sp=2
mod7 <- gam(symptoms.abdominal_pain  ~ 
              s(age, bs="cr", sp=2) + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial")

#use AIC test to compare models
AIC(mod4,mod5,mod6, mod7) #mod 6 or 7



plot(simulateResiduals(mod6)) #yes!


summary(mod6) #age


gam.check(mod6)
concurvity(mod6) 


#make paper quality graphs of the dats and smooths 


draw(mod6, select = "s(age)", rug = TRUE, residuals = T) +
  labs(title = NULL,
       x = "Age (years)",
       y = "Effect on log-odds of symptoms.abdominal_pain ")+
  theme_bw()




"symptoms.nausea" #######################################
#Thin splines - default, no knots
mod1 <- gam(symptoms.nausea  ~ 
              s(age, bs="tp") + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial", method = "ML")
#Cubic regression - less complex than thin, with knots
mod2 <- gam(symptoms.nausea  ~ 
              s(age, bs="cr") + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial", method = "ML")
#P splines - smoothing splines, good for evenly spaced knots
mod3 <- gam(symptoms.nausea  ~ 
              s(age, bs="ps") + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial", method = "ML")

#use AIC test to compare models
AIC(mod1,mod2,mod3) #mod 3 wins just

#Now test the smoothing metric

#REML
mod4 <- gam(symptoms.nausea  ~ 
              s(age, bs="ps") + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial", method = "REML")
#sp=0.1
mod5 <- gam(symptoms.nausea  ~ 
              s(age, bs="ps", sp=0.1) + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial")
#sp=1
mod6 <- gam(symptoms.nausea  ~ 
              s(age, bs="ps", sp=1) + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial")

#sp=2
mod7 <- gam(symptoms.nausea  ~ 
              s(age, bs="ps", sp=2) + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial")

#use AIC test to compare models
AIC(mod4,mod5,mod6, mod7) #mod 4 wins



plot(simulateResiduals(mod4)) #yes!


summary(mod4)


gam.check(mod4) #age and nearly malaria
concurvity(mod4) 


#make paper quality graphs of the dats and smooths 


draw(mod4, select = "s(age)", rug = TRUE, residuals = T) +
  labs(title = NULL,
       x = "Age (years)",
       y = "Effect on log-odds of symptoms.nausea ")+
  theme_bw()


"symptoms.vomiting" ################################
#Thin splines - default, no knots
mod1 <- gam(symptoms.vomiting  ~ 
              s(age, bs="tp") + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial", method = "ML")
#Cubic regression - less complex than thin, with knots
mod2 <- gam(symptoms.vomiting  ~ 
              s(age, bs="cr") + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial", method = "ML")
#P splines - smoothing splines, good for evenly spaced knots
mod3 <- gam(symptoms.vomiting  ~ 
              s(age, bs="ps") + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial", method = "ML")

#use AIC test to compare models
AIC(mod1,mod2,mod3) #mod 1

#Now test the smoothing metric

#REML
mod4 <- gam(symptoms.vomiting  ~ 
              s(age, bs="tp") + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial", method = "REML")
#sp=0.1
mod5 <- gam(symptoms.vomiting  ~ 
              s(age, bs="tp", sp=0.1) + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial")
#sp=1
mod6 <- gam(symptoms.vomiting  ~ 
              s(age, bs="tp", sp=1) + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial")

#sp=2
mod7 <- gam(symptoms.vomiting  ~ 
              s(age, bs="tp", sp=2) + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial")

#use AIC test to compare models
AIC(mod4,mod5,mod6, mod7) #mod 5 wins just



plot(simulateResiduals(mod5)) #yes!


summary(mod5) #malaria nearly


gam.check(mod5)
concurvity(mod5) 


#make paper quality graphs of the dats and smooths 


draw(mod5, select = "s(age)", rug = TRUE, residuals = T) +
  labs(title = NULL,
       x = "Age (years)",
       y = "Effect on log-odds of symptoms.nausea ")+
  theme_bw()


"symptoms.fever" ########################################
#Thin splines - default, no knots
mod1 <- gam(symptoms.fever  ~ 
              s(age, bs="tp") + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial", method = "ML")
#Cubic regression - less complex than thin, with knots
mod2 <- gam(symptoms.fever  ~ 
              s(age, bs="cr") + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial", method = "ML")
#P splines - smoothing splines, good for evenly spaced knots
mod3 <- gam(symptoms.fever  ~ 
              s(age, bs="ps") + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial", method = "ML")

#use AIC test to compare models
AIC(mod1,mod2,mod3) #no differnce choose cr

#Now test the smoothing metric

#REML
mod4 <- gam(symptoms.fever  ~ 
              s(age, bs="cr") + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial", method = "REML")
#sp=0.1
mod5 <- gam(symptoms.fever  ~ 
              s(age, bs="cr", sp=0.1) + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial")
#sp=1
mod6 <- gam(symptoms.fever  ~ 
              s(age, bs="cr", sp=1) + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial")

#sp=2
mod7 <- gam(symptoms.fever  ~ 
              s(age, bs="cr", sp=2) + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial")

#use AIC test to compare models
AIC(mod4,mod5,mod6, mod7) #mod 6 wins



plot(simulateResiduals(mod6)) #yes!


summary(mod6) #age nearly


gam.check(mod6)
concurvity(mod6) 


#make paper quality graphs of the dats and smooths 


draw(mod6, select = "s(age)", rug = TRUE, residuals = T) +
  labs(title = NULL,
       x = "Age (years)",
       y = "Effect on log-odds of symptoms.fever ")+
  theme_bw()


"symptoms.chills"  ##########################################

#Thin splines - default, no knots
mod1 <- gam(symptoms.chills  ~ 
              s(age, bs="tp") + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial", method = "ML")
#Cubic regression - less complex than thin, with knots
mod2 <- gam(symptoms.chills  ~ 
              s(age, bs="cr") + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial", method = "ML")
#P splines - smoothing splines, good for evenly spaced knots
mod3 <- gam(symptoms.chills  ~ 
              s(age, bs="ps") + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial", method = "ML")

#use AIC test to compare models
AIC(mod1,mod2,mod3) #mod 2 wins

#Now test the smoothing metric

#REML
mod4 <- gam(symptoms.chills  ~ 
              s(age, bs="cr") + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial", method = "REML")
#sp=0.1
mod5 <- gam(symptoms.chills  ~ 
              s(age, bs="cr", sp=0.1) + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial")
#sp=1
mod6 <- gam(symptoms.chills  ~ 
              s(age, bs="cr", sp=1) + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial")

#sp=2
mod7 <- gam(symptoms.chills  ~ 
              s(age, bs="cr", sp=2) + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial")

#use AIC test to compare models
AIC(mod4,mod5,mod6, mod7) #mod 6 wins



plot(simulateResiduals(mod6)) #yes!


summary(mod6) #age


gam.check(mod6)
concurvity(mod6) 


#make paper quality graphs of the dats and smooths 


draw(mod6, select = "s(age)", rug = TRUE, residuals = T) +
  labs(title = NULL,
       x = "Age (years)",
       y = "Effect on log-odds of symptoms.chills ")+
  theme_bw()




"symptoms.headache" #########################################
#Thin splines - default, no knots
mod1 <- gam(symptoms.headache  ~ 
              s(age, bs="tp") + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial", method = "ML")
#Cubic regression - less complex than thin, with knots
mod2 <- gam(symptoms.headache  ~ 
              s(age, bs="cr") + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial", method = "ML")
#P splines - smoothing splines, good for evenly spaced knots
mod3 <- gam(symptoms.headache  ~ 
              s(age, bs="ps") + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial", method = "ML")

#use AIC test to compare models
AIC(mod1,mod2,mod3) #no differnce choose cr

#Now test the smoothing metric

#REML
mod4 <- gam(symptoms.headache  ~ 
              s(age, bs="cr") + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial", method = "REML")
#sp=0.1
mod5 <- gam(symptoms.headache  ~ 
              s(age, bs="cr", sp=0.1) + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial")
#sp=1
mod6 <- gam(symptoms.headache  ~ 
              s(age, bs="cr", sp=1) + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial")

#sp=2
mod7 <- gam(symptoms.headache  ~ 
              s(age, bs="cr", sp=2) + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial")

#use AIC test to compare models
AIC(mod4,mod5,mod6, mod7) #mod 4 wins



plot(simulateResiduals(mod4)) #yes!


summary(mod4) #hookworm nearly neg, schisto pos nearly yes


gam.check(mod4)
concurvity(mod4) 


#make paper quality graphs of the dats and smooths 


draw(mod4, select = "s(age)", rug = TRUE, residuals = T) +
  labs(title = NULL,
       x = "Age (years)",
       y = "Effect on log-odds of symptoms.headache ")+
  theme_bw()



#rash" ################################
#Thin splines - default, no knots
mod1 <- gam(symptoms.rash  ~ 
              s(age, bs="tp") + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial", method = "ML")
#Cubic regression - less complex than thin, with knots
mod2 <- gam(symptoms.rash  ~ 
              s(age, bs="cr") + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial", method = "ML")
#P splines - smoothing splines, good for evenly spaced knots
mod3 <- gam(symptoms.rash  ~ 
              s(age, bs="ps") + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial", method = "ML")

#use AIC test to compare models
AIC(mod1,mod2,mod3) #mod 2 wins

#Now test the smoothing metric

#REML
mod4 <- gam(symptoms.rash  ~ 
              s(age, bs="cr") + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial", method = "REML")
#sp=0.1
mod5 <- gam(symptoms.rash  ~ 
              s(age, bs="cr", sp=0.1) + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial")
#sp=1
mod6 <- gam(symptoms.rash  ~ 
              s(age, bs="cr", sp=1) + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial")

#sp=2
mod7 <- gam(symptoms.rash  ~ 
              s(age, bs="cr", sp=2) + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial")

#use AIC test to compare models
AIC(mod4,mod5,mod6, mod7) #mod 5 wins



plot(simulateResiduals(mod5)) #yes!


summary(mod5) #age
mod5s <- gam(symptoms.rash  ~ 
              s(age, bs="cr", sp=0.1) + HW_pos + MalariaYN+schisto_positive+sex,
            data = Symp, family = "binomial")

summary(mod5s)

gam.check(mod5)
concurvity(mod5) 


#make paper quality graphs of the dats and smooths 


draw(mod5, select = "s(age)", rug = TRUE, residuals = T) +
  labs(title = NULL,
       x = "Age (years)",
       y = "Effect on log-odds of symptoms.rash ")+
  theme_bw()



"symptoms.muscle_pain" ##########################
#Thin splines - default, no knots
mod1 <- gam(symptoms.muscle_pain  ~ 
              s(age, bs="tp") + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial", method = "ML")
#Cubic regression - less complex than thin, with knots
mod2 <- gam(symptoms.muscle_pain  ~ 
              s(age, bs="cr") + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial", method = "ML")
#P splines - smoothing splines, good for evenly spaced knots
mod3 <- gam(symptoms.muscle_pain  ~ 
              s(age, bs="ps") + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial", method = "ML")

#use AIC test to compare models
AIC(mod1,mod2,mod3) #mod 1

#Now test the smoothing metric

#REML
mod4 <- gam(symptoms.muscle_pain  ~ 
              s(age, bs="tp") + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial", method = "REML")
#sp=0.1
mod5 <- gam(symptoms.muscle_pain  ~ 
              s(age, bs="tp", sp=0.1) + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial")
#sp=1
mod6 <- gam(symptoms.muscle_pain  ~ 
              s(age, bs="tp", sp=1) + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial")

#sp=2
mod7 <- gam(symptoms.muscle_pain  ~ 
              s(age, bs="tp", sp=2) + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial")

#use AIC test to compare models
AIC(mod4,mod5,mod6, mod7) #mod 7 wins



plot(simulateResiduals(mod7)) #some issues try others


plot(simulateResiduals(mod5)) #good use this


summary(mod5)


gam.check(mod5)
concurvity(mod5) 


#make paper quality graphs of the dats and smooths 


draw(mod5, select = "s(age)", rug = TRUE, residuals = T) +
  labs(title = NULL,
       x = "Age (years)",
       y = "Effect on log-odds of symptoms.muscle_pain ")+
  theme_bw()




"symptoms.diff_breath" #######################################

#Thin splines - default, no knots
mod1 <- gam(symptoms.diff_breath  ~ 
              s(age, bs="tp") + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial", method = "ML")
#Cubic regression - less complex than thin, with knots
mod2 <- gam(symptoms.diff_breath  ~ 
              s(age, bs="cr") + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial", method = "ML")
#P splines - smoothing splines, good for evenly spaced knots
mod3 <- gam(symptoms.diff_breath  ~ 
              s(age, bs="ps") + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial", method = "ML")

#use AIC test to compare models
AIC(mod1,mod2,mod3) #no differnce choose cr

#Now test the smoothing metric

#REML
mod4 <- gam(symptoms.diff_breath  ~ 
              s(age, bs="cr") + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial", method = "REML")
#sp=0.1
mod5 <- gam(symptoms.diff_breath  ~ 
              s(age, bs="cr", sp=0.1) + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial")
#sp=1
mod6 <- gam(symptoms.diff_breath  ~ 
              s(age, bs="cr") + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial")

#sp=2
mod7 <- gam(symptoms.diff_breath  ~ 
              s(age, bs="cr", sp=2) + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial")



#use AIC test to compare models
AIC(mod4,mod5,mod6, mod7) #mod 6 wins 


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


"symptoms.dizziness" #########################
#Thin splines - default, no knots
mod1 <- gam(symptoms.dizziness  ~ 
              s(age, bs="tp") + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial", method = "ML")
#Cubic regression - less complex than thin, with knots
mod2 <- gam(symptoms.dizziness  ~ 
              s(age, bs="cr") + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial", method = "ML")
#P splines - smoothing splines, good for evenly spaced knots
mod3 <- gam(symptoms.dizziness  ~ 
              s(age, bs="ps") + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial", method = "ML")

#use AIC test to compare models
AIC(mod1,mod2,mod3) #no diff use cr

#Now test the smoothing metric

#REML
mod4 <- gam(symptoms.dizziness  ~ 
              s(age, bs="cr") + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial", method = "REML")
#sp=0.1
mod5 <- gam(symptoms.dizziness  ~ 
              s(age, bs="tp", sp=0.1) + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial")
#sp=1
mod6 <- gam(symptoms.dizziness  ~ 
              s(age, bs="cr", sp=1) + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial")

#sp=2
mod7 <- gam(symptoms.dizziness  ~ 
              s(age, bs="cr", sp=2) + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial")

#use AIC test to compare models
AIC(mod4,mod5,mod6, mod7) #mod 5 wins



plot(simulateResiduals(mod5)) #yes!


summary(mod5) #age


gam.check(mod5)
concurvity(mod5) 


#make paper quality graphs of the dats and smooths 


draw(mod5, select = "s(age)", rug = TRUE, residuals = T) +
  labs(title = NULL,
       x = "Age (years)",
       y = "Effect on log-odds of symptoms.dizziness ")+
  theme_bw()



"symptoms.weakness" ###########################
#Thin splines - default, no knots
mod1 <- gam(symptoms.weakness  ~ 
              s(age, bs="tp") + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial", method = "ML")
#Cubic regression - less complex than thin, with knots
mod2 <- gam(symptoms.weakness  ~ 
              s(age, bs="cr") + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial", method = "ML")
#P splines - smoothing splines, good for evenly spaced knots
mod3 <- gam(symptoms.weakness  ~ 
              s(age, bs="ps") + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial", method = "ML")

#use AIC test to compare models
AIC(mod1,mod2,mod3) #mod 2

#Now test the smoothing metric

#REML
mod4 <- gam(symptoms.weakness  ~ 
              s(age, bs="cr") + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial", method = "REML")
#sp=0.1
mod5 <- gam(symptoms.weakness  ~ 
              s(age, bs="cr", sp=0.1) + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial")
#sp=1
mod6 <- gam(symptoms.weakness  ~ 
              s(age, bs="cr", sp=1) + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial")

#sp=2
mod7 <- gam(symptoms.weakness  ~ 
              s(age, bs="cr", sp=2) + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial")

#use AIC test to compare models
AIC(mod4,mod5,mod6, mod7) #mod 7 wins



plot(simulateResiduals(mod7)) #yes!


summary(mod7) #age


gam.check(mod7)
concurvity(mod7) 


#make paper quality graphs of the dats and smooths 


draw(mod7, select = "s(age)", rug = TRUE, residuals = T) +
  labs(title = NULL,
       x = "Age (years)",
       y = "Effect on log-odds of symptoms.weakness ")+
  theme_bw()


"symptoms.body_swelling"  ########################

#Thin splines - default, no knots
mod1 <- gam(symptoms.body_swelling  ~ 
              s(age, bs="tp") + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial", method = "ML")
#Cubic regression - less complex than thin, with knots
mod2 <- gam(symptoms.body_swelling  ~ 
              s(age, bs="cr") + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial", method = "ML")
#P splines - smoothing splines, good for evenly spaced knots
mod3 <- gam(symptoms.body_swelling  ~ 
              s(age, bs="ps") + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial", method = "ML")

#use AIC test to compare models
AIC(mod1,mod2,mod3) #no differnce choose cr

#Now test the smoothing metric

#REML
mod4 <- gam(symptoms.body_swelling  ~ 
              s(age, bs="cr") + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial", method = "REML")
#sp=0.1
mod5 <- gam(symptoms.body_swelling  ~ 
              s(age, bs="cr", sp=0.1) + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial")
#sp=1
mod6 <- gam(symptoms.body_swelling  ~ 
              s(age, bs="cr", sp=1) + HW_pos + MalariaYN+schisto_positive,
            data = Symp, family = "binomial")

#sp=2
mod7 <- gam(symptoms.body_swelling  ~ 
              s(age, bs="cr", sp=2) + HW_pos + MalariaYN+schisto_positive,
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

