#summary stats

#####################


#open main data doc ####

data <- read.csv("data_FINAL.csv")

#how many have positive PVD?
data%>%filter(PH_scoreBIN ==1)%>%
  nrow()

#What is the mean and range of epg?
pvd <- data%>%filter(PH_scoreBIN ==1)%>%
  filter(mean_intensity_noNA >0)

summary(pvd$mean_intensity_noNA)

#how many ≥400 epg?

data%>%filter(PH_scoreBIN ==1)%>%
  filter(mean_intensity_noNA>=399)%>%
nrow()

#how many poivtive by KK
data%>%filter(PH_scoreBIN ==1)%>%
  filter(Positive=="Y")%>%
  nrow()


######################

#how many have positive PSL?
data%>%filter(PSL_scoreBIN ==1)%>%
  nrow()

#What is the mean and range of epg?
psl<- data%>%filter(PSL_scoreBIN ==1)%>%
  filter(mean_intensity_noNA >0)

summary(psl$mean_intensity_noNA)

#how many ≥400 epg?

data%>%filter(PSL_scoreBIN ==1)%>%
  filter(mean_intensity_noNA>=399)%>%
  nrow()

#how many poivtive by KK
data%>%filter(PSL_scoreBIN ==1)%>%
  filter(Positive=="Y")%>%
  nrow()

#####################################

#how many have positive IP A
data%>%filter(A.normal_liver == "Y" & cirrhosis.like == "N" & fatty.liver == "N" & other == "N")%>%
  nrow()

#how many poivtive by KK
data%>%filter(A.normal_liver == "Y" & cirrhosis.like == "N" & fatty.liver == "N" & other == "N")%>%
  filter(Positive=="Y")%>%
  nrow()

#What is the mean and range of epg?
ipA <- data%>%filter(A.normal_liver == "Y" & cirrhosis.like == "N" & fatty.liver == "N" & other == "N")%>%
  filter(mean_intensity_noNA >0)

summary(ipA$mean_intensity_noNA)

#how many ≥400 epg?

data%>%filter(A.normal_liver == "Y" & cirrhosis.like == "N" & fatty.liver == "N" & other == "N")%>%
  filter(mean_intensity_noNA>=399)%>%
  nrow()


#######################################

#how many have positive IP B
data%>%filter(B0.feather_streaks == "Y" | B1.flying_saucers =="Y" | B2.spider_thickening =="Y"
              & cirrhosis.like == "N" & fatty.liver == "N" & other == "N")%>%
  nrow()

#how many poivtive by KK
data%>%filter(B0.feather_streaks == "Y" | B1.flying_saucers =="Y" | B2.spider_thickening =="Y"
              & cirrhosis.like == "N" & fatty.liver == "N" & other == "N")%>%
  filter(Positive=="Y")%>%
  nrow()

#What is the mean and range of epg?
ipB <- data%>%filter(B0.feather_streaks == "Y" | B1.flying_saucers =="Y" | B2.spider_thickening =="Y"
                    & cirrhosis.like == "N" & fatty.liver == "N" & other == "N")%>%
  filter(mean_intensity_noNA >0)

summary(ipB$mean_intensity_noNA)

#how many ≥400 epg?

data%>%filter(B0.feather_streaks == "Y" | B1.flying_saucers =="Y" | B2.spider_thickening =="Y"
              & cirrhosis.like == "N" & fatty.liver == "N" & other == "N")%>%
  filter(mean_intensity_noNA>=399)%>%
  nrow()

######################################

#how many have positive IP C
data%>%filter(C1.prom_peripheral_rings == "Y" | C2.prom_pipe_stems =="Y" 
              & cirrhosis.like == "N" & fatty.liver == "N" & other == "N")%>%
  nrow()

#how many poivtive by KK
data%>%filter(C1.prom_peripheral_rings == "Y" | C2.prom_pipe_stems =="Y" 
              & cirrhosis.like == "N" & fatty.liver == "N" & other == "N")%>%
  filter(Positive=="Y")%>%
  nrow()

#What is the mean and range of epg?
ipC <- data%>%filter(C1.prom_peripheral_rings == "Y" | C2.prom_pipe_stems =="Y" 
                     & cirrhosis.like == "N" & fatty.liver == "N" & other == "N")%>%
  filter(mean_intensity_noNA >0)

summary(ipC$mean_intensity_noNA)

#how many ≥400 epg?

data%>%filter(C1.prom_peripheral_rings == "Y" | C2.prom_pipe_stems =="Y" 
              & cirrhosis.like == "N" & fatty.liver == "N" & other == "N")%>%
  filter(mean_intensity_noNA>=399)%>%
  nrow()

######################################

#how many have positive IP D
data%>%filter(D.ruff == "Y" 
              & cirrhosis.like == "N" & fatty.liver == "N" & other == "N")%>%
  nrow()

#how many poivtive by KK
data%>%filter(D.ruff == "Y" 
              & cirrhosis.like == "N" & fatty.liver == "N" & other == "N")%>%
  filter(Positive=="Y")%>%
  nrow()

#What is the mean and range of epg?
ipD <- data%>%filter(D.ruff == "Y" 
                     & cirrhosis.like == "N" & fatty.liver == "N" & other == "N")%>%
  filter(mean_intensity_noNA >0)

summary(ipD$mean_intensity_noNA)

#how many ≥400 epg?

data%>%filter(C1.prom_peripheral_rings == "Y" | C2.prom_pipe_stems =="Y" 
              & cirrhosis.like == "N" & fatty.liver == "N" & other == "N")%>%
  filter(mean_intensity_noNA>=399)%>%
  nrow()

######################################

#how many have positive IP E
data%>%filter(E.patches == "Y" 
              & cirrhosis.like == "N" & fatty.liver == "N" & other == "N")%>%
  nrow()

#how many poivtive by KK
data%>%filter(E.patches == "Y"
              & cirrhosis.like == "N" & fatty.liver == "N" & other == "N")%>%
  filter(Positive=="Y")%>%
  nrow()

#What is the mean and range of epg?
ipE <- data%>%filter(E.patches == "Y"
                     & cirrhosis.like == "N" & fatty.liver == "N" & other == "N")%>%
  filter(mean_intensity_noNA >0)

summary(ipE$mean_intensity_noNA)

#how many ≥400 epg?

data%>%filter(C1.prom_peripheral_rings == "Y" | C2.prom_pipe_stems =="Y" 
              & cirrhosis.like == "N" & fatty.liver == "N" & other == "N")%>%
  filter(mean_intensity_noNA>=399)%>%
  nrow()


IPmod <- glm(IP_scoreBF ~ Category, data=data, family="binomial")
summary(IPmod)
#plot(test)

#test main model for dispersion

library(DHARMa)
testDispersion(IPmod)
simulationOutput <- simulateResiduals(fittedModel = IPmod, plot = T)

#and goodness of fit
library(glmtoolbox)
hltest(IPmod)


library(sjPlot)
library(sjmisc)
library(sjlabelled)

tab <-tab_model(IPmod)
tab

PSLmod <- glm(PSL_scoreBIN ~ Category, data=data, family="binomial")
summary(PSLmod)
#plot(test)

#test main model for dispersion

library(DHARMa)
testDispersion(PSLmod)
simulationOutput <- simulateResiduals(fittedModel = PSLmod, plot = T)

#and goodness of fit
library(glmtoolbox)
hltest(PSLmod)


library(sjPlot)
library(sjmisc)
library(sjlabelled)

tab <-tab_model(IPmod)
tab

#1p and age

mod <- glm(IP_scoreBF ~ age_group, data = data, family = "binomial")
summary(mod)
