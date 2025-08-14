##Clear environment
rm(list=ls())


#open main data doc ####

data <- read.csv("data_FINAL.csv")

############################################################################3

library(dplyr)
library(forcats)
library(ggplot2)
library(tidyr)
library(ggbreak)
#make sure age groups are in the right order
data<- data%>%mutate(age_group = fct_relevel(age_group, 
                                             "3-5", "6-10", "11-14", "15-20", "21-30", "31-40", ">40"))


#DEMO TABLE####

#how many people in each age group

data %>%
  group_by(age_group)%>%
  tally()

#overall community prevalence
data %>% filter(schisto_positive == "Y")%>%
  count() #167
167/287 #= 58%

#number of females
data %>% 
  filter(sex == "F")%>%
  count() #183

#number of males
data %>% 
  filter(sex == "M")%>%
  count() #104


#prev by sex
data %>% filter(schisto_positive == "Y")%>%
  filter(sex == "F")%>%
  count() #105
105/183 #0.573

data %>% filter(schisto_positive == "Y")%>%
  filter(sex == "M")%>%
  count() #62
62/104 #0.592

#prev by age
data %>% group_by(age_group, schisto_positive)%>%
  tally()

data %>%group_by(Category)%>%
  tally()

#malaria
data %>%group_by(MalariaYN)%>%
  tally()

#hookworm
data %>%group_by(HW_cat)%>%
  tally()


#anaemia summary stats
 data %>% group_by(Anaemia)%>%
   tally()


 
 #Geometric mean
 
 female <- data %>%
   filter(sex=="F")%>%
   filter(mean_intensity_noNA >0)%>%
   summarise(geo_mean = exp(mean(log(mean_intensity_noNA)))) #61
 
male <- data %>%
   filter(sex=="M")%>%
   filter(mean_intensity_noNA >0)%>%
   summarise(geo_mean = exp(mean(log(mean_intensity_noNA)))) #132

ad_female <- data %>%
  filter(sex=="F")%>%
  filter(age >15)%>%
  filter(mean_intensity_noNA >0)%>%
  summarise(geo_mean = exp(mean(log(mean_intensity_noNA)))) #38

ad_male <- data %>%
  filter(sex=="M")%>%
  filter(age >15)%>%
  filter(mean_intensity_noNA >0)%>%
  summarise(geo_mean = exp(mean(log(mean_intensity_noNA)))) #117
   
PSAC <- data %>%
  filter(age <6)%>%
  filter(mean_intensity_noNA >0)%>%
  summarise(geo_mean = exp(mean(log(mean_intensity_noNA)))) #50

yes <- data %>%
  filter(age >5 & age <11)%>%
  filter(mean_intensity_noNA >0)%>%
  summarise(geo_mean = exp(mean(log(mean_intensity_noNA)))) #132

yes1 <- data %>%
  filter(age >10 & age <15)%>%
  filter(mean_intensity_noNA >0)%>%
  summarise(geo_mean = exp(mean(log(mean_intensity_noNA)))) #103

yes2 <- data %>%
  filter(age >14 & age <21)%>%
  filter(mean_intensity_noNA >0)%>%
  summarise(geo_mean = exp(mean(log(mean_intensity_noNA)))) #106

yes3 <- data %>%
  filter(age >20 & age <31)%>%
  filter(mean_intensity_noNA >0)%>%
  summarise(geo_mean = exp(mean(log(mean_intensity_noNA)))) #27

yes4 <- data %>%
  filter(age >30 & age <41)%>%
  filter(mean_intensity_noNA >0)%>%
  summarise(geo_mean = exp(mean(log(mean_intensity_noNA)))) #27

yes5 <- data %>%
  filter(age >40)%>%
  filter(mean_intensity_noNA >0)%>%
  summarise(geo_mean = exp(mean(log(mean_intensity_noNA)))) #27

#males and females per age group

data %>%
  group_by(ageCLASS, sex) %>%
  summarise(n = n(), .groups = "drop")


#intensity per age and sex
data %>%
  group_by(ageCLASS, sex) %>%
  summarise(
    n = n(),
    arith_mean = mean(mean_intensity_noNA, na.rm = TRUE),
    geom_mean = exp(mean(log(mean_intensity_noNA[mean_intensity_noNA > 0]), na.rm = TRUE)),
    .groups = "drop"
  )

#intensity per age group and sex

data %>%
  group_by(age_group, sex) %>%
  summarise(
    n = n(),
    arith_mean = mean(mean_intensity_noNA, na.rm = TRUE),
    geom_mean = exp(mean(log(mean_intensity_noNA[mean_intensity_noNA > 0]), na.rm = TRUE)),
    .groups = "drop"
  )


#DEMO GRAPHS####
#graph morb vs age##

#make sure age groups are in the right order
library(forcats)
data <- data %>%
  mutate(age_group = fct_relevel(age_group, 
                                 "3-5", "6-10", "11-14", "15-20", "21-30", "31-40", ">40"))

#Fig 1a in paper
#makine a new table where it says number of schisto_positive people per age group
age_prevalence_data_perc_overall <- data %>% dplyr::group_by(age_group) %>% dplyr::summarise(schisto_positive = sum(schisto_positive == 'Y')/sum((schisto_positive == 'Y' )+ (schisto_positive == 'N'))*100)
age_prevalence_data_perc_overall #in demo graph for prev by age

#stratify by sex
age_prevalence_data_perc_MF <- data %>% dplyr::group_by(age_group,sex) %>% dplyr::summarise(schisto_positive = sum(schisto_positive == 'Y')/sum((schisto_positive == 'Y' )+ (schisto_positive == 'N'))*100)
age_prevalence_data_perc_MF #for graph

#male only prevalence tables for each age group
age_prevalence_data_perc_M <- filter(age_prevalence_data_perc_MF, sex != "F")
age_prevalence_data_perc_M #for graph



#female only prevalence tables for each age group 
age_prevalence_data_perc_F <- filter(age_prevalence_data_perc_MF, sex != "M")
age_prevalence_data_perc_F


#graph age and sex prevalence ###

#male and female line graph

#set colours 
colors <- c("M" = "#D95F02", "F" = "#1B9E77")

library(ggplot2)

#Fig 1a in paper stratified by sex####
ggplot(age_prevalence_data_perc_F, aes(x = age_group, y = schisto_positive, group = 1)) +
  geom_line(aes(color = 'F')) + 
  geom_point(aes(color = 'F'))+
  geom_line(data = age_prevalence_data_perc_M, aes(color = 'M')) +
  geom_point(data = age_prevalence_data_perc_M, aes(color = 'M'))+
  geom_line(data = age_prevalence_data_perc_overall)+
  geom_point(data = age_prevalence_data_perc_overall)+
  ylim(0,100)+
  geom_hline(yintercept=58, linetype='dotted', col = 'black')+
  scale_colour_brewer(palette = "Dark2")+
  theme_bw()+
  labs(y='Prevalence %', x='Age',
       color = "sex") +
  scale_color_manual(values = colors)+ 
  theme(axis.text = element_text(size = 12)) 


#Fig 1a no sex####
ggplot(age_prevalence_data_perc_overall, aes(x = age_group, y = schisto_positive, group = 1)) +
  geom_line() + 
  geom_point()+
  ylim(0,100)+
  geom_line(df, aes(x=)) + 
  geom_point()+
  scale_colour_brewer(palette = "Dark2")+
  theme_bw()+
  labs(y='Prevalence %', x='Age',
       color = "sex") +
  scale_color_manual(values = colors)+ 
  theme(text=element_text(size=12))


library(dplyr)
library(ggplot2)




# Here, it's assumed that there is a 'schisto_binary' column with 1 for positive and 0 for negative
black_line_data <- data %>%
  dplyr::group_by(age_group) %>%
  dplyr::summarise(CCA = sum(PosKK_CCA1 == '1') / sum((PosKK_CCA1 == '1') + (PosKK_CCA1 == '0')) * 100) # Adjust column name as necessary

# Combine both datasets for plotting
combined_data <- age_prevalence_data_perc_overall %>%
  dplyr::mutate(type = 'Blue Line') %>%
  dplyr::bind_rows(black_line_data %>% dplyr::mutate(type = 'Black Line'))

combined_data <- combined_data %>%
  mutate(age_group = fct_relevel(age_group, 
                                 "3-5", "6-10", "11-14", "15-20", "21-30", "31-40", ">40"))

library(dplyr)
library(ggplot2)

# Filter out rows with NA values separately for each line
blue_line_data <- combined_data %>%
  filter(type == "Blue Line" & !is.na(schisto_positive))

black_line_data <- combined_data %>%
  filter(type == "Black Line" & !is.na(CCA))

## Plot both lines with italicized S. mansoni and axis labels handled separately
ggplot() +
  geom_line(data = blue_line_data, aes(x = age_group, y = schisto_positive, group = 1), color = "blue", size = 1) +
  geom_point(data = blue_line_data, aes(x = age_group, y = schisto_positive), color = "blue", size = 2) +
  geom_line(data = black_line_data, aes(x = age_group, y = CCA, group = 1), color = "black", size = 1) +
  geom_point(data = black_line_data, aes(x = age_group, y = CCA), color = "black", size = 2) +
  ylim(0, 100) +
  theme_bw() +
  labs(y = "S. mansoni prevalence (%)", x = 'Age (Years)') +
  theme(
    axis.title.x = element_text(size = 24, face = "bold"),
    axis.title.y = element_text(size = 24, face = "bold"),
    text = element_text(size = 24, face="bold") # Ensuring all text size is consistent
  )




MP <-data %>% select(age_group, symptoms.muscle_pain)

#Remove rows with NAs
MP <- MP[which(complete.cases(MP)==T),]

MP <-MP %>%group_by(symptoms.muscle_pain, age_group)%>%tally()

sum(MP5$n)

MP5 <- MP %>%filter(age_group=="15-20" | age_group =="21-30" |
                      age_group=="31-40" | age_group ==">40")

#Fig 1b in paper####

#intensity by age
age_intensity <- data%>% dplyr::group_by(age_group) %>% 
  summarise(Mean = mean(mean_intensity_noNA), SE = sd(mean_intensity_noNA, na.rm = TRUE)/sqrt(length((mean_intensity_noNA))))




age_intensity_sex <- data%>% dplyr::group_by(age_group, sex) %>% 
  summarise(Mean = mean(mean_intensity_noNA), SE = sd(mean_intensity_noNA, na.rm = TRUE)/sqrt(length((mean_intensity_noNA))))

#Fig 1b in paper
ggplot(data = age_intensity_sex) +
  geom_bar(aes(x = age_group, y = Mean, fill = sex), stat="identity", width=.5, position = "dodge")  + 
  geom_errorbar(aes(x = age_group, y = Mean, fill = sex, ymin= Mean - SE, ymax= Mean + SE), width = 0.5, position = "dodge") + 
  labs(y='Mean Egg Intensity (EPG)', x='Age (Years)')+
  geom_hline(yintercept=400, linetype='dotted', col = 'black')+
  annotate("text", x = "31-40", y = 400, label = "Heavy infection", vjust = -0.5)+
  #geom_line(data=age_intensity, aes(x=age_group, y = Mean, group = 1))+
  scale_fill_brewer(palette = "Dark2")+
  theme_bw()+ 
  theme(axis.text = element_text(size = 10))   

#Fig 1b in paper no sex
ggplot(data = age_intensity) +
  geom_col(aes(x = age_group, y = Mean), width=.7, fill="dark blue")  + 
  geom_errorbar(aes(x = age_group, y = Mean, ymin= Mean - SE, ymax= Mean + SE), width = 0.2) + 
  labs(y='Mean Egg Intensity (EPG)', x='Age (Years)')+
  #geom_hline(yintercept=400, linetype='dotted', col = 'black')+
  #annotate("text", x = "31-40", y = 400, label = "Heavy infection", vjust = -0.5)+
  #geom_line(data=age_intensity, aes(x=age_group, y = Mean, group = 1))+
  theme_bw()+ 
  theme(text=element_text(size=12))+
  ylim(0,500)

#Make with datapoints
#
##Fig 1b with sex
## Fig 1b with sex
ggplot(data = data) +
  geom_bar(aes(x = age_group, y = mean_intensity_noNA, fill = sex), position = "dodge", stat = "summary", fun = "mean", width = 0.7) + 
  geom_point(data = data, aes(x = age_group, y = mean_intensity_noNA, color = sex), position = position_dodge(width = 0.7)) +
  #geom_errorbar(data = age_intensity, aes(x = age_group, y = Mean, ymin = Mean - SE, ymax = Mean + SE, group = sex, color = sex), position = position_dodge(width = 0.7), width = 0.2) + 
  labs(y = 'Mean Egg Intensity (EPG)', x = 'Age (Years)') +
  #geom_hline(yintercept = 400, linetype = 'dotted', col = 'black') +
  #annotate("text", x = "31-40", y = 400, label = "Heavy infection", vjust = -0.5) +
  #geom_line(data = age_intensity, aes(x = age_group, y = Mean, group = sex, color = sex)) +
  theme_bw() + 
  scale_fill_brewer(palette = "Dark2") +
  scale_color_manual(values = c("F" = "darkgreen", "M" = "darkred")) +
  theme(text = element_text(size = 12)) +
  ylim(0, 2500) +
  scale_y_cut(breaks = 400, which = c(1, 2), scales = c(1, 2))



##Fig 1b in paper no sex
ggplot(data = data) +
  geom_bar(aes(x = age_group, y = mean_intensity_noNA), position = "dodge", stat = "summary", fun.y = "mean", width=.7, fill="dark blue")  + 
  geom_point(data=data, aes(x=age_group, y=mean_intensity_noNA))+
  geom_errorbar(data = age_intensity, aes(x = age_group, y = Mean, ymin= Mean - SE, ymax= Mean + SE), width = 0.2) + 
  labs(y='Mean egg intensity (EPG)', x='Age (Years)')+
  #geom_hline(yintercept=400, linetype='dotted', col = 'black')+
  #annotate("text", x = "31-40", y = 400, label = "Heavy infection", vjust = -0.5)+
  #geom_line(data=age_intensity, aes(x=age_group, y = Mean, group = 1))+
  theme_bw()+ 
  theme(text=element_text(size=24, family = "Helvetica", face="bold"), axis.text=element_text(size=24, family="Helvetica", face="bold"), legend.text=element_text(size=24))+
  ylim(0,2500)+
  scale_y_cut(breaks=400, which=c(1, 2), scales=c(1,2)) 

#for BSP
ggplot(data = data) +
  geom_bar(aes(x = age_group, y = mean_intensity_noNA), position = "dodge", stat = "summary", fun.y = "mean", width=.7, fill="dark blue")  + 
  geom_errorbar(data = age_intensity, aes(x = age_group, y = Mean, ymin= Mean - SE, ymax= Mean + SE), width = 0.2) + 
  labs(y='Mean Egg Intensity (EPG)', x='Age (Years)')+
  #geom_hline(yintercept=400, linetype='dotted', col = 'black')+
  #annotate("text", x = "31-40", y = 400, label = "Heavy infection", vjust = -0.5)+
  #geom_line(data=age_intensity, aes(x=age_group, y = Mean, group = 1))+
  theme_bw()+ 
  theme(text=element_text(size=20))+
  coord_cartesian(ylim=c(0,400))



#COINFECTION summary stats ####

data %>% filter(schisto_positive=="Y")%>%
  filter(HW_pos=="1")%>%
  filter(MalariaYN=="1")

test4 <- chisq.test(data$MalariaYN, data$Anaemia) #test says it is good
test4 #they are highly correlated 0.005

test4 <- chisq.test(data$MalariaYN, data$PSL_scoreBIN) #test says it is good
test4 #they are highly correlated 0.07

test <- glm(MalariaYN~PSL_scoreBIN, data=data, family = "binomial")
summary(test)
#CATEGORIES####
#INTESNITY CATEGORY NOT RELATED TO PVD OR PSL

#OODS OF HAVING PVD IN HEAVILY INFECTED??


glmPVDhigh <- glm(PH_scoreBIN ~ sex+age_group+Category,
                               data = data, 
                               family ="binomial")

summary(glmPVDhigh) #no


#relevel the data to compare to Negative ##

data$Category <- as.factor(data$Category)


data$Category <- relevel(data$Category, ref = "Negative") 


# Fit the logistic regression model
model <- glm(PH_scoreBIN ~ sex + age_group + Category, data = data, family = "binomial")

# Get the coefficient estimates
coefficients <- coef(model)

# Calculate odds ratios and their confidence intervals

odds_ratio_high_vs_low <- exp(coefficients["CategoryHigh"] - coefficients["CategoryLow"])


# Calculate confidence intervals
conf_intervals <- confint(model)
ci_high_vs_low<- exp(conf_intervals["CategoryHigh",] - conf_intervals["CategoryLow",])

# Get standard errors
std_errors <- summary(model)$coef[, "Std. Error"]

# Get p-values for specific comparisons using the Wald test
p_value_high_vs_low <- 2 * (1 - pnorm(abs(coefficients["CategoryHigh"] - coefficients["CategoryLow"]) / sqrt(std_errors["CategoryHigh"]^2 + std_errors["CategoryLow"]^2)))

# Print results

cat("Odds Ratio (High vs. Low):", odds_ratio_high_vs_low, "\n")
cat("95% CI:", ci_high_vs_low, "\n")
cat("p-value:", p_values_high_vs_low, "\n")
cat("\n")


#graph with both PVD and PSL results in one
#
dataBOTH <- data %>% select(ID,age_group,PH_scoreBIN, PSL_scoreBIN )

summary_data <- dataBOTH %>%
  group_by(age_group) %>%
  summarize(PVD_positive = sum(PH_scoreBIN) / n() * 100,
            PSL_positive = sum(PSL_scoreBIN) / n() * 100,
            PVD_se = sqrt((sum(PH_scoreBIN) / n()) * (1 - sum(PH_scoreBIN) / n()) / n()) * 100,
            PSL_se = sqrt((sum(PSL_scoreBIN) / n()) * (1 - sum(PSL_scoreBIN) / n()) / n()) * 100)


# Reshape the dataset into long format
long_data <- summary_data %>%
  pivot_longer(cols = starts_with("PVD_") | starts_with("PSL_"),
               names_to = c("Morbidity", ".value"),
               names_pattern = "(.*)_(.*)")

ggplot(data=long_data, aes(x=age_group, y=positive, fill=Morbidity))+
  geom_bar(stat="identity",position=position_dodge(0.5,preserve = 'single'), width=.5)+
  geom_errorbar(aes(ymin= positive-se, ymax= positive+se), width = 0.3, position = position_dodge2(0.)) + 
  labs(y='Percent liver morbidity', x='Age (Years)')+
  scale_fill_brewer(palette = "Dark2")+
  theme_bw()+ 
  theme(axis.text = element_text(size = 12)) +
  coord_cartesian(ylim=c(0,100))




age35 <- data %>% filter(age_group=="3-5")
age610 <- data %>% filter(age_group=="6-10")
age1114 <- data %>% filter(age_group=="11-14")
age1520 <- data %>% filter(age_group=="15-20")
age2130 <- data %>% filter(age_group=="21-30")
age3140 <- data %>% filter(age_group=="31-40")
age40 <- data %>% filter(age_group==">40")

glm <- glm(mean_intensity_noNA ~ sex, data=age40)
summary(glm)  

summary(data$mean_intensity_noNA)

data$age_group <- relevel(data$age_group, ref = "6-10") 
glm <- glm(MalariaYN ~ age_group, data=data, family="binomial")
summary(glm)


#malaria graoh
dataMal <- data %>%
group_by(age_group, MalariaYN)%>%
tally()%>%
filter(!is.na(age_group))%>%
filter(!is.na(MalariaYN))

# Calculate the total counts for each age group
total_counts <- dataMal %>%
  group_by(age_group) %>%
  summarise(total = sum(n))

# Merge the total counts with the original data
dataMal <- dataMal %>%
  left_join(total_counts, by = "age_group") %>%
  filter(MalariaYN == 1) %>% # Filter to keep only malaria-positive cases
  mutate(proportion = n / total)


dataMal <- dataMal%>%mutate(age_group = fct_relevel(age_group, 
                                                             "3-5", "6-10", "11-14", "15-20", "21-30", "31-40", ">40"))

# Create the bar chart with proportions
ggplot(dataMal, aes(x = age_group, y = proportion)) +
  geom_bar(stat = "identity", fill = "darkblue", width=0.2) +
  labs(y = "Proportion of Participants with Malaria", x = "Age Group") +
  theme_bw() +
  theme(text = element_text(size = 12))

dataMal <- data %>%
  group_by(age_group, MalariaYN) %>%
  tally() %>%
  filter(!is.na(age_group)) %>%
  filter(!is.na(MalariaYN))

# Calculate the total counts for each age group
total_counts <- dataMal %>%
  group_by(age_group) %>%
  summarise(total = sum(n))

# Merge the total counts with the original data
dataMal <- dataMal %>%
  left_join(total_counts, by = "age_group") %>%
  filter(MalariaYN == 1) %>% # Filter to keep only malaria-positive cases
  mutate(percentage = (n / total) * 100)  # Convert proportion to percentage

# Relevel age_group factor for ordering
dataMal <- dataMal %>%
  mutate(age_group = fct_relevel(age_group, 
                                 "3-5", "6-10", "11-14", "15-20", "21-30", "31-40", ">40"))

# Create the bar chart with percentages
mal <-ggplot(dataMal, aes(x = age_group, y = percentage)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  labs(y = "Prevalence of malaria (%)", x = "Age Group") +
  theme_bw() +
  theme(
    text=element_text(size=24, family = "Helvetica", face="bold"), 
    axis.text=element_text(size=24, family="Helvetica", face="bold"))+
  ylim(0,100)
mal

# Save the plot as PNG
ggsave("Submission_Plos_NTD/Reviewers_comments/Figs/malaria.png", plot = mal, width = 10, height = 8, dpi = 300)




#anameia graph
# Group by age group and Anaemia category (exclude "Normal" category)
dataAnaemia <- data %>%
  group_by(age_group, Anaemia) %>%
  tally() %>%
  filter(!is.na(age_group)) %>%
  filter(Anaemia != "Normal")  # Exclude the "Normal" category

# Calculate the total counts for each age group (from the full dataset, not just those with anaemia)
total_counts_anaemia_all <- data %>%
  group_by(age_group) %>%
  summarise(total = n())

# Merge the total counts with the original data
dataAnaemia <- dataAnaemia %>%
  left_join(total_counts_anaemia_all, by = "age_group") %>%
  mutate(percentage = (n / total) * 100)  # Calculate percentage of total population

# Relevel age_group factor for ordering
dataAnaemia <- dataAnaemia %>%
  mutate(age_group = fct_relevel(age_group, 
                                 "3-5", "6-10", "11-14", "15-20", "21-30", "31-40", ">40"))

# Create the stacked bar chart with percentages for Anaemia, grouped by Anaemia category
AN <-ggplot(dataAnaemia, aes(x = age_group, y = percentage, fill = Anaemia)) +
  geom_bar(stat = "identity") +
  labs(y = "Prevalence of anaemia (%)", x = "Age Group") +
  scale_fill_manual(values = c("Mild" = "darkslategray3", "Moderate" = "darkslategray4", "Severe" = "darkslategray")) + 
  theme_bw() +
  theme(
    text=element_text(size=24, family = "Helvetica", face="bold"), 
    axis.text=element_text(size=24, family="Helvetica", face="bold"))+
  ylim(0,100)
AN

# Save the plot as PNG
ggsave("Submission_Plos_NTD/Reviewers_comments/Figs/anaemia.png", plot = AN, width = 10, height = 8, dpi = 300)



  