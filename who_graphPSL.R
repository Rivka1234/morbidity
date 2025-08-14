###############
#compare PSL to WHO ranges
##Clear environment
rm(list=ls())

library(dplyr)
library(ggplot2)
library(forcats)
#DATA####


#open main data doc ####

data <- read.csv("data_FINAL.csv")

#make sure age groups are in the right order
data<- data%>%mutate(age_group = fct_relevel(age_group, 
                                             "3-5", "6-10", "11-14", "15-20", "21-30", "31-40", ">40"))

####compare to healthy controls from Niamey protocol



#create height categories that fit with WHO healthy controls

data <- data %>%
  mutate(height_cat = case_when(height_cm >=80 & height_cm <101 ~ '80-100',
                                height_cm >=101 & height_cm <121 ~ '101-120',
                                height_cm >=121 & height_cm <141 ~ '121-140',
                                height_cm >=141 & height_cm <161 ~ '141-160',
                                height_cm >160 ~'>160'))

test2<-data%>%group_by(height_cat)%>%count()



nrow(data)

#convery psl to mm
data <- data %>%
  mutate(PSLmm = liverPSL_cm *10)

#create dataframe which only has schisto negative people
dfNEG <-data %>% select(height_cat, PSLmm, Portal_vein_diameter_mm, Positive, PosKK_CCA1)%>%
  filter(PosKK_CCA1 ==0)%>%
  mutate(Group = "END")%>%
  rename(height=height_cat)



nrow(dfNEG) #77

# Ensure height is a factor with the correct levels before summarizing
dfNEG$height <- factor(dfNEG$height, levels = c("80-100", "101-120", "121-140", "141-160", ">160"))

# Add confidence intervals and mean
dfNEG_summary <- dfNEG %>%
  group_by(height) %>% 
  summarize(
    mean = mean(PSLmm),
    sd_value = sd(PSLmm),
    n = n(),
    lower = mean - qt(0.975, df = n - 1) * sd_value / sqrt(n),
    upper = mean + qt(0.975, df = n - 1) * sd_value / sqrt(n)
  )

# Ensure height is a factor with the correct levels again, just in case
dfNEG_summary$height <- factor(dfNEG_summary$height, levels = c("80-100", "101-120", "121-140", "141-160", ">160"))

# View the summary
print(dfNEG_summary)

print(dfNEG_summary)

# Rename columns to match PSLNONwho
dfNEG_summary <- dfNEG_summary %>%
  rename(
    PSLmeanNON = mean,
    PSLconf_low = lower,
    PSLconf_high = upper
  )


# Data for negative only add in WHO points
data2 <- data.frame(
  height = c("80-100cm", "101-120cm", "121-140cm", "141-160cm", ">160cm"),
  mean = c(49,53,57,63,65),
  sd=c(4.5,6.5,7.9,10.1,11.6),
  sample=c(17,38,65,89,78),
  min2sd = c(58, 66, 73, 85, 89),
  max2sd = c(70, 78, 88, 103, 111),
  min4sd = c(70, 78, 88, 103, 111),
  max4sd = c(120, 120, 120, 120, 120))



#add mean and ci from raw data
data2$height <-dfNEG_summary$height
data2$raw_points <-dfNEG_summary$PSLmeanNON
data2$PSLconf_low <-dfNEG_summary$PSLconf_low
data2$PSLconf_high <-dfNEG_summary$PSLconf_high

print(data2)

#####simulate data from WHO mean and sd and plot this on graph as well
####
# Load necessary library
library(boot)


# Function to generate bootstrap samples and calculate mean
bootstrap_function <- function(data, indices) {
  sample_data <- data[indices]
  return(mean(sample_data))
}

# Initialize a list to store results
results <- list()

# Loop through each row in the data frame
for (i in 1:nrow(data2)) {
  mean <- data2$mean[i]
  sd <- data2$sd[i]
  sample_size <- data2$sample[i]
  
  # Generate the simulated data
  simulated_data <- rnorm(sample_size, mean, sd)
  
  # Perform bootstrap
  bootstrap_results <- boot(data = simulated_data, statistic = bootstrap_function, R = 1000)
  
  # Calculate 95% confidence intervals
  ci <- boot.ci(bootstrap_results, type = "perc")
  
  # Store results
  results[[i]] <- data.frame(
    height = data2$height[i],
    mean = mean(bootstrap_results$t),
    ci_lower = ci$percent[4],
    ci_upper = ci$percent[5]
  )
}

# Combine results into a single data frame
bootstrap_results_df <- do.call(rbind, results)

# Print the results
print(bootstrap_results_df)


# Combine the data frames for ggplot
data_combined <- merge(data2, bootstrap_results_df, by = "height")

# Create the plot
ggplot(data_combined, aes(x = height)) +
  # Shade area below min2sd
  geom_ribbon(aes(ymin = 40, ymax = min2sd, group = 1, fill="Normal Range"), alpha = 0.3) +
  # Shade area between min2sd and min4sd
  geom_ribbon(aes(ymin = min2sd, ymax = min4sd, group = 1, fill="Moderate abnormal"), alpha = 0.3) +
  # Shade area above min4sd
  geom_ribbon(aes(ymin = min4sd, ymax = max4sd, group = 1, fill="Abnormal"), alpha = 0.3) +
  # Schisto negative points
  geom_point(aes(y = raw_points, color="Schisto negative"), size = 2) +
  geom_line(aes(y = raw_points, group = 1, color="Schisto negative"), size = 0.7) +
  geom_errorbar(aes(ymin = PSLconf_low, ymax = PSLconf_high), width = 0.1, position = position_dodge(width = 0.3)) +
  # WHO simulated healthy controls points
  geom_point(data=bootstrap_results_df, aes(x=height, y = mean, color="WHO simulated healthy controls"), size = 2) +
  geom_line(data=bootstrap_results_df, aes(x=height, y = mean, group = 1, color="WHO simulated healthy controls"), size = 0.7) +
  geom_errorbar(data=bootstrap_results_df, aes(ymin = ci_lower, ymax = ci_upper), width = 0.1, position = position_dodge(width = 0.3)) +
  labs(x = "Height (cm)",
       y = "Parasternal line [PSL] (mm)",
       fill = "PSL ranges",
       color = "Legend") +
  scale_y_continuous(limits = c(40, 120)) +
  scale_x_discrete(expand = c(0.02,0))+
  scale_fill_manual(values = c("Normal Range" = "green", "Moderate abnormal" = "orange", "Abnormal" = "red")) +
  scale_color_manual(values = c("Schisto negative" = "blue", "WHO simulated healthy controls" = "purple")) +
  theme_minimal() +
  theme(text=element_text(size=12))

# Create the plot
ggplot(data2, aes(x = height)) +
  # Shade area below min2sd
  geom_ribbon(aes(ymin = 40, ymax = min2sd, group = 1, , fill="Normal Range"), alpha = 0.3) +
  # Shade area between min2sd and min4sd
  geom_ribbon(aes(ymin = min2sd, ymax = min4sd, group = 1, fill="Moderate abnormal"), alpha = 0.3) +
  # Shade area above min4sd
  geom_ribbon(aes(ymin = min4sd, ymax = max4sd, group = 1, fill="Abnormal"), alpha = 0.3) +
  # neg points
  geom_point(aes(y = raw_points, color="Schisto negative"), size = 2) +
  geom_line(aes(y = raw_points, group = 1, color="Schisto negative"), size = 0.7) +
  geom_errorbar(aes(ymin = PSLconf_low, ymax = PSLconf_high), width = 0.1, position = position_dodge(width = 0.3)) +
  # pos points
  # geom_point(aes(y = pos_points, color="Positive"), size = 2) +
  # geom_line(aes(y = pos_points, group = 1, color="Positive"), size = 0.7) +
  # geom_errorbar(aes(ymin = pos_low, ymax = pos_high, color="Positive"), width = 0.1, position = position_dodge(width = 0.3)) +
  
  labs(x = "Height (cm)",
       y = "Left parasternal line [PSL] (mm)",
       fill = "PSL ranges",
       color = "Legend")+  
  scale_y_continuous(limits = c(40, 120)) +
  scale_x_discrete(expand = c(0.02,0))+
  scale_fill_manual(values = colors[1:3]) +
  scale_color_manual(values = colors[4:5]) +
  theme_minimal()+
  theme(text=element_text(size=12))



######
#what does it look like when only have the positive people?
#
## Calculate means and confidence intervals
## 
## # Ensure height is a factor with the correct levels before summarizing
data$height_cat <- factor(data$height_cat, levels = c("80-100", "101-120", "121-140", "141-160", ">160"))

#create dataframe which only has schisto negative people
dfPOS <-data %>% select(height_cat, PSLmm, PosKK_CCA1)%>%
  filter(PosKK_CCA1 ==1)%>%
  mutate(Group = "END")%>%
  rename(height=height_cat)



# Add confidence intervals and mean
df_summaryPOS <- dfPOS %>%
  group_by(height) %>% 
  summarize(
    mean = mean(PSLmm),
    sd_value = sd(PSLmm),
    n = n(),
    lower = mean - qt(0.975, df = n - 1) * sd_value / sqrt(n),
    upper = mean + qt(0.975, df = n - 1) * sd_value / sqrt(n)
  )

# Ensure height is a factor with the correct levels again, just in case
df_summaryPOS$height <- factor(df_summaryPOS$height, levels = c("80-100", "101-120", "121-140", "141-160", ">160"))

# View the summary
print(df_summary)


# Rename columns to match PSLNONwho
df_summaryPOS <- df_summaryPOS %>%
  rename(
    PSLmeanNONPOS = mean,
    PSLconf_lowPOS = lower,
    PSLconf_highPOS = upper
  )


# Data for negative only add in WHO points
data2POS <- data.frame(
  min2sd = c(58, 66, 73, 85, 89),
  max2sd = c(70, 78, 88, 103, 111),
  min4sd = c(70, 78, 88, 103, 111),
  max4sd = c(120, 120, 120, 120, 120))

#add mean and ci from raw data
data2POS$height <-df_summaryPOS$height
data2POS$raw_pointsPOS <-df_summaryPOS$PSLmeanNONPOS
data2POS$PSLconf_lowPOS <-df_summaryPOS$PSLconf_lowPOS
data2POS$PSLconf_highPOS <-df_summaryPOS$PSLconf_highPOS

print(data2POS)

# # Convert height to a factor with specified levels
# Ensure height is treated as a factor with specified levels
data2POS$height <- factor(data2POS$height, levels = c("80-100", "101-120", "121-140", "141-160", ">160"))

colors <- c("Normal Range" = "green", "Moderate abnormal" = "yellow", "Abnormal" = "red", "Schisto positive" = "purple" )

# Create the plot
ggplot(data2POS, aes(x = height)) +
  # Shade area below min2sd
  geom_ribbon(aes(ymin = 40, ymax = min2sd, group = 1, , fill="Normal Range"), alpha = 0.3) +
  # Shade area between min2sd and min4sd
  geom_ribbon(aes(ymin = min2sd, ymax = min4sd, group = 1, fill="Moderate abnormal"), alpha = 0.3) +
  # Shade area above min4sd
  geom_ribbon(aes(ymin = min4sd, ymax = max4sd, group = 1, fill="Abnormal"), alpha = 0.3) +
  # neg points
  geom_point(aes(y = raw_pointsPOS, color="Schisto positive"), size = 2) +
  geom_line(aes(y = raw_pointsPOS, group = 1, color="Schisto positive"), size = 0.7) +
  geom_errorbar(aes(ymin = PSLconf_lowPOS, ymax = PSLconf_highPOS, color = "Schisto positive"), width = 0.1, position = position_dodge(width = 0.3)) +
  # pos points
  # geom_point(aes(y = pos_points, color="Positive"), size = 2) +
  # geom_line(aes(y = pos_points, group = 1, color="Positive"), size = 0.7) +
  # geom_errorbar(aes(ymin = pos_low, ymax = pos_high, color="Positive"), width = 0.1, position = position_dodge(width = 0.3)) +
  
  labs(x = "Height (cm)",
       y = "Left parasternal line [PSL] (mm)",
       fill = "PSL ranges",
       color = "Legend")+  
  scale_y_continuous(limits = c(40, 120)) +
  scale_x_discrete(expand = c(0.02,0))+
  scale_fill_manual(values = colors[1:3]) +
  scale_color_manual(values = colors[4:5]) +
  theme_minimal()+
  theme(text=element_text(size=12))





 