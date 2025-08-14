###############
#compare PVD to WHO ranges
##Clear environment
rm(list=ls())

library(dplyr)
library(ggplot2)
#DATA####



#open main data doc ####

data <- read.csv("data_FINAL.csv")

#make sure age groups are in the right order
data<- data%>%mutate(age_group = fct_relevel(age_group, 
                                             "3-5", "6-10", "11-14", "15-20", "21-30", "31-40", ">40"))

####compare to healthy controls from Niamey protocol



#create height categories

data <- data %>%
  mutate(height = case_when(height_cm >=80 & height_cm <101 ~ '80-100',
                                height_cm >=101 & height_cm <121 ~ '101-120',
                                height_cm >=121 & height_cm <141 ~ '121-140',
                                height_cm >=141 & height_cm <161 ~ '141-160',
                                height_cm >160 ~'>160'))


nrow(data)

#convery psl to mm
data <- data %>%
  mutate(PSLmm = liverPSL_cm *10)

#create dataframe which only has schisto negative people
dfNEG <-data %>% select(height, PSLmm, Portal_vein_diameter_mm, Positive, PosKK_CCA1)%>%
  filter(PosKK_CCA1 ==0)%>%
  mutate(Group = "END")



nrow(dfNEG) #93


#add confidence intervals and mean


# Ensure height is a factor with the correct levels before summarizing
dfNEG$height <- factor(dfNEG$height, levels = c("80-100", "101-120", "121-140", "141-160", ">160"))

# Add confidence intervals and mean
dfNEG_summary <- dfNEG %>%
  group_by(height) %>% 
  summarize(
    mean = mean(Portal_vein_diameter_mm),
    median=median(Portal_vein_diameter_mm),
    sd_value = sd(Portal_vein_diameter_mm),
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
    PVDmeanNON = mean,
    PVDconf_low = lower,
    PVDconf_high = upper
  )


# Data for negative only add in WHO points
# Data
data3 <- data.frame(
  mean = c(6.4, 6.8, 7.5, 8.65, 9.15),
  sd = c(0.2, 0.4, 0.75, 1.3, 1.55),
  sample=c(17,38,65,89,78),
  height = c("80-100cm", "101-120cm", "121-140cm", "141-160cm", ">160cm"),
  min2sd = c(6.8, 7.9, 9.0, 11.3, 12.3),
  max2sd = c(8.7, 10.2, 11.1, 14.9, 15.8),
  min4sd = c(8.7, 10.2, 11.1, 14.9, 15.8),
  max4sd = c(16,16,16,16,16))


#add mean and ci from raw data
data3$height <-dfNEG_summary$height
data3$raw_points <-dfNEG_summary$PVDmeanNON
data3$PVDconf_low <-dfNEG_summary$PVDconf_low
data3$PVDconf_high <-dfNEG_summary$PVDconf_high

print(data3)

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
for (i in 1:nrow(data3)) {
  mean <- data3$mean[i]
  sd <- data3$sd[i]
  sample_size <- data3$sample[i]
  
  # Generate the simulated data
  simulated_data <- rnorm(sample_size, mean, sd)
  
  # Perform bootstrap
  bootstrap_results <- boot(data = simulated_data, statistic = bootstrap_function, R = 1000)
  
  # Calculate 95% confidence intervals
  ci <- boot.ci(bootstrap_results, type = "perc")
  
  # Store results
  results[[i]] <- data.frame(
    height = data3$height[i],
    mean = mean(bootstrap_results$t),
    ci_lower = ci$percent[4],
    ci_upper = ci$percent[5]
  )
}

# Combine results into a single data frame
bootstrap_results_df <- do.call(rbind, results)

# Print the results
print(bootstrap_results_df)

# # Convert height to a factor with specified levels
# Ensure height is treated as a factor with specified levels
data3$height <- factor(data3$height, levels = c("80-100", "101-120", "121-140", "141-160", ">160"))

colors <- c("Normal Range" = "green", "Moderate abnormal" = "yellow", "Abnormal" = "red", "Schisto negative" = "black", "WHO simulated healthy controls" = "purple")

# Combine the data frames for ggplot
data_combined <- merge(data3, bootstrap_results_df, by = "height")

# Create the plot
ggplot(data_combined, aes(x = height)) +
  # Shade area below min2sd
  geom_ribbon(aes(ymin = 5.5, ymax = min2sd, group = 1, fill="Normal Range"), alpha = 0.3) +
  # Shade area between min2sd and min4sd
  geom_ribbon(aes(ymin = min2sd, ymax = min4sd, group = 1, fill="Moderate abnormal"), alpha = 0.3) +
  # Shade area above min4sd
  geom_ribbon(aes(ymin = min4sd, ymax = max4sd, group = 1, fill="Abnormal"), alpha = 0.3) +
  # Schisto negative points
  geom_point(aes(y = raw_points, color="Schisto negative"), size = 2) +
  geom_line(aes(y = raw_points, group = 1, color="Schisto negative"), size = 0.7) +
  geom_errorbar(aes(ymin = PVDconf_low, ymax = PVDconf_high), width = 0.1, position = position_dodge(width = 0.3)) +
  # WHO simulated healthy controls points
  geom_point(data=bootstrap_results_df, aes(x=height, y = mean, color="WHO simulated healthy controls"), size = 2) +
  geom_line(data=bootstrap_results_df, aes(x=height, y = mean, group = 1, color="WHO simulated healthy controls"), size = 0.7) +
  geom_errorbar(data=bootstrap_results_df, aes(ymin = ci_lower, ymax = ci_upper), width = 0.1, position = position_dodge(width = 0.3)) +
  labs(x = "Height (cm)",
       y = "Portal vein diameter [PVD] (mm)",
       fill = "PVD ranges",
       color = "Legend") +
  scale_y_continuous(limits = c(5.5,16)) +
  scale_x_discrete(expand = c(0.02,0)) +
  scale_fill_manual(values = c("Normal Range" = "green", "Moderate abnormal" = "orange", "Abnormal" = "red")) +
  scale_color_manual(values = c("Schisto negative" = "blue", "WHO simulated healthy controls" = "purple")) +
  theme_minimal() +
  theme(text=element_text(size=12))





