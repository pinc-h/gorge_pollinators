# Alex Pinch, last edited Sept 8 2024

# This one is a mess. Totally unorganized pieces of code that does not run and has not been looked over.
# A big pastebin really

library(tidyverse)
library(patchwork)

setwd("/Users/alexpinch/Documents/obs_vault/Gorge_work_2024/gorge_pollinators_2024/data")

# Loading data stuff, copied from the main analysis
pan_trap_data_2023 <- read.csv("20240905_pantraps_2023.csv")
pan_trap_data_2023$Collection.Date <- format(as.Date(pan_trap_data_2023$Collection.Date, format = "%B %d, %Y"), "%d-%b-%y")
visitation_sampling_data_2023 <- read.csv("20240905_visitation_2023.csv")
visitation_sampling_data_2023$Collection.Date <- format(as.Date(visitation_sampling_data_2023$Collection.Date, format = "%B %d, %Y"), "%d-%b-%y")
pan_trap_data_2024 <- read.csv("20240905_pantraps_2024.csv")
# pan_trap_data_2024$Collection.Date <- format(as.Date(pan_trap_data_2024$Collection.Date, format = "%B %d, %Y"), "%d-%b-%y")
visitation_sampling_data_2024 <- read.csv("20240905_visitation_2024.csv")
visitation_sampling_data_2024$Collection.Date <- format(as.Date(visitation_sampling_data_2024$Collection.Date, format = "%B %d, %Y"), "%d-%b-%y")
# this might be a one-off, for some reason pan trap data for 2023 the date is formatted differently. changing to match the others

pan_trap_data <- rbind(pan_trap_data_2023, pan_trap_data_2024)
visitation_sampling_data <- rbind(visitation_sampling_data_2023, visitation_sampling_data_2024)

common_columns <- intersect(colnames(pan_trap_data), colnames(visitation_sampling_data))
data <- rbind(
  pan_trap_data %>% select(all_of(common_columns)),
  visitation_sampling_data %>% select(all_of(common_columns))
)

data$Collection.Date <- as.Date(data$Collection.Date, format = "%d-%b-%y")

# --------
# Attempt 1 at rarefaction
#   Ok thinking out loud for me to look at in the morning:
#   From Wikipedia: “Rarefaction curves are created by randomly re-sampling the pool of N samples multiple times and then plotting the average number of species found in each sample.”
#   How many times is multiple times? Until you get the asymptote?
#   So pull random samples & get average number of genera, do this enough times to get an asymptote?
#   Did I summarize correctly
#   Connor — 07/26/2024 12:40 AM
#   Do it a number of time equal to the lowest sample size

# empty dataframe to store the results
average_genera <- data.frame(Average = numeric(0))

for (i in 1:nrow(data)) {
  
  random_rows <- data[sample(1:nrow(data), 10, replace = TRUE), ]
  unique_strings <- length(unique(random_rows$Genus))
  average_unique_strings <- unique_strings / 10
  
  # Making sure this is working before adding to empty dataframe 
  # print("Unique strings:")
  # cat(unique_strings)
  # print("average unique strings:")
  # cat(average_unique_strings)
  # Sys.sleep(2)
  
  average_genera <- rbind(average_genera, data.frame(Average = average_unique_strings))
}

# View the resulting dataframe
print(average_genera)

# This plot doesnt work
diy_rarefaction_plot <- average_genera %>%
  ggplot(aes(x = 1:nrow(data), y = Average)) +
  geom_line() +  # Add black outline
  geom_point() + 
  labs(x = "Times sampled", y = "Average genera") +
  guides(fill = "none") +
  theme_classic()
diy_rarefaction_plot

# ------------ this didn't work. starting over.

# ATTEMPTING A DIFFERENT, NOT RAREFACTION CURVE
# Genera and individuals caught over time

# plot 1: black and white plot that has disjunct flat line over Fall/Winter
plot1 <- data %>%
  group_by(Collection.Date) %>%
  summarise(Count = n()) %>%
  ggplot(aes(x = Collection.Date, y = Count)) +
  # ggplot(aes(x = 1:nrow(data), y = '')) # how to get x axis to be the total number of columns
  geom_line() +  # Add black outline
  geom_point() + 
  labs(x = "Date", y = "Samples") +
  guides(fill = "none") +
  theme_classic()
plot1

# plot 2: individuals AND genera over time line plot with disjunct flatl ine over fall/winter
plot2 <- data %>%
  group_by(Collection.Date) %>%
  summarise(individuals = n(), 
            genera = n_distinct(Genus)) %>%
  pivot_longer(cols = c(individuals, genera), 
               names_to = "variable", 
               values_to = "value") %>%
  ggplot(aes(x = Collection.Date, y = value, color = variable)) +
  # ggplot(aes(x = 1:nrow(data), y = '')) # how to get x axis to be the total number of columns +
  geom_line() +
  scale_color_manual(values = c("individuals" = "red", "genera" = "blue")) +
  labs(x = "Date", y = "Number") +
  theme_classic()
plot2

# "plot 4" (side by side plots of 2023 and 2024 using patchwork)
# This should solve the disjunct flat line, and have two clean line plots

to_rarefy <- data %>%
  group_by(Collection.Date) %>%
  summarise(individuals = n(), 
            genera = n_distinct(Genus))


# Create plot for the year 2023
genera_plot_2023 <- to_rarefy %>%
  filter(year(Collection.Date) == 2023) %>%
  pivot_longer(cols = c(individuals, genera), 
               names_to = "variable", 
               values_to = "value") %>%
  ggplot(aes(x = Collection.Date, y = value, color = variable)) +
  geom_line() +
  scale_color_manual(values = c("individuals" = "red", "genera" = "blue")) +
  labs(x = "Date", y = "Number", title = "Year 2023") +
  theme_classic()

# Create plot for the year 2024
genera_plot_2024 <- to_rarefy %>%
  filter(year(Collection.Date) == 2024) %>%
  pivot_longer(cols = c(individuals, genera), 
               names_to = "variable", 
               values_to = "value") %>%
  ggplot(aes(x = Collection.Date, y = value, color = variable)) +
  geom_line() +
  scale_color_manual(values = c("individuals" = "red", "genera" = "blue")) +
  labs(x = "Date", y = "Number", title = "Year 2024") +
  theme_classic()

# Display the plots side by side using patchwork
genera_plot_2023 + genera_plot_2024





