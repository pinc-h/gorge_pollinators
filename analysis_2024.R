# Data Analysis for the 2024 Pollinator Meadow Insect Survey
# Created by Alex Pinch
# Modified from last year's script, last edited Jul 24 2024

# tidyverse is for making plots
library(tidyverse)

# Set working directory to where you've downloaded the specimen catalogue as a .csv
setwd("/Users/alexpinch/Documents/obs_vault/Gorge_work_2024/gorge_pollinators_2024/data")

# loading and filtering the data, selecting the columns they have in common and binding them together
pan_trap_data <- read.csv("20240724_pantraps2024.csv")
visitation_sampling_data <- read.csv("20240724_visitation2024.csv")

# Identify common columns and combine data frames
common_columns <- intersect(colnames(pan_trap_data), colnames(visitation_sampling_data))
data <- rbind(
  pan_trap_data %>% select(all_of(common_columns)),
  visitation_sampling_data %>% select(all_of(common_columns))
)

data <- data %>%
  filter(!grepl("Non-survey", Comments)) %>% # removes non-survey
  filter(Family != "") # removes individuals not identified to family


# First plot: Total counts across both pan trap and visitation sampling
# Count the number of individuals in each family
plot1_counts <- table(data$Family)
names(plot1_counts)[1] <- ""
# Remove the row where the family name is empty
if ("" %in% names(plot1_counts)) {
  plot1_counts <- plot1_counts[names(plot1_counts) != ""]
}
as.data.frame(plot1_counts) %>%
  ggplot(aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity", color = "black") +  # Add black outline
  labs(x = "Families", y = "Total Specimens Caught", fill = "Pollinator Families") +
  guides(fill = "none") +
  theme_classic()

plot1 <- data %>%
  filter(Family != "" & Genus != "") %>%
  group_by(Family, Genus) %>%
  summarise(Count = n()) %>%
  ggplot(aes(x=Family, y=Count, fill=Genus)) +
  geom_bar(stat = "identity", color = "black") +
  labs(x = "Family", y="Total Caught", fill="Genus") +
  theme_classic()

plot1
