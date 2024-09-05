# Calculating rarefaction curve for the 2024 Pollinator Meadow Insect Survey
# Created by Alex Pinch, last edited Aug 29 2024
# ....without a package. this is dumb.

      #   Ok thinking out loud for me to look at in the morning:
      #   From Wikipedia: “Rarefaction curves are created by randomly re-sampling the pool of N samples multiple times and then plotting the average number of species found in each sample.”
      #   How many times is multiple times? Until you get the asymptote?
      #   So pull random samples & get average number of genera, do this enough times to get an asymptote?
      #   Did I summarize correctly
      #   Connor — 07/26/2024 12:40 AM
      #   Do it a number of time equal to the lowest sample size

# tidyverse is for making plots
library(tidyverse)
library(vegan)

# Set working directory to where you've downloaded the specimen catalogue as a .csv
setwd("/Users/alexpinch/Documents/obs_vault/Gorge_work_2024/gorge_pollinators_2024/data")

# loading and filtering the data, selecting the columns they have in common and binding them together
pan_trap_data_2023 <- read.csv("20240729_pantraps_2023.csv")
visitation_sampling_data_2023 <- read.csv("20240729_visitation_2024.csv")
pan_trap_data_2024 <- read.csv("20240729_pantraps_2024.csv")
visitation_sampling_data_2024 <- read.csv("20240729_visitation_2024.csv")

# this might be a one-off, for some reason pan trap data for 2023 the date is formatted differently. changing to match the others
pan_trap_data_2023$Collection.Date <- format(as.Date(pan_trap_data_2023$Collection.Date, format = "%B %d, %Y"), "%d-%b-%y")

pan_trap_data <- rbind(pan_trap_data_2023, pan_trap_data_2024)
visitation_sampling_data <- rbind(visitation_sampling_data_2023, visitation_sampling_data_2024)

common_columns <- intersect(colnames(pan_trap_data), colnames(visitation_sampling_data))
data <- rbind(
  pan_trap_data %>% select(all_of(common_columns)),
  visitation_sampling_data %>% select(all_of(common_columns))
)

data <- data %>%
  filter(!grepl("Non-survey", Comments)) %>% # removes non-survey
  filter(Family != "") # removes individuals not identified to family

data$Collection.Date <- as.Date(data$Collection.Date, format = "%d-%b-%y")

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
ggsave(filename = "plot1.jpeg", plot = plot1, height = 5, width = 7, units = "in")

data <- data %>%
  mutate(Genus = ifelse(Genus == "", paste0("unidentified_", Family), Genus))

# YEaAAHAHAHH FIGURED THIS OUT
to_rarefy <- data %>%
  group_by(Collection.Date) %>%
  summarise(individuals = n(), 
            num_of_genera = n_distinct(Genus))

# Print the summarized result
print(to_rarefy)
  
  





