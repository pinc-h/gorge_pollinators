# Calculating rarefaction curve for the 2024 Pollinator Meadow Insect Survey
# Created by Alex Pinch, last edited Aug 29 2024
# ....without a package. this is dumb.

library(tidyverse)
library(vegan)
library(patchwork)

# Set working directory to where you've downloaded the specimen catalogue as a .csv
setwd("/Users/alexpinch/Documents/obs_vault/Gorge_work_2024/gorge_pollinators_2024/data")

# loading and filtering the data, selecting the columns they have in common and binding them together
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

# YEaAAHAHAHH FIGURED THIS OUT
data <- data %>%
  filter(Genus != "")


to_rarefy <- data %>%
  group_by(Collection.Date) %>%
  summarise(individuals = n(), 
            genera = n_distinct(Genus)) %>%
  select(-Collection.Date)


g <- to_rarefy$genera
raremax <- min(rowSums(to_rarefy))
g_rare <- rarefy(to_rarefy, raremax)
plot(g, g_rare, xlab = "Observed Num of Genera", ylab = "Rarefied Num of Genera")
abline(0,1)
rarecurve(to_rarefy, step = 20, sample = raremax, col = "blue", cex = 0.6)

# These look weird. Either data is too low resolution or im doing something weird