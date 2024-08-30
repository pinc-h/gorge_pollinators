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

# Set working directory to where you've downloaded the specimen catalogue as a .csv
setwd("/Users/alexpinch/Documents/obs_vault/Gorge_work_2024/gorge_pollinators_2024/data")

# loading and filtering the data, selecting the columns they have in common and binding them together
pan_trap_data <- read.csv("20240729_pantraps_2024.csv")
visitation_sampling_data <- read.csv("20240729_visitation_2024.csv")
# Identify common columns and combine data frames
common_columns <- intersect(colnames(pan_trap_data), colnames(visitation_sampling_data))
data <- rbind(
  pan_trap_data %>% select(all_of(common_columns)),
  visitation_sampling_data %>% select(all_of(common_columns))
)

data <- data %>%
  filter(!grepl("Non-survey", Comments)) %>% # removes non-survey
  filter(Family != "") # removes individuals not identified to family

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

plot1 <- average_genera %>%
  ggplot(aes(x = 1:nrow(data), y = Average)) +
  geom_line() +  # Add black outline
  geom_point() + 
  labs(x = "Times sampled", y = "Average genera") +
  guides(fill = "none") +
  theme_classic()
plot1
ggsave(filename = "plot1.jpeg", plot = plot1, height = 5, width = 7, units = "in")




