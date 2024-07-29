# Data Analysis for the 2024 Pollinator Meadow Insect Survey
# Created by Alex Pinch
# Modified from last year's script, last edited Jul 24 2024

# tidyverse is for making plots
library(tidyverse)

# Set working directory to where you've downloaded the specimen catalogue as a .csv
setwd("/Users/alexpinch/Documents/obs_vault/Gorge_work_2024/gorge_pollinators_2024/data")

# loading and filtering the data, selecting the columns they have in common and binding them together
pan_trap_data <- read.csv("20240729_pantraps_2023.csv")
visitation_sampling_data <- read.csv("20240729_visitation_2023.csv")

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

plot1 <- data %>%
  group_by(Family) %>%
  summarise(Count = n()) %>%
  ggplot(aes(x = Family, y = Count, fill = Family)) +
  geom_bar(stat = "identity", color = "black") +  # Add black outline
  labs(x = "Family", y = "Total Specimens Caught", fill = "Family") +
  guides(fill = "none") +
  theme_classic() +
  theme(axis.text.x=element_text(face="italic",angle=45, vjust=1, hjust=1), 
        plot.margin=margin(t=10,r=10,b=10,l=45))
plot1
ggsave(filename = "plot1.jpeg", plot = plot1, height = 5, width = 7, units = "in")



# Second plot, total counts for genus across both pan and visitation sampling
plot2 <- data %>%
  filter(Genus != "") %>%
  group_by(Family, Genus) %>%
  summarise(Count = n()) %>%
  ggplot(aes(x=Family, y=Count, fill=Genus)) +
  geom_bar(stat = "identity", color = "black") +
  labs(x = "Family", y="Total Specimens Caught", fill="Genus") +
  theme_classic()
plot2
ggsave(filename = "plot2.jpeg", plot = plot2, height = 5, width = 7, units = "in")

# Third plot, flower visitation data

# This converts flower codes for 2023, not 2024 data: (highlight + press CMD-Shift-C to uncomment)

# visitation_sampling_data <- visitation_sampling_data %>%
#   mutate(
#     Plant = case_when(
# 
#       grepl("TAROFF", Plant) ~ "Dandelion",
#       grepl("ERILAN", Plant) ~ "Common Wooly Sunflower",
#       grepl("ROSNUT", Plant) ~ "Nootka Rose",
#       grepl("ACHMIL", Plant) ~ "Yarrow",
#       grepl("SIDSPP", Plant) ~ "Henderson's Checkermallow",
#       grepl("AQUFOR", Plant) ~ "Red Columbine",
#       grepl("GERDIS", Plant) ~ "Geranium",
#       grepl("SYMSUB", Plant) ~ "Douglas Aster",
#       grepl("SOLSPP", Plant) ~ "Goldenrod",
#       grepl("SYMALB", Plant) ~ "Snowberry",
#       grepl("GRIINT", Plant) ~ "Gumweed",
#       grepl("PRUVUL", Plant) ~ "Self-heal",
#       TRUE ~ NA_character_
#     )
#   )

plot3 <- visitation_sampling_data %>%
  filter(Family != "", Plant != "") %>%
  group_by(Family, Plant) %>%
  summarize(Count = n()) %>%
  ggplot(aes(x=Plant, y=Count, fill=Family)) +
  geom_bar(stat = "identity", color = "black") +
  labs(x = "Plant Species", y="Total Specimens Caught", fill="Family") +
  theme_classic() +
  theme(axis.text.x=element_text(face="italic",angle=45, vjust=1, hjust=1), 
        plot.margin=margin(t=10,r=10,b=10,l=45))
plot3
ggsave(filename = "plot3.jpeg", plot = plot3, height = 5, width = 7, units = "in")

# plot 4 idea: number of genera in PM vs. CG
# plot 5 idea: temporal data.
# plot 6 idea: rarefaction curve
