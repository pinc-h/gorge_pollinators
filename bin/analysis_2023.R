# Data Analysis for the 2023 Pollinator Meadow Insect Survey
# Created by Alex Pinch
# Last edited by Alex Pinch, August 26 2023

# tidyverse is for making plots
library(tidyverse)

# Set working directory to where you've downloaded the the data
setwd("/Users/alexpinch/GitHub/private/gorge_pollinators_2023")

# Load CSV file
pan_trap_data <- read.csv("20230905_pan_traps.csv")
visitation_sampling_data <- read.csv("20230905_visitation_sampling.csv")

# Make sure both datasets have the same column names
common_columns <- intersect(colnames(pan_trap_data), colnames(visitation_sampling_data))

# Filter for the common columns
filtered_pan_trap_data <- pan_trap_data %>%
  select(all_of(common_columns))
filtered_visitation_sampling_data <- visitation_sampling_data %>%
  select(all_of(common_columns))

# Combine dataframes using rbind
data <- rbind(filtered_pan_trap_data, filtered_visitation_sampling_data)

# A custom plot template to Connor's nitpicks
theme_connor <- theme_minimal() +
  theme( 
    legend.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.background = element_rect(fill = "white") 
  )

###########
# ---
# First plot: Total counts across both pan trap and visitation sampling
# ---

# Count the number of individuals in each family
plot1_counts <- table(data$Family)
names(plot1_counts)[1] <- ""
# Remove the row where the family name is empty
if ("" %in% names(plot1_counts)) {
  plot1_counts <- plot1_counts[names(plot1_counts) != ""]
}

# Very simple first plot of aggregate caught from each family
as.data.frame(plot1_counts) %>%
  ggplot(aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity", color = "black") +  # Add black outline
  labs(x = "Families", y = "Total Specimens Caught", fill = "Pollinator Families") +
  guides(fill = "none") +
  theme_connor

###########
# ---
# Second plot: Pan trap functional group abundance
# ---

# More useful to plot as functional groups than strictly families

# Defining functional groups for the pan trap data
functional_group_pan_trap_data <- pan_trap_data %>%
  filter(!grepl("Non-survey", Comments)) %>%
  mutate(
    Functional.Group = case_when(
      grepl("Wasp", Comments) ~ "Pollinating Wasps",
      grepl("Bombus", Genus) ~ "Bumblebees",
      grepl("mellifera", Species) ~ "Non-native Honeybees",
      grepl("Syrphidae", Family) ~ "Pollinating Flies",
      !grepl("mellifera", Species) & !grepl("Diptera", Order) ~ "Other Pollinating Bees",
      TRUE ~ NA_character_
    ),
    # If including Visitation sampling data for this plot, uncomment line below, it lumps all the pollinator meadow zones into just Pollinator Meadow
    # Location = ifelse(Location %in% c("PM1", "PM2", "PM3", "PM4"), "PM", Location)
  )

# Lengthen location acronyms
functional_group_pan_trap_data <- functional_group_pan_trap_data %>%
  mutate (
    Location = case_when(
      grepl("PM", Location) ~ "Pollinator Meadow",
      grepl("EB", Location) ~ "Edible Beds",
      grepl("CG", Location) ~ "Control Garden",
      TRUE ~ NA_character_
    )
  )

grouped_pan_trap_data <-
  functional_group_pan_trap_data %>%
  group_by(Functional.Group, Location) %>%
  summarise(Count = n())

# Alter custom theme for this plot specifically
theme_connor <- theme_connor +
  theme(
    legend.position = c(0.14, 0.88)
  )


grouped_pan_trap_data %>%
  ggplot(aes(x=Location, y=Count, fill=Functional.Group)) +
  geom_bar(stat = "identity",  color = "black") +
  labs(x = "Location", y="Total Specimens Caught", fill="Pollinator Type") +
  theme_connor




###########
# ---
# Third plot: Distribution of families across flower type
# ---
# More useful to plot as functional groups than strictly families

# Defining functional groups for the pan trap data
functional_group_visitation_sampling_data <- visitation_sampling_data %>%
  filter(!grepl("Non-survey", Comments)) %>%
  mutate(
    Functional.Group = case_when(
      grepl("Wasp", Comments) ~ "Pollinating Wasps",
      grepl("Bombus", Genus) ~ "Bumblebees",
      grepl("mellifera", Species) ~ "Non-native Honeybees",
      grepl("Syrphidae", Family) ~ "Pollinating Flies",
      !grepl("mellifera", Species) & !grepl("Diptera", Order) ~ "Other Pollinating Bees",
      TRUE ~ NA_character_
    ),
    Plant = case_when(
      grepl("TAROFF", Plant) ~ "Dandelion",
      grepl("ERILAN", Plant) ~ "Common Wooly Sunflower",
      grepl("ROSNUT", Plant) ~ "Nootka Rose",
      grepl("ACHMIL", Plant) ~ "Yarrow",
      grepl("SIDSPP", Plant) ~ "Mallow",
      grepl("AQUFOR", Plant) ~ "Red Columbine",
      grepl("GERDIS", Plant) ~ "Geranium",
      grepl("SYMSUB", Plant) ~ "Aster",
      grepl("SOLSPP", Plant) ~ "Goldenrod",
      grepl("SYMALB", Plant) ~ "Snowberry",
      grepl("GRIINT", Plant) ~ "Gumweed",
      grepl("PRUVUL", Plant) ~ "Self-heal",
      
      TRUE ~ NA_character_
    )
  )

# Lengthen location acronyms
functional_group_visitation_sampling_data <- functional_group_visitation_sampling_data %>%
  mutate (
    Location = case_when(
      grepl("PM", Location) ~ "Pollinator Meadow",
      grepl("EB", Location) ~ "Edible Beds",
      grepl("CG", Location) ~ "Control Garden",
      TRUE ~ NA_character_
    )
  )

# Alter Connor's theme for this plot
theme_connor <- theme_connor +
  theme(
    legend.position = c(0.90, 0.91)
  )

grouped_visitation_sampling_data <-
  functional_group_visitation_sampling_data %>%
  group_by(Functional.Group, Plant) %>%
  summarise(Count = n())

grouped_visitation_sampling_data %>%
  ggplot(aes(x=Plant, y=Count, fill=Functional.Group)) +
  geom_bar(stat = "identity", color = "black") +
  labs(x = "Plant Species", y="Total Specimens Caught", fill="Pollinator Type") +
  theme_connor

