# Data Analysis for the 2024 Pollinator Meadow Insect Survey
# Alex Pinch
# Modified from last year's script, created Jul 24 2024

# tidyverse is for making plots
library(tidyverse)

# Set working directory to where you've downloaded the specimen catalogue as a .csv
setwd("/Users/alexpinch/GitHub/private/gorge_pollinators_2024/data")

# loading and filtering the data, selecting the columns they have in common and binding them together
pan_trap_data_2024 <- read.csv("20240918_pantraps_2024.csv")
visitation_sampling_data_2024 <- read.csv("20240918_visitation_2024.csv")
pan_trap_data_2023 <- read.csv("20240918_pantraps_2023.csv")
visitation_sampling_data_2023 <- read.csv("20240918_visitation_2023.csv")

pan_trap_data_2023$Collection.Date <- format(as.Date(pan_trap_data_2023$Collection.Date, format = "%B %d, %Y"), "%d-%b-%y")
visitation_sampling_data_2023$Collection.Date <- format(as.Date(visitation_sampling_data_2023$Collection.Date, format = "%B %d, %Y"), "%d-%b-%y")
pan_trap_data_2024$Collection.Date <- format(as.Date(pan_trap_data_2024$Collection.Date, format = "%B %d, %Y"), "%d-%b-%y")
visitation_sampling_data_2024$Collection.Date <- format(as.Date(visitation_sampling_data_2024$Collection.Date, format = "%B %d, %Y"), "%d-%b-%y")


pan_trap_data <- rbind(pan_trap_data_2023, pan_trap_data_2024)
visitation_sampling_data <- rbind(visitation_sampling_data_2023, visitation_sampling_data_2024)

# Identify common columns and combine data frames
common_columns <- intersect(colnames(pan_trap_data), colnames(visitation_sampling_data))
data <- rbind(
  pan_trap_data %>% select(all_of(common_columns)),
  visitation_sampling_data %>% select(all_of(common_columns))
)

# data <- data %>%
#   filter(!grepl("Non-survey", Comments)) %>% # removes non-survey
#   filter(Family != "") # removes individuals not identified to family

data <- data %>%
  mutate(Family = ifelse(is.na(Family) | Family == "", "Unidentified", Family),
         Genus = ifelse(is.na(Genus) | Genus == "", "Unidentified", Genus))
  
# First plot: Total counts across both pan trap and visitation sampling

plot1 <- data %>%
  group_by(Family) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  mutate(Family = factor(Family, levels = Family)) %>% # need this for descending order, idk what it does
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
  group_by(Family, Genus) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ggplot(aes(x=Family, y=Count, fill=Genus)) +
  geom_bar(stat = "identity", color = "black") +
  labs(x = "Family", y="Total Specimens Caught", fill="Genus") +
  theme_classic() +
  theme(axis.text.x=element_text(face="italic",angle=45, vjust=1, hjust=1), 
        plot.margin=margin(t=10,r=10,b=10,l=45)) 
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

visitation_sampling_data <- visitation_sampling_data %>%
  mutate(
    Plant = case_when(
      
      grepl("Aster", Plant) ~ "Symphyotrichum subspicatum",
      grepl("Common Wooly Sunflower", Plant) ~ "Eriophyllum lanatum",
      grepl("Douglas Aster", Plant) ~ "Symphyotrichum subspicatum",
      grepl("Gumweed", Plant) ~ "Grindelia integrifolia",
      grepl("Henderson's Checkermallow", Plant) ~ "Sidalcea hendersonii",
      grepl("N/A", Plant) ~ "Red Columbine",
      grepl("Nootka Rose", Plant) ~ "Rosa nutkana",
      grepl("Self-heal", Plant) ~ "Prunella vulgaris",
      grepl("Snowberry", Plant) ~ "Symphoricarpos albus",
      grepl("Yarrow", Plant) ~ "Achillea millefolium",
      grepl("Goldenrod", Plant) ~ "Solidago canadensis",
      grepl("Nodding Onion", Plant) ~ "Allium cernuum",
      grepl("Prunella", Plant) ~ "Prunella vulgaris",
      grepl("Red Columbine", Plant) ~ "Aquilegia canadensis",
      
      TRUE ~ NA_character_
    )
  )

# Doesnt work, change so that $Family is included for each case_when.
# visitation_sampling_data <- visitation_sampling_data %>%
#   mutate(
#   Genus = case_when(
#     grepl("", Genus) ~ "Unidentified"$Family,
#     TRUE ~ NA_character_
#   )
# )

visitation_sampling_data <- visitation_sampling_data %>%
  mutate(Family = ifelse(is.na(Family) | Family == "", "Unidentified", Family),
         Genus = ifelse(is.na(Genus) | Genus == "", "Unidentified", Genus))

plot3 <- visitation_sampling_data %>%
  filter(Plant != "", Plant != "N/A") %>%
  group_by(Genus, Plant) %>%
  summarize(Count = n()) %>%
  ggplot(aes(x=Plant, y=Count, fill=Genus)) +
  geom_bar(stat = "identity", color = "black") +
  labs(x = "Plant Species", y="Total Specimens Caught", fill="Genus") +
  theme_classic() +
  theme(axis.text.x=element_text(face="italic",angle=45, vjust=1, hjust=1), 
        plot.margin=margin(t=10,r=10,b=10,l=45))
plot3
ggsave(filename = "plot3.jpeg", plot = plot3, height = 5, width = 7, units = "in")

# plot 4 idea: number of genera in PM vs. CG