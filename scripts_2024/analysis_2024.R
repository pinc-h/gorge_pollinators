# Data Analysis for the 2024 Pollinator Meadow Insect Survey
# Alex Pinch
# Modified from last year's script, created Jul 24 2024

# tidyverse is for making plots
library(tidyverse)
library(scales)  # for hue_pal()
library(lubridate)
library(viridis)
library(patchwork)

# Set working directory to where you've downloaded the specimen catalogue as a .csv
setwd("/Users/alexpinch/GitHub/private/gorge_pollinators")

# loading and filtering the data, selecting the columns they have in common and binding them together
pan_trap_data_2024 <- read.csv("data/20240918_pantraps_2024.csv")
visitation_sampling_data_2024 <- read.csv("data/20240918_visitation_2024.csv")
visitation_sampling_data_2024$Collection.Date <- format(as.Date(visitation_sampling_data_2024$Collection.Date, format = "%B %d, %Y"), "%d-%b-%y")
pan_trap_data_2023 <- read.csv("data/20240918_pantraps_2023.csv")
visitation_sampling_data_2023 <- read.csv("data/20240918_visitation_2023.csv")
visitation_sampling_data_2023$Collection.Date <- format(as.Date(visitation_sampling_data_2023$Collection.Date, format = "%B %d, %Y"), "%d-%b-%y")
pan_trap_data <- rbind(pan_trap_data_2023, pan_trap_data_2024)
visitation_sampling_data <- rbind(visitation_sampling_data_2023, visitation_sampling_data_2024)
visitation_sampling_data$Collection.Date <- as.Date(visitation_sampling_data$Collection.Date, format = "%d-%b-%y")


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

# Creating unique colour palette where unidentified are shown in black
genera_colours <- setNames(scales::hue_pal()(length(unique(data$Genus))),
                          unique(data$Genus))
genera_colours["Unidentified"] <- "black"
  
# -----------
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
# ggsave(filename = "plot1.jpeg", plot = plot1, height = 5, width = 7, units = "in")

# --------
# Second plot, total counts for genus across both pan and visitation sampling

plot2 <- data %>%
  filter(Family != "Unidentified") %>%
  group_by(Family, Genus) %>%
  summarise(Count = n()) %>%
  ggplot(aes(x=Family, y=Count, fill=Genus)) +
  geom_bar(stat = "identity", color = "black") +
  labs(x = "Family", y="Total Specimens Caught", fill="Genus") +
  theme_classic() +
  theme(axis.text.x=element_text(face="italic",angle=45, vjust=1, hjust=1), 
        plot.margin=margin(t=10,r=10,b=10,l=45)) +
  scale_fill_manual(values = genera_colours)
plot2
# ggsave(filename = "plot2.jpeg", plot = plot2, height = 5, width = 7, units = "in")

# --------
# Third plot, flower visitation data

# This converts various flower codes and common names to scientific names
visitation_sampling_data <- visitation_sampling_data %>%
  mutate(
    Plant = case_when(
      grepl("TAROFF", Plant) ~ "Taraxacum officinale",
      grepl("Aster", Plant) ~ "Symphyotrichum subspicatum",
      grepl("Common Wooly Sunflower", Plant) ~ "Eriophyllum lanatum",
      grepl("ERILAN", Plant) ~ "Eriophyllum lanatum",
      grepl("Douglas Aster", Plant) ~ "Symphyotrichum subspicatum",
      grepl("SYMSUB", Plant) ~ "Symphyotrichum subspicatum",
      grepl("Gumweed", Plant) ~ "Grindelia integrifolia",
      grepl("GRIINT", Plant) ~ "Grindelia integrifolia",
      grepl("Henderson's Checkermallow", Plant) ~ "Sidalcea hendersonii",
      grepl("SIDSPP", Plant) ~ "Sidalcea hendersonii",
      grepl("ROSNUT", Plant) ~ "Rosa nutkana",
      grepl("Nootka Rose", Plant) ~ "Rosa nutkana",
      grepl("Self-heal", Plant) ~ "Prunella vulgaris",
      grepl("Prunella", Plant) ~ "Prunella vulgaris",
      grepl("PRUVUL", Plant) ~ "Prunella vulgaris",
      grepl("Snowberry", Plant) ~ "Symphoricarpos albus",
      grepl("SYMALB", Plant) ~ "Symphoricarpos albus",
      grepl("Yarrow", Plant) ~ "Achillea millefolium",
      grepl("ACHMIL", Plant) ~ "Achillea millefolium",
      grepl("Goldenrod", Plant) ~ "Solidago spp.",
      grepl("SOLSPP", Plant) ~ "Solidago spp.",
      grepl("Nodding Onion", Plant) ~ "Allium cernuum",
      grepl("Red Columbine", Plant) ~ "Aquilegia formosa",
      grepl("AQUFOR", Plant) ~ "Aquilegia formosa",
      grepl("GERDIS", Plant) ~ "Geranium spp.",
      TRUE ~ NA_character_
    )
  )

visitation_sampling_data <- visitation_sampling_data %>%
  mutate(Family = ifelse(is.na(Family) | Family == "", "Unidentified", Family),
         Genus = ifelse(is.na(Genus) | Genus == "", "Unidentified", Genus))

# Updating our colours palette but plugging in this dataframe's name (visitation_sampling_data replaces "data")
genera_colours <- setNames(scales::hue_pal()(length(unique(visitation_sampling_data$Genus))),
                          unique(visitation_sampling_data$Genus))
genera_colours["Unidentified"] <- "black"

plot3 <- visitation_sampling_data %>%
  filter(Plant != "", Plant != "N/A") %>%
  group_by(Genus, Plant) %>%
  summarize(Count = n()) %>%
  ggplot(aes(x=Plant, y=Count, fill=Genus)) +
  geom_bar(stat = "identity", color = "black") +
  labs(x = "Plant Species", y="Total Specimens Caught", fill="Genus") +
  theme_classic() +
  theme(axis.text.x=element_text(face="italic",angle=45, vjust=1, hjust=1), 
        plot.margin=margin(t=10,r=10,b=10,l=45)) +
  scale_fill_manual(values = genera_colours)
plot3
# ggsave(filename = "plot3.jpeg", plot = plot3, height = 5, width = 7, units = "in")

# plot 4: flower activity
plot4_2023 <- visitation_sampling_data %>%
  filter(year(Collection.Date) == 2023) %>%   # Filter for the year 2024
  mutate(Collection.Date = factor(Collection.Date)) %>%
  filter(Plant != "" & Plant != "N/A") %>%                   
  group_by(Collection.Date, Plant) %>%        # Group by Collection.Date and Plant
  summarise(visitation_count = n(), .groups = "drop") %>%
  complete(Collection.Date, Plant, fill = list(visitation_count = 0)) %>%
  ggplot(aes(x = Collection.Date, y = Plant, fill = visitation_count)) +
  geom_tile(na.rm = FALSE) +
  labs(x = "Date", y = "Plant species", fill="Pollinators caught", title = "2023") +
  theme_classic() +
  theme(
    axis.text.y = element_text(face = "italic", angle = 45, vjust = 1, hjust = 1), 
    axis.text.x = element_text(face = "italic", angle = 45, vjust = 1, hjust = 1),  # Tilt x-axis text
    plot.margin = margin(t = 10, r = 10, b = 10, l = 45)
  ) +
  scale_fill_viridis(option="viridis")

plot4_2024 <- visitation_sampling_data %>%
  filter(year(Collection.Date) == 2024) %>%   # Filter for the year 2024
  mutate(Collection.Date = factor(Collection.Date)) %>%
  filter(Plant != "" & Plant != "N/A") %>%                   
  group_by(Collection.Date, Plant) %>%        # Group by Collection.Date and Plant
  summarise(visitation_count = n(), .groups = "drop") %>%
  complete(Collection.Date, Plant, fill = list(visitation_count = 0)) %>%
  ggplot(aes(x = Collection.Date, y = Plant, fill = visitation_count)) +
  geom_tile(na.rm = FALSE) +
  labs(x = "Date", y = "Plant species", fill="Pollinators caught", title = "2024") +
  theme_classic() +
  theme(
    axis.text.y = element_text(face = "italic", angle = 45, vjust = 1, hjust = 1), 
    axis.text.x = element_text(face = "italic", angle = 45, vjust = 1, hjust = 1),  # Tilt x-axis text
    plot.margin = margin(t = 10, r = 10, b = 10, l = 45)
  ) +
  # theme(axis.text.y=element_text(face="italic",angle=45, vjust=1, hjust=1), 
  #       plot.margin=margin(t=10,r=10,b=10,l=45)) +
  scale_fill_viridis(option="viridis")

plot4_2023 + plot4_2024 + plot_layout(guides = "collect")
