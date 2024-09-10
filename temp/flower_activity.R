# Alex Pinch, last edited Sept 10
# Visualizing flower activity through the summer (including unidentified bees/wasps)

library(tidyverse)
library(patchwork)
library(viridis)

setwd("/Users/alexpinch/Documents/obs_vault/Gorge_work_2024/gorge_pollinators_2024/data")

# See analysis_2024.R for explanation of this block, basically data cleaning
visitation_sampling_data_2023 <- read.csv("20240905_visitation_2023.csv")
visitation_sampling_data_2023$Collection.Date <- format(as.Date(visitation_sampling_data_2023$Collection.Date, format = "%B %d, %Y"), "%d-%b-%y")
visitation_sampling_data_2024 <- read.csv("20240905_visitation_2024.csv")
visitation_sampling_data_2024$Collection.Date <- format(as.Date(visitation_sampling_data_2024$Collection.Date, format = "%B %d, %Y"), "%d-%b-%y")

data <- rbind(visitation_sampling_data_2023, visitation_sampling_data_2024)

data$Collection.Date <- as.Date(data$Collection.Date, format = "%d-%b-%y")

plot1 <- data %>%
  filter(year(Collection.Date) == 2024) %>%
  filter(Plant != "" & Plant != "N/A") %>%
  group_by(Plant) %>%
  summarise(Count = n()) %>%
  ggplot(aes(x = Plant, y = Count, fill = Plant)) +
  geom_bar(stat = "identity") +
  labs(x = "Plant", y = "Number caught on plant") +
  guides(fill = "none") +
  theme_classic() +
  theme(axis.text.x=element_text(face="italic",angle=45, vjust=1, hjust=1), 
        plot.margin=margin(t=10,r=10,b=10,l=45))
plot1

plot2 <- data %>%
  filter(year(Collection.Date) == 2024) %>%   # Filter for the year 2024
  mutate(Collection.Date = factor(Collection.Date)) %>%
  filter(Plant != "" & Plant != "N/A") %>%                   
  group_by(Collection.Date, Plant) %>%        # Group by Collection.Date and Plant
  summarise(visitation_count = n(), .groups = "drop") %>%
  complete(Collection.Date, Plant, fill = list(visitation_count = 0)) %>%
  ggplot(aes(x = Collection.Date, y = Plant, fill = visitation_count)) +
  geom_tile(na.rm = FALSE) +
  labs(x = "Date", y = "Plant species") +
  theme_classic() +
  theme(axis.text.y=element_text(face="italic",angle=45, vjust=1, hjust=1), 
        plot.margin=margin(t=10,r=10,b=10,l=45)) +
  scale_fill_viridis(option="viridis")
plot2

