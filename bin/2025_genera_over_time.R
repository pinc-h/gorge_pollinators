# Alex Pinch, last edited Aug 25 2025

# This code is to recreate the 2024 plot of sampling over time

library(tidyverse)
library(patchwork)

setwd("/Users/pinch/Desktop/gorge_pollinators/lib/data")
data <- read.csv ("cleaned_pollinator_data_2025.csv")

data$Date <- format(as.Date(data$Date, format = "%m/%d/%Y"), "%d-%b-%y")
data$Genus <- word(data$Latin.Name, 1)

plot <- data %>%
  group_by(Date) %>%
  summarise(individuals = n(), 
            genera = n_distinct(Genus)) %>%
  pivot_longer(cols = c(individuals, genera), 
               names_to = "variable", 
               values_to = "value") %>%
  ggplot(aes(x = as.Date(Date, format = "%d-%b-%y"), y = value, color = variable, group = variable)) +
  geom_line() +
  scale_color_manual(values = c("individuals" = "red", "genera" = "blue")) +
  labs(title = "2025", x = "Date", y = "Number caught", color = "Type") +
  theme_classic() +
  scale_x_date(date_labels = "%d-%b-%y")
plot
