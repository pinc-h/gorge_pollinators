library(tidyverse)
library(ggsci)
setwd("/Users/alexpinch/Documents/obs_vault/Gorge_work_2024/gorge_pollinators_2024/data") #data
df <- read.csv("observations-472967.csv")
plot1 <- df %>%
  filter(as.Date(observed_on) >= as.Date("2008-01-01")) %>% # Excluding personal observations dated prior to iNaturalist's founding date
  mutate(observed_on = as.Date(observed_on)) %>%
  #mutate(observed_on = floor_date(observed_on, unit = "month")) %>%
  group_by(observed_on) %>%
  summarise(Count = n()) %>%
  ggplot(aes(x = observed_on, y = Count)) +
  geom_line(color = "blue") +
  labs(x = "Date", y = "Number of Observations") +
  theme_classic()
plot1
