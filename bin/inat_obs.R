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

plot2 <- df %>%
  filter(num_identification_agreements >= 2) %>% # Excluding personal observations dated prior to iNaturalist's founding date
  filter(iconic_taxon_name == "Insecta") %>%
  group_by(common_name) %>%
  summarise(Count = n()) %>%
  ggplot(aes(x = common_name, y = Count, fill = common_name)) +
  geom_bar(stat="identity") +
  guides(fill = "none") +
  labs(x = "Species", y = "Number of Observations") +
  theme_classic() +
  theme(axis.text.x=element_text(face="italic",angle=45, vjust=1, hjust=1), 
        plot.margin=margin(t=10,r=10,b=10,l=45))
plot2

plot3 <- df %>%
  filter(user_login == "emoellenhoff") %>%
  filter(num_identification_agreements >= 1) %>%
  group_by(common_name) %>%
  filter(common_name != "") %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  mutate(common_name = factor(common_name, levels = common_name)) %>%
  ggplot(aes(x = common_name, y = Count, fill = common_name)) + 
  geom_bar(stat = "identity") +
  guides(fill = "none") +
  labs(x = "Common name", y = "Number of iNat observations") +
  theme_classic() +
  theme(axis.text.x=element_text(face="italic",angle=45, vjust=1, hjust=1), 
        plot.margin=margin(t=10,r=10,b=10,l=45))
plot3
