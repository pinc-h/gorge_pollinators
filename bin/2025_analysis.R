library(tidyverse)
library(scales)  # for hue_pal()
library(lubridate)
library(viridis)
library(patchwork)
library(stringr)


setwd("/Users/pinch/Desktop/gorge_pollinators/lib/data")
data <- read.csv ("cleaned_pollinator_data_2025.csv")

data$Date <- format(as.Date(data$Date, format = "%m/%d/%Y"), "%d-%b-%y")
data$Genus <- word(data$Latin.Name, 1)
genera_colours <- setNames(scales::hue_pal()(length(unique(data$Genus))),
                           unique(data$Genus))

plot1 <- data %>%
  group_by(Latin.Name) %>%
  filter(Latin.Name != "") %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  mutate(Latin.Name = factor(Latin.Name, levels = Latin.Name)) %>% # need this for descending order, idk what it does
  ggplot(aes(x = Latin.Name, y = Count, fill = Latin.Name)) +
  geom_bar(stat = "identity", color = "black") +  # Add black outline
  labs(x = "Latin Name", y = "Total Specimens Caught") +
  guides(fill = "none") +
  theme_classic() +
  theme(axis.text.x=element_text(face="italic",angle=45, vjust=1, hjust=1), 
        plot.margin=margin(t=10,r=10,b=10,l=45))
plot1

# Total specimens caught by zone
plot2 <- data %>%
  filter(Location == "SM") %>% # Changed this to PM or SM for different sites
  group_by(Genus) %>%
  filter(Genus != "") %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  mutate(Genus = factor(Genus, levels = Genus)) %>% # need this for descending order, idk what it does
  ggplot(aes(x = Genus, y = Count, fill = Genus)) +
  geom_bar(stat = "identity", color = "black") +  # Add black outline
  labs(x = "Genus", y = "Total Specimens Caught", title = "Salt Marsh Site") + # Changed title to reflect which site
  guides(fill = "none") +
  theme_classic() +
  theme(axis.text.x=element_text(face="italic",angle=45, vjust=1, hjust=1), 
        plot.margin=margin(t=10,r=10,b=10,l=45))
plot2

plot3 <- data %>%
  group_by(Genus, Latin.Name..Plant.) %>%
  filter(Genus != "", Location == "C") %>%
  summarise(Count = n()) %>%
  ggplot(aes(x = Latin.Name..Plant., y = Count, fill = Genus)) +
  geom_bar(stat = "identity", color = "black") +
  labs(x = "Plant Species", y="Total Specimens Caught", fill="Genus") +
  theme_classic() +
  theme(axis.text.x=element_text(face="italic",angle=45, vjust=1, hjust=1), 
        plot.margin=margin(t=10,r=10,b=10,l=45)) +
  scale_fill_manual(values = genera_colours)
plot3


plot4 <- data %>%
  group_by(Genus, Latin.Name..Plant.) %>%
  filter(Genus == "Dianthidium") %>%
  summarise(Count = n()) %>%
  ggplot(aes(x = Latin.Name..Plant., y = Count)) +
  geom_bar(stat = "identity", color = "black") +
  labs(x = "Plant Species", y="Total Specimens Caught") +
  theme_classic() +
  theme(axis.text.x=element_text(face="italic",angle=45, vjust=1, hjust=1), 
        plot.margin=margin(t=10,r=10,b=10,l=45)) +
  scale_fill_manual(values = genera_colours)
plot4

