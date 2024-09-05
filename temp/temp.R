# Attempt 1 at rarefaction
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