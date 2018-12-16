library(tidyverse)
library(cowplot)

meme_data <- read.csv("meme_experiment_data.csv")
summary(meme_data)
View(meme_data)

#table(meme_data$subjects, meme_data$group_assignment)

meme_data[is.na(meme_data)] <- 0
combined_meme_data <- meme_data %>%
  mutate(total_polarity = preexperiment_polarization + experiment_polarization + postexperiment_polarization)

View(combined_meme_data)

combined_averages <- combined_meme_data %>%
  group_by(week) %>%
  summarise(N = n(),
            ave_polarity = mean(total_polarity), na.rm = TRUE)

#ggplot(combined_averages, aes(x = week, y = ave_polarity)) +
#  geom_bar(stat = "identity", position = "dodge")

###LOOKING AT EACH GROUP SEPARATELY###
dank_group <- combined_meme_data %>%
  filter(group_assignment == 1)
View(dank_group)

dank_group_averages <- dank_group %>%
  group_by(week) %>%
  summarise(N = n(),
            ave_polarity = mean(total_polarity), na.rm = TRUE)

View(dank_group_averages)

ggplot(dank_group_averages, aes(x = week, y = ave_polarity)) +
  geom_bar(stat = "identity", position = "dodge")

normie_group <- combined_meme_data %>%
  filter(group_assignment == 0)

normie_group_averages <- normie_group %>%
  group_by(week) %>%
  summarise(N = n(),
            ave_polarity = mean(total_polarity), 
            sd_polarity = sd(total_polarity),
            na.rm = TRUE)

ggplot(normie_group_averages, aes(x = week, y = ave_polarity)) +
  geom_bar(stat = "identity", position = "dodge")

###THIS IS WHERE EVERYTHING GETS SEPARATED AND PLACED INTO A SINGLE CHART###
grouped_averages <- combined_meme_data %>%
  mutate(group_label = ifelse(group_assignment > 0, "dank", "normie"))

grouped_averages <- grouped_averages %>%
  group_by(group_label, week) %>%
  summarise(N = n(),
            ave_polarity = mean(total_polarity))

View(grouped_averages)

ggplot(grouped_averages, aes(x = week, y = ave_polarity, fill = group_label)) +
  geom_bar(stat = "identity", position = "dodge")
  