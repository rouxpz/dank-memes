library(tidyverse)
library(cowplot)

meme_data <- read.csv("meme_experiment_data.csv")
meme_data[is.na(meme_data)] <- 0

summary(meme_data)
View(meme_data)

ggplot(meme_data, aes(x = preexperiment_polarization)) +
  geom_density()

#table(meme_data$subjects, meme_data$group_assignment)

combined_meme_data <- meme_data %>%
  mutate(total_polarity = preexperiment_polarization + experiment_polarization + postexperiment_polarization)

View(combined_meme_data)

###DENSITY PLOTS BY TIME###
time_groups <- combined_meme_data %>%
  mutate(time_label = ifelse(preexperiment_polarization > 0, "pre_experiment",
         ifelse(experiment_polarization > 0, "during_experiment",
         ifelse(postexperiment_polarization > 0, "post_experiment", "none"))))

View(time_groups)

ggplot(time_groups, aes(x = total_polarity, color = time_label, fill = time_label)) +
  geom_density(alpha = 0.3)

###COMBINING AVERAGES###
combined_averages <- combined_meme_data %>%
  group_by(week, group_assignment) %>%
  summarise(N = n(),
            ave_polarity = mean(total_polarity), na.rm = TRUE)

View(combined_averages)


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
grouped_data <- combined_meme_data %>%
  mutate(group_label = ifelse(group_assignment > 0, "dank", "normie"))

grouped_averages <- grouped_data %>%
  group_by(group_label, week) %>%
  summarise(N = n(),
            ave_polarity = mean(total_polarity))

View(grouped_averages)

ggplot(grouped_averages, aes(x = week, y = ave_polarity, fill = group_label)) +
  geom_bar(stat = "identity", position = "dodge")

  