library(tidyverse)
library(cowplot)
library(reshape2)

meme_data <- read.csv("meme_experiment_data.csv")
meme_data[is.na(meme_data)] <- 0

summary(meme_data)
View(meme_data)

###HISTOGRAM FOR AGES###
age_data <- meme_data %>%
  group_by(subjects, age) %>%
  summarise(N = n())

View(age_data)

ggplot(age_data, aes(x = age)) +
  geom_histogram(binwidth = 1, color = "#1c0f4c", fill = "#5e32ff", alpha = 0.4)

#table(meme_data$subjects, meme_data$group_assignment)

combined_meme_data <- meme_data %>%
  mutate(total_polarity = preexperiment_polarization + experiment_polarization + postexperiment_polarization)

View(combined_meme_data)

###SINGLE DENSITY PLOT FOR ALL POLARITIES###
ggplot(combined_meme_data, aes(x = total_polarity)) +
  #geom_density(color = "#199790", fill = "#199790", alpha = 0.4)
  geom_histogram(color = "#199790", fill = "#199790", alpha = 0.4) +
  stat_function(fun = dnorm, color = "red", arg = list(mean = mean(combined_meme_data$total_polarity, na.rm = TRUE), sd = sd(combined_meme_data$total_polarity, na.rm = TRUE)))

###DENSITY PLOTS BY TIME###
time_groups <- combined_meme_data %>%
  mutate(time_label = ifelse(preexperiment_polarization > 0, "pre_experiment",
         ifelse(experiment_polarization > 0, "during_experiment",
         ifelse(postexperiment_polarization > 0, "post_experiment", "none"))))

View(time_groups)

ggplot(time_groups, aes(x = total_polarity, color = time_label, fill = time_label)) +
  geom_density(alpha = 0.3)

dank_time_group <- time_groups %>%
  filter(group_assignment == 1)

ggplot(dank_time_group, aes(x = total_polarity, color = time_label, fill = time_label)) +
  geom_density(alpha = 0.3)

normie_time_group <- time_groups %>%
  filter(group_assignment == 0)

ggplot(normie_time_group, aes(x = total_polarity, color = time_label, fill = time_label)) +
  geom_density(alpha = 0.3) +
  xlim(0.0, 1.0)

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

###THIS IS WHERE EVERYTHING GETS PLACED INTO A SINGLE CHART###
grouped_data <- combined_meme_data %>%
  mutate(group_label = ifelse(group_assignment > 0, "dank", "normie"))

grouped_averages <- grouped_data %>%
  group_by(group_label, week) %>%
  summarise(N = n(),
            ave_polarity = mean(total_polarity),
            sd_polarity = sd(total_polarity))

grouped_averages <- grouped_averages %>%
  mutate(upper_int = ave_polarity + 1.282 * sd_polarity,
         lower_int = ave_polarity - 1.282 * sd_polarity)

View(grouped_averages)

ggplot(grouped_averages, aes(x = week, y = ave_polarity, fill = group_label)) +
  #geom_bar(stat = "identity", position = "dodge")
  geom_line() +
  geom_ribbon(aes(x = week, ymax = upper_int, ymin = lower_int), alpha = 0.2)

###AGE AND POLARITY###
combined_dank_age_data <- combined_meme_data %>%
  filter(group_assignment == 1) %>%
  group_by(age, week) %>%
  summarise(N = n(),
            ave_polarity = mean(total_polarity))

combined_normie_age_data <- combined_meme_data %>%
  filter(group_assignment == 0) %>%
  group_by(age, week) %>%
  summarise(N = n(),
            ave_polarity = mean(total_polarity))

combined_dank_age_data$age <- as.factor(combined_dank_age_data$age)
combined_normie_age_data$age <- as.factor(combined_normie_age_data$age)

View(combined_dank_age_data)
ggplot(combined_dank_age_data, aes(x = week, y = ave_polarity, group = age, color = age)) +
  geom_line() +
  scale_color_discrete() +
  ylim(0.0, 1.0)

ggplot(combined_normie_age_data, aes(x = week, y = ave_polarity, group = age, color = age)) +
  geom_line() +
  scale_color_discrete() +
  ylim(0.0, 1.0)

##DECLARED PARTISANSHIP AND POLARITY##
partisanship_dank_data <- combined_meme_data %>%
  filter(group_assignment == 1) %>%
  group_by(week, declared_partisanship) %>%
  summarise(N = n(),
            ave_polarity = mean(total_polarity))

partisanship_dank_data$declared_partisanship <- as.factor(partisanship_dank_data$declared_partisanship)

ggplot(partisanship_dank_data, aes(x = week, y = ave_polarity, group = declared_partisanship, color = declared_partisanship)) +
  geom_line() +
  scale_color_discrete() +
  ylim(0.0, 1.0)

partisanship_normie_data <- combined_meme_data %>%
  filter(group_assignment == 0) %>%
  group_by(week, declared_partisanship) %>%
  summarise(N = n(),
            ave_polarity = mean(total_polarity))

partisanship_normie_data$declared_partisanship <- as.factor(partisanship_normie_data$declared_partisanship)

ggplot(partisanship_normie_data, aes(x = week, y = ave_polarity, group = declared_partisanship, color = declared_partisanship)) +
  geom_line() +
  scale_color_discrete() +
  ylim(0.0, 1.0)
