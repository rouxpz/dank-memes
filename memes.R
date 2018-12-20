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
  geom_histogram(binwidth = 1, color = "#1c0f4c", fill = "#5e32ff", alpha = 0.4) +
  ggtitle("Distribution of Participant Ages") +
  xlab("Age") +
  ylab("Number of participants")

#table(meme_data$subjects, meme_data$group_assignment)

combined_meme_data <- meme_data %>%
  mutate(total_polarity = preexperiment_polarization + experiment_polarization + postexperiment_polarization)

View(combined_meme_data)

partisanship_data <- combined_meme_data %>%
  mutate(ideology = ifelse(declared_partisanship == -1, "Conservative,",
                           ifelse(declared_partisanship == 0, "Neutral",
                                  ifelse(declared_partisanship == 1, "Liberal", "none")))) %>%
  group_by(week, ideology) %>%
  summarise(N = n(),
            ave_polarity = mean(total_polarity))

ggplot(partisanship_data, aes(x = ideology)) +
  geom_bar(color = "#1c0f4c", fill = "#5e32ff", alpha = 0.4) +
  ggtitle("Distribution of Participant Declared Partisanship") +
  xlab("Political Affiliation") +
  ylab("Number of participants")

ggplot(partisanship_data, aes(x = week, y = ave_polarity)) +
  geom_point()

###SINGLE DENSITY PLOT FOR ALL POLARITIES###
ggplot(combined_meme_data, aes(x = total_polarity)) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "white") +
  geom_density(color = "black", fill = "#199790", alpha = 0.4) +
  ggtitle("Distribution of Total Polarities") +
  xlab("Total Polarity") +
  ylab("Density")

###DENSITY PLOTS BY TIME###
time_groups <- combined_meme_data %>%
  mutate(time_label = ifelse(preexperiment_polarization > 0, "pre_experiment",
         ifelse(experiment_polarization > 0, "during_experiment",
         ifelse(postexperiment_polarization > 0, "post_experiment", "none"))))

View(time_groups)

time_groups$time_label <- factor(time_groups$time_label, levels = c("pre_experiment", "during_experiment", "post_experiment"), labels = c("Pre Treatment (Weeks 1-3)", "During Treatment (Weeks 4-15)", "Post Treatment (Weeks 16-19)"))

ggplot(time_groups, aes(x = total_polarity, color = time_label, fill = time_label)) +
  geom_density(alpha = 0.4) +
  ggtitle("Distribution of Total Polarities By Treatment Phase") +
  scale_color_manual("Treatment Phase", values = c("#a590f0", "#f0a590", "#90f0a5")) +
  scale_fill_manual("Treatment Phase", values = c("#a590f0", "#f0a590", "#90f0a5")) +
  xlab("Total Polarity") +
  ylab("Density") +
  labs(color = "Treatment Phase")

dank_time_group <- time_groups %>%
  filter(group_assignment == 1)

ggplot(dank_time_group, aes(x = total_polarity, color = time_label, fill = time_label)) +
  geom_density(alpha = 0.3) +
  xlim(0.0, 1.0)

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
  mutate(upper_int = ave_polarity + 1.96 * sd_polarity/sqrt(750),
         lower_int = ave_polarity - 1.96 * sd_polarity/sqrt(750))

View(grouped_averages)

ggplot(grouped_averages, aes(x = week, y = ave_polarity)) +
  #geom_bar(stat = "identity", position = "dodge")
  geom_line(aes(color = group_label)) +
  scale_color_manual("Group Assignment", values = c("#b80077", "#77b800"), labels = c('"Dank" Memes', '"Normie" Memes')) +
  geom_ribbon(aes(x = week, ymax = upper_int, ymin = lower_int, fill = group_label), alpha = 0.3) +
  scale_fill_manual("Group Assignment", values = c("#b80077", "#77b800"), labels = c('"Dank" Memes', '"Normie" Memes')) +
  ylim(0.0, 1.0) +
  geom_vline(xintercept=3, linetype = "dotted", color = "black") +
  geom_vline(xintercept=15, linetype = "dotted", color = "black") +
  ggtitle("Average Post Polarity of Participants By Group") +
  xlab("Week of Measurement") +
  ylab("Average Polarity") +
  labs(fill = "Group Assignment", caption = "The dotted lines represent the demarcation between phases of treatment. \nThe ribbon represents the 95% confidence interval for both results.")

###AGE AND POLARITY###
combined_dank_age_data <- combined_meme_data %>%
  filter(group_assignment == 1) %>%
  mutate(age_range = ifelse(age <= 21, "18-21",
                            ifelse(age <= 25, "22-25", "none")))
combined_dank_age_data <- combined_dank_age_data %>%
  group_by(age_range, week) %>%
  summarise(N = n(),
            ave_polarity = mean(total_polarity),
            sd_polarity = sd(total_polarity))

View(combined_dank_age_data)

combined_normie_age_data <- combined_meme_data %>%
  filter(group_assignment == 0) %>%
  mutate(age_range = ifelse(age <= 21, "18-21",
                            ifelse(age <= 25, "22-25", "none")))
combined_normie_age_data <- combined_normie_age_data %>%
  group_by(age_range, week) %>%
  summarise(N = n(),
            ave_polarity = mean(total_polarity),
            sd_polarity = sd(total_polarity))

combined_dank_age_data <- combined_dank_age_data %>%
  mutate(upper_int = ave_polarity + 1.96 * sd_polarity/sqrt(750),
         lower_int = ave_polarity - 1.96 * sd_polarity/sqrt(750))

combined_normie_age_data <- combined_normie_age_data %>%
  mutate(upper_int = ave_polarity + 1.96 * sd_polarity/sqrt(750),
         lower_int = ave_polarity - 1.96 * sd_polarity/sqrt(750))

combined_dank_age_data$age <- as.factor(combined_dank_age_data$age)
combined_normie_age_data$age <- as.factor(combined_normie_age_data$age)

ggplot() +
  geom_line(data = combined_dank_age_data, aes(x = week, y = ave_polarity, color = age_range)) +
  geom_ribbon(data = combined_dank_age_data, aes(x = week, ymax = upper_int, ymin = lower_int, fill = age_range), alpha = 0.2) +
  geom_line(data = combined_normie_age_data, aes(x = week, y = ave_polarity, color = age_range), linetype = "dotdash") +
  geom_ribbon(data = combined_normie_age_data, aes(x = week, ymax = upper_int, ymin = lower_int, fill = age_range), alpha = 0.2) +
  ylim(0.0, 1.0) +
  labs(title = "Average Post Polarity by Age Range", x = "Week", y = "Average Polarity", caption = 'Solid line indicates membership in the "dank" group. \nDashed line indicates membership in the "normie" group.\nRibbons indicate 95% confidence intervals.') +
  scale_color_manual("Age Range", values = c("#5cb253", "#535cb2")) +
  scale_fill_manual("Age Range", values = c("#5cb253", "#535cb2"))

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
            ave_polarity = mean(total_polarity),
            sd_polarity = sd(total_polarity))
View(partisanship_dank_data)

partisanship_dank_data <- partisanship_dank_data %>%
  mutate(upper_int = ave_polarity + 1.96 * sd_polarity/sqrt(750),
         lower_int = ave_polarity - 1.96 * sd_polarity/sqrt(750))

partisanship_dank_data$declared_partisanship <- as.factor(partisanship_dank_data$declared_partisanship)

ggplot(partisanship_dank_data, aes(x = week, y = ave_polarity, group = declared_partisanship, color = declared_partisanship)) +
  geom_line() +
  scale_color_discrete() +
  ylim(0.0, 1.0)

partisanship_normie_data <- combined_meme_data %>%
  filter(group_assignment == 0) %>%
  group_by(week, declared_partisanship) %>%
  summarise(N = n(),
            ave_polarity = mean(total_polarity),
            sd_polarity = sd(total_polarity))

partisanship_normie_data <- partisanship_normie_data %>%
  mutate(upper_int = ave_polarity + 1.96 * sd_polarity/sqrt(750),
         lower_int = ave_polarity - 1.96 * sd_polarity/sqrt(750))

partisanship_normie_data$declared_partisanship <- as.factor(partisanship_normie_data$declared_partisanship)

ggplot(partisanship_normie_data, aes(x = week, y = ave_polarity, group = declared_partisanship, color = declared_partisanship)) +
  geom_point() +
  #geom_smooth(span = 0.25) +
  scale_color_discrete() +
  ylim(0.0, 1.0)

ggplot() +
  geom_line(data = partisanship_dank_data, aes(x = week, y = ave_polarity, color = declared_partisanship)) +
  geom_ribbon(data = partisanship_dank_data, aes(x = week, ymax = upper_int, ymin = lower_int, fill = declared_partisanship), alpha = 0.2) +
  geom_line(data = partisanship_normie_data, aes(x = week, y = ave_polarity, color = declared_partisanship), linetype="dotdash") +
  geom_ribbon(data = partisanship_normie_data, aes(x = week, ymax = upper_int, ymin = lower_int, fill = declared_partisanship), alpha = 0.2) +
  ylim(0.0, 1.0) +
  scale_color_manual("Political Affiliation", values = c("#b4464b", "#82b446", "#4682b4"), labels = c('Conservative', 'Neutral', 'Liberal')) +
  scale_fill_manual("Political Affiliation", values = c("#b4464b", "#82b446", "#4682b4"), labels = c('Conservative', 'Neutral', 'Liberal')) +
  labs(title = "Average Post Polarity by Declared Partisanship", x = "Week", y = "Average Polarity", caption = 'Solid line indicates membership in the "dank" group. \nDashed line indicates membership in the "normie" group.\nRibbons indicate 95% confidence intervals.' )

###MEMES SEEN AND POLARITY SCATTERPLOT###
memes_seen_data <- combined_meme_data %>%
  mutate(memes_seen = memes_seen * 100) %>%
  filter(memes_seen > 0) %>%
  group_by(subjects, group_assignment) %>%
  filter(group_assignment == 1) %>%
  summarise(N = n(), ave_memes_seen = mean(memes_seen), ave_polarity = mean(total_polarity))
  #filter(subjects < 50)
  #filter(week == 4) %>%

View(memes_seen_data)

ggplot(memes_seen_data, aes(x = ave_memes_seen, y = ave_polarity)) +
  geom_point(color = "#467ae1", alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "#e15f46") +
  xlim(50, 100) +
  ylim(0.5, 1.0) +
  labs(title = 'Average Post Polarity Per Average Memes Seen: "Dank" Group') +
  xlab("Average Percentage of Memes Seen") +
  ylab("Average Post Polarity")
