library(dplyr)
library(expss)

data <- read.csv("~/Downloads/meme_experiment_data.csv")

# recode vars to better stuff
data$current_polarization <- case_when(
  data$week %in% c(1:3) ~ data$preexperiment_polarization,
  data$week %in% c(4:15) ~ data$experiment_polarization,
  data$week %in% c(16:19) ~ data$postexperiment_polarization
)
data$group_assignment <- factor(data$group_assignment, 
                                labels = c("Control", "Experimental"))
labels(data, which = c("group_assignment") <- c("Group"))
data = apply_labels(data, group_assignment = "Group")


control_means <- data[data$group_assignment == "Control",] %>% 
  group_by(week) %>%
  dplyr::summarise(N = n(),
                   week_mean = mean(current_polarization),
                   week_sd = sd(current_polarization),
                   week_ci = 1.96 * week_sd/sqrt(N))

experiment_means <- data[data$group_assignment == "Experimental",] %>% 
  group_by(week) %>%
  dplyr::summarise(N = n(),
                   week_mean = mean(current_polarization),
                   week_sd = sd(current_polarization),
                   week_ci = 1.96 * week_sd/sqrt(N))

c_e_means <- data.frame(rbind(control_means, experiment_means))
c_e_means$control <- c(rep("Control", 19), rep("Experimental", 19))

# 
# Construct plots ----------
#
plot1 <- ggplot(during, aes(x=week, y=current_polarization)) +
  # add confidence intervals
  geom_ribbon(data = c_e_means, aes(x = week, 
                                    ymin = week_mean - week_ci, 
                                    ymax = week_mean + week_ci,
                                    fill = c_e_means$control), 
                     inherit.aes = FALSE, alpha = 0.5) +
  # add lines
  geom_line(data = control_means, aes(x = week, y = week_mean)) +
  geom_line(data = experiment_means, aes(x = week, y = week_mean)) +
  # add marker lines
  geom_vline(xintercept=3, linetype = "dashed") +
  geom_text(data=data.frame(x=3.5,y=0.25), aes(x, y), hjust = 0,
            label="Start of Experiment") +
  geom_vline(xintercept=15, linetype = "dashed") +
  geom_text(data=data.frame(x=14.5,y=0.55), aes(x, y), hjust = 1,
            label="End of Experiment") +
  # add title
  ggtitle("Average Weekly Polarization") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Week", y = "Polarization of Facebook Posts (0-1)",
       caption = "Pre-experiment data collected during weeks 1-3
       The experiment ran during weeks 4-15
       Post-experiment data was collected during weeks 16-19") +
# rework legend
  scale_fill_manual("Group", values=c("#F8766D", "#00BFC4"))

# examine the polarization of posts from subjects pre-experiment
pre_experiment <- data.frame(data[data$week %in% c(1,2,3),])
during_experiment <- data.frame(data[data$week %in% range(4:15),])
post_experiment <- data.frame(data[data$week %in% range(16:19),])
pre_experiment$Group <- pre_experiment$group_assignment
during_experiment$Group <- during_experiment$group_assignment
post_experiment$Group <- post_experiment$group_assignment


pre_means <- ddply(pre_experiment, "Group", 
                   summarise, grp.mean=mean(current_polarization))
during_means <- ddply(during_experiment, "Group", 
                      summarise, grp.mean=mean(current_polarization))
post_means <- ddply(post_experiment, "Group",
                    summarise, grp.mean=mean(current_polarization))

# pre-experiment density
plot3 <- ggplot(pre_experiment, aes(x=current_polarization, 
                                    fill = Group)) + 
  geom_density(alpha = 0.5) + xlim(c(0, 1)) +
  # add mean lines
  geom_vline(data = pre_means, aes(xintercept=grp.mean, 
                                   color = Group), 
             linetype="dashed", size=1) +
  ggtitle("Distribution of Polarization Pre-Experiment") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Polarization of Facebook Posts (0-1)", 
       y = "Density") +
  # use consistent colors with other plots
  scale_fill_manual(values=c("#F8766D", "#00BFC4"))


# during experiment density
plot4 <- ggplot(during_experiment, aes(x=current_polarization, 
                                    fill = Group)) + 
  geom_density(alpha = 0.5) + xlim(c(0, 1)) +
  # add mean lines
  geom_vline(data = during_means, aes(xintercept=grp.mean, 
                                   color = Group), 
             linetype="dashed", size=1) +
  ggtitle("Distribution of Polarization During Experiment") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Polarization of Facebook Posts (0-1)", 
       y = "Density") +
  # use consistent colors with other plots
  scale_fill_manual(values=c("#F8766D", "#00BFC4"))


# post experiment density
plot5 <- ggplot(post_experiment, aes(x=current_polarization, 
                                    fill = Group)) + 
  geom_density(alpha = 0.5) + xlim(c(0, 1)) +
  # add mean lines
  geom_vline(data = post_means, aes(xintercept=grp.mean, 
                                   color = Group), 
             linetype="dashed", size=1) +
  ggtitle("Distribution of Polarization Post-Experiment") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Polarization of Facebook Posts (0-1)", 
       y = "Density") +
  # use consistent colors with other plots
  scale_fill_manual(values=c("#F8766D", "#00BFC4"))



### General tests -------

model <- lm(current_polarization ~ memes_seen, 
            data = during_experiment[during_experiment$group_assignment == 1,])

model <- lm(current_polarization ~ memes_seen, 
            data = during_experiment[during_experiment$group_assignment == 0,])


summary(model)
during_experiment$group_assignment

colnames(during_experiment)