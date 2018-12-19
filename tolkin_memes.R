library(dplyr)

data <- read.csv("~/Downloads/meme_experiment_data.csv")

colnames(data)

pre_polar_dens <- density(data$preexperiment_polarization, na.rm = TRUE)
plot(polar_dens)

exp_polar_dens <- density(data$preexperiment_polarization, na.rm = TRUE)

colnames(data)

# Just take the during experiment data for now
during <- data %>% filter(between(week, 4, 15))

colnames(during)
during$group_assignment

data$current_polarization <- case_when(
  data$week %in% c(1:3) ~ data$preexperiment_polarization,
  data$week %in% c(4:15) ~ data$experiment_polarization,
  data$week %in% c(16:19) ~ data$postexperiment_polarization
)

control_means <- data[data$group_assignment == 0,] %>% 
  group_by(week) %>%
  dplyr::summarise(N = n(),
                   week_mean = mean(current_polarization),
                   week_sd = sd(current_polarization),
                   week_ci = 1.96 * week_sd/sqrt(N))

experiment_means <- data[data$group_assignment == 1,] %>% 
  group_by(week) %>%
  dplyr::summarise(N = n(),
                   week_mean = mean(current_polarization),
                   week_sd = sd(current_polarization),
                   week_ci = 1.96 * week_sd/sqrt(N))

c_e_means <- data.frame(rbind(control_means, experiment_means))
c_e_means$control <- c(rep("control", 19), rep("experimental", 19))

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
  labs(x = "Week", y = "Polarization of Facebook Posts (0-1)") +
# rework legend
  scale_fill_manual("Group", values=c("royalblue", "salmon"))


# examine the polarization of posts from subjects pre-experiment
pre_experiment <- data[data$week %in% c(1,2,3),]
plot2 <- ggplot(aes(x=current_polarization), data = pre_experiment) + 
  geom_density(fill="springgreen4") + xlim(c(-0, 0.5))

plot3 <- ggplot(pre_experiment, aes(x=current_polarization, 
                                    fill = group_assignment)) + geom_density()


table(pre_experiment$group_assignment)




###################
# STORAGE - REDO WITH ALL WEEKS NOW
control_means <- during[during$group_assignment == 0,] %>% 
  group_by(week) %>%
  dplyr::summarise(N = n(),
                   week_mean = mean(experiment_polarization),
                   week_sd = sd(experiment_polarization),
                   ci = 1.96 * week_sd/sqrt(N))


experiment_means <- during[during$group_assignment == 1,] %>% 
  group_by(week) %>%
  dplyr::summarise(N = n(),
            week_mean = mean(experiment_polarization),
            week_sd = sd(experiment_polarization),
            ci = 1.96 * week_sd/sqrt(N))

# ok now we want to split experiment and control during the experiment
p <- ggplot(during, aes(x=week, y=experiment_polarization))

# add confidence intervals
p <- p + geom_ribbon(data = control_means, aes(x = week, ymin = week_mean - ci, 
                                          ymax = week_mean + ci), 
            inherit.aes = FALSE, alpha = 0.5, fill = "blue") +
  
  geom_ribbon(data = experiment_means, aes(x = week, ymin = week_mean - ci,
                                           ymax = week_mean + ci), 
              inherit.aes = FALSE, alpha = 0.5, fill = "red")

# add lines
p <- p + geom_line(data = control_means, aes(x = week, y = week_mean))
p <- p + geom_line(data = experiment_means, aes(x = week, y = week_mean))

p
# add labels to plot - ask sean tomorrow what you are doing wrong??

# you are going to want to do some funky case_when shit to get these means
# to line up properly



p + geom_line(data = control_means, aes(x = week, y = week_mean))

p + geom_ribbon(aes(ymin = week_mean-week_sd, ymax = week_mean+week_sd),
                alpha=0.3,
                fill="green") +
  geom_line(aes(y=week_mean))




# storage for working
p <- ggplot(with_means, aes(x=week, y=week_mean))
p + geom_ribbon(aes(ymin = week_mean-week_sd, ymax = week_mean+week_sd),
            alpha=0.3,
            fill="green") +
  geom_line(aes(y=week_mean))

