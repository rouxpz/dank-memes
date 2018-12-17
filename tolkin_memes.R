library(dplyr)

data <- read.csv("~/Downloads/meme_experiment_data.csv")

colnames(data)

pre_polar_dens <- density(data$preexperiment_polarization, na.rm = TRUE)
plot(polar_dens)

exp_polar_dens <- density(data$preexperiment_polarization, na.rm = TRUE)

You do over time dank vs not dank with confidence intervals.
two groups, confidence intervals over time

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
                   ci = 1.96 * week_sd/sqrt(N))

experiment_means <- data[data$group_assignment == 1,] %>% 
  group_by(week) %>%
  dplyr::summarise(N = n(),
                   week_mean = mean(current_polarization),
                   week_sd = sd(current_polarization),
                   ci = 1.96 * week_sd/sqrt(N))


p <- ggplot(during, aes(x=week, y=current_polarization))
p <- p + geom_ribbon(data = control_means, aes(x = week, ymin = week_mean - week_sd, 
                                               ymax = week_mean + week_sd), 
                     inherit.aes = FALSE, alpha = 0.5, fill = "blue") +
  geom_ribbon(data = experiment_means, aes(x = week, ymin = week_mean - week_sd,
                                           ymax = week_mean + week_sd), 
              inherit.aes = FALSE, alpha = 0.5, fill = "red")

# add lines
p <- p + geom_line(data = control_means, aes(x = week, y = week_mean))
p <- p + geom_line(data = experiment_means, aes(x = week, y = week_mean))

# add marker lines
p <- p + geom_vline(xintercept=3, linetype = "dashed") +
  geom_text(data=data.frame(x=3.5,y=0.25), aes(x, y), hjust = 0,
            label="Start of Experiment") +
  geom_vline(xintercept=15, linetype = "dashed") +
  geom_text(data=data.frame(x=14.5,y=0.55), aes(x, y), hjust = 1,
            label="End of Experiment")

# add title + axis titles
p <- p + ggtitle("Average Weekly Polarization") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Week", y = "Polarization")

# add legend
p <- p + 
  scale_color_manual(name="", values = c("red","red")) +
  scale_fill_manual(name="", values=c("white","red"))


p




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




colnames(polarization)$data.group_assignment) 

polarization$mean_polarized <- 
  summarise(polarization$data.experiment_polarization, stats = "mean")

polarization <- polarization %>% mutate(mean_polar = mean(data.experiment_polarization)) %>%
  group_by(week)

polarization$mean_polar

polarization$data.experiment_polarization


pol <- ggplot(polarization, aes(x=data$week, y=mean(data$experiment_polarization)))
pol + geom_line()
pol + geom_ribbon((ymin=experiment_polarization - .1, ymax=experiment_polarization + .1 ))