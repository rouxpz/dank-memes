library(tidyverse)

meme_data<- read_csv("~/Desktop/Methods Final/meme_experiment_data.csv")

#Create all_memes data set 
all_memes <- meme_data %>%
  group_by(group_assignment, week)

#remove N/As and replace with 0s
all_memes[is.na(all_memes)] <-0

#Create a total polarization column (sum of all polarizations) 
all_memes <- all_memes %>%
  mutate(total_polarization = preexperiment_polarization+ experiment_polarization+ postexperiment_polarization)

#Plot the Groups against each by week 
mean_memes <- all_memes %>%
  group_by(group_assignment, week) %>% 
  summarize(N=n(),
            ave_total_polarization= mean(total_polarization),
            sd =sd(total_polarization),
            ci=1.96*sd/sqrt(N))

ggplot(mean_memes, aes(x=week, y=ave_total_polarization, fill=as.factor(group_assignment), group=group_assignment))+
  geom_line(aes(color=as.factor(group_assignment)))+
  geom_ribbon(aes(ymin =ave_total_polarization-ci, 
                  ymax = ave_total_polarization+ci),
              alpha=.4)+
  scale_fill_manual("Group Assignment", values=c("Blue", "Green"),
                     labels=c("Normie", "Dank"))+
  scale_color_manual(values=c("Blue", "Green"), guide=FALSE)+
  ylab("Average Total Polarization")+
  xlab("Week")+
  theme_minimal()+
  labs(title="Average Polarization of Participants Facebook Posts", 
       caption="Pre-experiment data collection occured from week 1 to week 3.\n The experiment ran from week 4 to week 15.\n Post-experiment data was collected weeks 16 through 19.")
 

#Differences in Age 

age_grps <- c("18-21", "22-25")

age <- all_memes 
age$age_groups <- cut(age$age, breaks=c("18", "22", "26"), labels=age_grps, right=FALSE)

summary(age$age)

age <- age %>%
  group_by(group_assignment, age_groups, week) %>%
  summarize(N=n(), 
            ave_total_polarization = mean(total_polarization), 
            sd = sd(total_polarization), 
            ci=1.96*sd/sqrt(N))

ggplot(age, aes(x=week, y=ave_total_polarization, group=group_assignment))+
  geom_point(aes(color=age_groups))+
  geom_smooth(method = "auto", se = TRUE, color = "orange", span=1)+
  ylab("Average Total Polarization")+
  xlab("Week")+
  theme_minimal()+
  labs(title="Average Polarization of Participants Facebook Posts by age", 
       caption="Pre-experiment data collection occured from week 1 to week 3.\n The experiment ran from week 4 to week 15.\n Post-experiment data was collected weeks 16 through 19.",
       color="Age Group")

#Differences in declared partisanship 

summary(all_memes$declared_partisanship)

partisan <- all_memes 
partisan$p_level <- abs(partisan$declared_partisanship)

summary(partisan$p_level)
table(partisan$p_level)

partisan <- partisan %>%
  group_by(group_assignment, p_level, week) %>%
  summarize(N=n(), 
            ave_total_polarization = mean(total_polarization), 
            sd = sd(total_polarization), 
            ci=1.96*sd/sqrt(N))

ggplot(partisan, aes(x=week, y=ave_total_polarization, group=group_assignment))+
  geom_point(aes(color=p_level))+
  geom_smooth(method = "auto", se = TRUE, color = "orange", span=1)+
  ylab("Average Total Polarization")+
  xlab("Week")+
  theme_minimal()+
  labs(title="Average Polarization of Participants Facebook Posts by Declared Partisanship", 
       caption="Pre-experiment data collection occured from week 1 to week 3.\n The experiment ran from week 4 to week 15.\n Post-experiment data was collected weeks 16 through 19.",
       color="Declared Partisanship")

#playing around 
ggplot(all_memes, aes(x=week, y=total_polarization, group=subjects, color=as.factor(group_assignment)))+
  geom_line(alpha=.2)+
  scale_color_manual("Group Assignment", values=c("Blue", "Pink"),
                    labels=c("Normie", "Dank"))




