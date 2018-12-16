library(tidyverse)
library(cowplot)

meme_data <- read.csv("meme_experiment_data.csv")
summary(meme_data)
View(meme_data)

table(meme_data$subjects, meme_data$group_assignment)
  