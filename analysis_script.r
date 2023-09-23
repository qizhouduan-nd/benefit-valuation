## analysis script
setwd(dirname(rstudioapi::documentPath()))
library(tidyverse)


data <- read.csv("gratitude.csv", header = T)
data <-  data[,-c(62,63)]

q_num <- 1:3
q_val <- c("grateful", "want", "obligated")
q_map <- setNames(q_val, q_num)


data_restructure <- data %>% 
  # make data long format with respect to scenario/question combination
  gather(scenario, value, Q2.2_1:Q3.22_3) %>% 
  # separate the scenarios from their emotion questions
  separate(scenario, c("scenario", "question"), sep = "_") %>% 
  # replace question number with emotion content
  mutate(question = q_map[question]) %>% 
  separate(scenario,c("condition","scenario",sep="."))%>%
  # make emotion question wide format while retaining long format of scenario
  spread(question, value)

data_restructure <- data_restructure[,-c(2,5)]
data_restructure$scenario <- paste("S", data_restructure$scenario, sep="")

study1_scaling <- read.csv("grat_wtr_study1_scaling.csv", header = T)
study1_scaling <- study1_scaling[-(1:2),]
study1_scaling[,paste("S",1:21,sep="")] <- sapply(study1_scaling[,paste("S",1:21,sep="")], as.character)
study1_scaling[,paste("S",1:21,sep="")] <- sapply(study1_scaling[,paste("S",1:21,sep="")], as.numeric)
scale_map <- colMeans(study1_scaling[,paste("S",1:21,sep="")], na.rm = T)

data_restructure$scenario_val <- scale_map[data_restructure$scenario]
data_restructure$log_scenario_val <- log(data_restructure$scenario_val, base = 10)

## working up until this point


study1_scaling[,paste("",1:21,sep="")] <- sapply(study1_scaling[,paste("",1:21,sep="")], as.character)
study1_scaling[,paste("",1:21,sep="")] <- sapply(study1_scaling[,paste("",1:21,sep="")], as.numeric)

scale_map <- colMeans(study1_scaling[,1:21], na.rm = T)
study1_restructure$scenario_val <- scale_map[study1_restructure$scenario]
study1_restructure$log_scenario_val <- log(study1_restructure$scenario_val, base = 10)

#dummy code friend vs. stranger (0=friend, 1=stranger)
data_restructure[data_restructure == "Q2"] <- "0"
data_restructure[data_restructure == "Q3"] <- "1"




