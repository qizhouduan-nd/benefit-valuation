## analysis script
### data restructure by lyncie
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

View(data_restructure)
dim(data_restructure)
## Q2 (Stranger), Q3 (Friends)
unique(data_restructure$condition)


