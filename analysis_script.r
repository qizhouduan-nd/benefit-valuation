## analysis script
### data restructure by lyncie _ Updated 10/5/2023
setwd(dirname(rstudioapi::documentPath()))
library(tidyverse)
library(dplyr)
library(rstatix)

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

# Turning into Long format
Data_long <- data_restructure %>%
  gather(key="Type",value="Score", grateful, obligated, want) %>%
  convert_as_factor(condition,Type)

Data_long$Type <- factor(Data_long$Type,
                        levels = c("grateful", "obligated", "want"))

Data_long_averaged <- Data_long %>%
  group_by(subject, condition, Type) %>%
  summarise(score = mean(Score, na.rm = TRUE)) %>%
  ungroup()

# 2X3 ANOVA on Grateful
res.aov <- anova_test(data = Data_long_averaged, dv = score, wid = subject,
                      between  = c(condition,Type), effect.size = "pes")
get_anova_table(res.aov)
boxplot(score ~ condition, data = Data_long_averaged, main = "ANOVA Boxplot", ylab = "Response", xlab = "Group")

aov_res <- aov(score ~ condition*Type + Error(subject/(condition*Type)), data = Data_long_averaged)

library(emmeans)

# Calculate the estimated marginal means
emm <- emmeans(aov_res, specs = pairwise ~ condition * Type)

# Conduct pairwise comparisons for 'type'
pairwise_comp <- pairs(emm, adjust = "bonferroni")
print(pairwise_comp)

## multi-level model here
library(lme4)
library(tidyr)
library(ggplot2)
library(reshape2)
library(tidyverse)
library(haven)
library(aod)
library(WeMix)
library(psych)
library(lavaan)
library(lmtest)

model = lmer(Score ~ Type + (1 | subject),data = Data_long)
model


## age range
friend_data = data_restructure[data_restructure$condition == 'Q2',]
stranger_data = data_restructure[data_restructure$condition == 'Q3',]

hist(friend_data$grateful)
hist(stranger_data$grateful)


