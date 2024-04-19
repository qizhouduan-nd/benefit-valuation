## analysis script
### data restructure by lyncie _ Updated 4/18/2024
setwd("/Users/lynciexu/Desktop/projects/gratitude")
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

# mean score averaged across scenarios
Data_long_averaged <- Data_long %>%
  group_by(subject, condition, Type) %>%
  summarise(score = mean(Score, na.rm = TRUE)) %>%
  ungroup()

# mean score averaged across subject, with quantified monetary value for each scenario
Data_long_across_subject <- Data_long %>%
  group_by(condition, scenario, Type) %>%
  summarise(AverageScore = mean(Score, na.rm = TRUE), .groups = 'drop')
scenario <- paste0("S", 1:21)  # Creates labels S1, S2, ..., S21
df <- data.frame(Score = scale_map, scenario = scenario)

merged_df <- merge(df, Data_long_across_subject, by = "scenario", all.x = TRUE)

data_matrix <- matrix(scale_map)
colnames(data_matrix) <- paste0("S", 1:21)
data_df <- as.data.frame(data_matrix)

# 2X3 ANOVA on Grateful
res.aov <- anova_test(data = Data_long_averaged, dv = score, wid = subject,
                      between  = c(condition,Type), effect.size = "pes")
get_anova_table(res.aov)

aov_res <- aov(score ~ condition*Type + Error(subject/(condition*Type)), data = Data_long_averaged)

library(emmeans)

# Calculate the estimated marginal means
emm <- emmeans(aov_res, specs = pairwise ~ condition * Type)

# Simple effects of condition at each level of Type
simple_effects_condition <- emmeans(aov_res, pairwise ~ condition | Type, adjust = "bonferroni")
print(simple_effects_condition)

# Simple effects of Type at each level of condition
simple_effects_type <- emmeans(aov_res, pairwise ~ Type | condition, adjust = "bonferroni")
print(simple_effects_type)

# For the main effect of condition:
emm_condition <- emmeans(aov_res, "condition")
pairs_condition <- pairs(emm_condition, adjust = "bonferroni") # adjust for multiple comparisons if needed
print(pairs_condition)

# Simple effects of Type at each level of condition
simple_effects_type <- emmeans(aov_res, pairwise ~ Type | condition, adjust = "bonferroni")
print(simple_effects_type)

# For the main effect of type:
emm_type <- emmeans(aov_res, "Type")
pairs_type <- pairs(emm_type, adjust = "bonferroni") # adjust for multiple comparisons if needed
print(pairs_type)

# Conduct pairwise comparisons for 'type'
pairwise_comp <- pairs(emm, adjust = "bonferroni")
print(pairwise_comp)

library(dplyr)
library(purrr)

# Assuming merged_df is your merged data frame with 'Score' and 'AverageScore' columns

# Remove rows where scenario is S1
df_filtered <- merged_df[merged_df$scenario != "S1", ]

# Define a function to perform cor.test and return a tibble with the results
perform_cor_test <- function(df) {
  test_result <- cor.test(df$Score, df$AverageScore, method = "pearson")
  return(tibble(
    correlation = test_result$estimate,
    p_value = test_result$p.value,
    sample_size = length(df$Score)
  ))
}

# Group by 'condition' and 'Type', then calculate correlations and p-values within each group
correlation_tests <- df_filtered %>%
  group_by(condition, Type) %>%
  # Use nest() to create a nested data frame for each group
  nest() %>%
  # Use map() to apply the perform_cor_test function to each nested data frame
  mutate(
    cor_test_results = map(data, perform_cor_test)
  ) %>%
  # Unnest the correlation test results
  select(-data) %>%
  unnest(cor_test_results)

# View the correlation tests results
print(correlation_tests)

