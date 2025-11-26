############################################################
# Load Libraries
############################################################
library(dplyr)
library(janitor)
library(tidyr)
library(pls)
library(glmnet)
library(ggplot2)
library(knitr)
library(gridExtra)
library(broom)
library(kableExtra)


############################################################
# Import and Clean Data
############################################################
df_nutrition <- read.csv("772713vg_health_nutrition_population.csv") %>% clean_names()
df_life_expectancy <- read.csv("life_expectancy_data.csv") %>% clean_names() %>% select(-x)
df_prediction <- read.csv("predictions.csv") %>% clean_names() %>% select(-x)

############################################################
# Merge Data for Model Training
############################################################
df_model <- df_nutrition %>% left_join(df_life_expectancy, by = "country")

# Check merge success (should return zero rows)
df_model %>% filter(is.na(life_expectancy_at_birth_total_years)) %>% select(country)

df_pred <- df_prediction   # stored for later predictions
