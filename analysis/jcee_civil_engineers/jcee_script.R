
library(tidyverse)
library(dbscan)
library(umap)
library(psych)





setwd("G:/My Drive/AK Faculty/Research/Projects/project students and climate change")

#climate_df <- read_csv("filtered_climate_survey_20200122.csv")
climate_df <- read_csv("updated_climate_df_2.csv")











Q27_vars <- paste0("Q27", letters[1:9])
Q28_vars <- paste0("Q28", letters[1:12])





### Fill in NAs for specific items ---- 

Q1_vars <- paste0("Q1", letters[1:20])
Q3_vars <- paste0("Q3", letters[1:7])
Q5_vars <- paste0("Q5", letters[1:10])
Q16_vars <- paste0("Q16", letters[1:13])
Q27_vars <- paste0("Q27", letters[1:9])
Q28_vars <- paste0("Q28", letters[1:12])
Q35_vars <- paste0("Q35", letters[1:9])
Q39_vars <- paste0("Q39", letters[1:9])

Q7_vars <- paste0("Q7", letters[1:22])
Q7tally_vars <- paste0("Q7", letters[1:22], "_tally")




climate_df[, Q1_vars]
climate_df <- climate_df %>% mutate_at(vars(Q1_vars), ~replace_na(., 0))
climate_df[, Q1_vars]

climate_df[, Q3_vars]
climate_df <- climate_df %>% mutate_at(vars(Q3_vars), ~ replace_na(.,0))
climate_df[, Q3_vars]

climate_df[,Q5_vars]
climate_df <- climate_df %>% mutate_at(vars(Q5_vars), ~ replace_na(.,0))
climate_df[, Q5_vars]

climate_df[, Q7_vars]
climate_df <- climate_df %>% mutate_at(vars(Q7_vars), ~ replace_na(., 0))
climate_df[,Q7_vars]

climate_df[, Q35_vars]
climate_df <- climate_df %>% mutate_at(vars(Q35_vars), ~ replace_na(., 0))
climate_df[,Q35_vars]

climate_df[, Q39_vars]
climate_df <- climate_df %>% mutate_at(vars(Q39_vars), ~ replace_na(., 0))
climate_df[,Q39_vars]


climate_df[, Q28_vars]
rf_df <- climate_df %>% drop_na(Q28_vars)
climate_df[, Q28_vars]
climate_df <- climate_df %>% drop_na(Q16_vars)







#use add_count to add a new column (n) that counts the number of observations for that variable
climate_df %>% select(student_id, Q27a, major) %>% add_count(major)
