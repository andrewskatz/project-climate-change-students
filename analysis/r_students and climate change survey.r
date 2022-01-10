library(tidyverse)
library(broom)
library(knitr)
library(kableExtra)
library(randomForest)
library(purrr)
library(purrrlyr)
library(RColorBrewer)
library(gplots)
library(gmodels)
library(dbscan)
library(umap)
library(psych)
library(factoextra)


####
#
# Reading in dataset ----
#
####



getwd()
setwd("G:/My Drive/AK Faculty/Research/Projects/project students and climate change")
list.files()
#copying from rdata file sent to me


# writing to csv file
write_csv(climate_data, "updated_climate_df_5.csv")

#writing filtered climate data df 3221 x 276
write_csv(train_df, "filtered climate data.csv")



climate_data <- read_csv("climate_data.csv") # 4605 x 311

climate_data <- read_csv("updated_climate_df_2.csv") # 4605 x 475 (includes Q7 variants, Q12 factors, Q16factors, Q28 factors, Q16bin, Q6bin)

climate_data <- read_csv("updated_climate_df_3.csv") # 4605 x 472

climate_data_4 <- read_csv("updated_climate_df_4.csv") # 4074 x 478

climate_data <- read_csv("updated_climate_df_5.csv") # 4074 x 497


updated_data <- read_csv("filtered_climate_survey_20200122.csv") #3216 x 256






###
#
# ** Adding variables to dataset -----
#
###

climate_data <- climate_data %>% 
  select(-`16_CareerFac1`, -`16_CareerFac2`, -`16_CareerFac3`, -`16_CareerFac4`)

climate_data <- climate_data %>% 
  select(-`17_EngTopics1`, -`17_EngTopics2`, -`17_EngTopics3`)

climate_data <- climate_data %>% 
  select(-`4_CareerSat1`, -`4_CareerSat2`, -`4_CareerSat3`, -`4_CareerSat4`)

climate_data <- climate_data %>% 
  select(-`28_GlobWarmBelief1`, -`28_GlobWarmBelief2`)

climate_data <- climate_data %>% 
  select(-`27_GlobWarmImpact1`, -`27_GlobWarmImpact2`, -`27_GlobWarmImpact3`, -`27_GlobalWarmImpact4`)

climate_data <- climate_data %>% 
  select(-`Filter 1`, -`F1 (2)`, -`F1 (3)`, -A, -B, -C)

write_csv(climate_data, "updated_climate_df_2.csv")

# old code used to create new column with major in three-letter code
climate_data <- climate_data %>% 
  mutate(major = case_when(Q29 == 1 ~ "Aer/Oce",
                            Q29 == 2 ~ "Agr/Biol",
                            Q29 == 3 ~ "Bio",
                            Q29 == 4 ~ "Civ",
                            Q29 == 5 ~ "Che",
                            Q29 == 6 ~ "Con",
                            Q29 == 7 ~ "Com",
                            Q29 == 8 ~ "Ele",
                            Q29 == 9 ~ "EngPhy",
                            Q29 == 10 ~ "Env/Eco",
                            Q29 == 11 ~ "Ind",
                            Q29 == 12 ~ "Mat",
                            Q29 == 13 ~ "Mec",
                            Q29 == 14 ~ "Min",
                            Q29 == 15 ~ "Nuc",
                            Q29 == 16 ~ "Sof",
                            Q29 == 17 ~ "Str/Arc",
                            Q29 == 18 ~ "Gen"))





# record Q6 to binary

climate_data <- climate_data %>% 
  mutate(Q6a_ind = case_when(Q6a == 1 | Q6a == 2 ~ 0,
                             Q6a == 3 | Q6a == 4 | Q6a == 5 ~ 1))


climate_data <- climate_data %>% 
  mutate(Q6b_ind = case_when(Q6b == 1 | Q6b == 2 ~ 0,
                             Q6b == 3 | Q6b == 4 | Q6b == 5 ~ 1))


climate_data <- climate_data %>% 
  mutate(Q6c_ind = case_when(Q6c == 1 | Q6c == 2 ~ 0,
                             Q6c == 3 | Q6c == 4 | Q6c == 5 ~ 1))


climate_data <- climate_data %>% 
  mutate(Q6d_ind = case_when(Q6d == 1 | Q6d == 2 ~ 0,
                             Q6d == 3 | Q6d == 4 | Q6d == 5 ~ 1))


climate_data <- climate_data %>% 
  mutate(Q6e_ind = case_when(Q6e == 1 | Q6e == 2 ~ 0,
                             Q6e == 3 | Q6e == 4 | Q6e == 5 ~ 1))



climate_data <- climate_data %>% 
  mutate(Q6f_ind = case_when(Q6f == 1 | Q6f == 2 ~ 0,
                             Q6f == 3 | Q6f == 4 | Q6f == 5 ~ 1))



climate_data <- climate_data %>% 
  mutate(Q6g_ind = case_when(Q6g == 1 | Q6g == 2 ~ 0,
                             Q6g == 3 | Q6g == 4 | Q6g == 5 ~ 1))


climate_data <- climate_data %>% 
  mutate(Q6h_ind = case_when(Q6h == 1 | Q6h == 2 ~ 0,
                             Q6h == 3 | Q6h == 4 | Q6h == 5 ~ 1))


climate_data <- climate_data %>% 
  mutate(Q6i_ind = case_when(Q6i == 1 | Q6i == 2 ~ 0,
                             Q6i == 3 | Q6i == 4 | Q6i == 5 ~ 1))


climate_data <- climate_data %>% 
  mutate(Q6j_ind = case_when(Q6j == 1 | Q6j == 2 ~ 0,
                             Q6j == 3 | Q6j == 4 | Q6j == 5 ~ 1))


climate_data <- climate_data %>% 
  mutate(Q6k_ind = case_when(Q6k == 1 | Q6k == 2 ~ 0,
                             Q6k == 3 | Q6k == 4 | Q6k == 5 ~ 1))




# add in binary columns for Q16 items with 0,1,2 as no and 3, 4 as yes

Q16_bin_vars <- paste0("Q16", "_bin_", letters[1:13])
Q16_vars <- paste0("Q16", letters[1:13])



for (i in seq_along(Q16_bin_vars)){
  climate_data[Q16_bin_vars[i]] <- case_when(climate_data[Q16_vars[i]] == 0 ~ 0,
                                        climate_data[Q16_vars[i]] == 1 ~ 0,
                                        climate_data[Q16_vars[i]] == 2 ~ 0, 
                                        climate_data[Q16_vars[i]] == 3 ~ 1, 
                                        climate_data[Q16_vars[i]] == 4 ~ 1)
}






####  Q12 Factor Analysis ---------


q12_fact_mat <- as.matrix(climate_data %>% 
                            select(Q12a:Q12i)) 

fa(q12_fact_mat, nfactors = 2, rotate = "oblimin")
# TLI = 0.956
# RMSEA = 0.083
# RMSR = 0.02





climate_data <- climate_data %>% 
  mutate(Q12abc = Q12a + Q12b + Q12c,
         Q12defghi = Q12d + Q12e + Q12f + Q12g + Q12h + Q12i)










####  Q16 Factor Analysis ---------


q16_fact_mat <- as.matrix(climate_data %>% 
  select(Q16a:Q16m) %>% 
    drop_na) 
  
fa(q16_fact_mat, nfactors = 4, rotate = "oblimin")
# TLI = 0.891
# RMSEA = 0.104
# RMSR = 0.03


# check PCA scree plot
q16_pca_res <- prcomp(q16_fact_mat, scale = TRUE)
fviz_eig(q16_pca_res)


climate_data <- climate_data %>% 
  mutate(Q16abc = Q16a + Q16b + Q16c,
         Q16dfg = Q16d + Q16f + Q16g,
         Q16hijk = Q16h + Q16i + Q16j + Q16k)











####  Q28 Factor Analysis ---------


q28_fact_mat <- as.matrix(climate_df %>% 
                            select(Q28a:Q28l)) 

fa(q28_fact_mat, nfactors = 2, rotate = "oblimin")
# TLI = 0.936
# RMSEA = 0.082
# RMSR = 0.03





climate_data <- climate_data %>% 
  mutate(Q28afghik = (Q28a + Q28f + Q28g + Q28h + Q28i + Q28k)/6,
         Q28cdejl = (Q28c + Q28d + Q28e + Q28j + Q28l)/5)








####
#
# ** Statistical analysis -----
#
####









sum_df <- climate_data %>% 
  mutate(gender = case_when(Q37 == 1 ~ "male",
                            Q37 == 2 ~ "female",
                            Q37 == 3 ~ "non-binary",
                            Q37 == 4 ~ "not listed")) %>% 
  group_by(major, gender) %>% 
  summarize(
    Q7a_mean = mean(Q7a_wt_sum),
    Q7b_mean = mean(Q7b_wt_sum),
    Q7c_mean = mean(Q7c_wt_sum),
    Q7d_mean = mean(Q7d_wt_sum),
    Q7e_mean = mean(Q7e_wt_sum),
    Q7f_mean = mean(Q7f_wt_sum),
    Q7g_mean = mean(Q7g_wt_sum),
    Q7h_mean = mean(Q7h_wt_sum),
    Q7i_mean = mean(Q7i_wt_sum),
    Q7j_mean = mean(Q7j_wt_sum),
    Q7k_mean = mean(Q7k_wt_sum),
    Q7l_mean = mean(Q7l_wt_sum),
    Q7m_mean = mean(Q7m_wt_sum),
    Q7n_mean = mean(Q7n_wt_sum),
    Q7o_mean = mean(Q7o_wt_sum),
    Q7p_mean = mean(Q7p_wt_sum),
    Q7q_mean = mean(Q7q_wt_sum),
    Q7r_mean = mean(Q7r_wt_sum),
    Q7s_mean = mean(Q7s_wt_sum),
    Q7t_mean = mean(Q7t_wt_sum),
    Q7u_mean = mean(Q7u_wt_sum),
    Q7v_mean = mean(Q7v_wt_sum),
    ) %>% 
  ungroup()

unique(climate_data$Q29)


#data exploration


# number of students in each major
table(climate_data$major)
cs_counts <- climate_data %>% 
  filter(major %in% c('Com', 'Sof')) %>% 
  group_by(School, major) %>% 
  summarize(count = n())

sum(cs_counts$count) #total number of CS or computer engineering students: 270

#number of schools and participants  from each school
unique(climate_data$School)
length(unique(climate_data$School)) #96 unique schools
table(climate_data$School)


## Question 3 - future plans - analysis
climate_data %>% 
  filter(Q3f == 1) %>% 
  ggplot(aes(x = major, fill = major)) + 
  geom_bar(stat = "count") +
  theme(legend.position = "none") +
  ggtitle("Which are you likely to pursue in the next five years? MA/MS (non-eng)")

Q3_vars <- paste0("Q3", letters[1:7])
Q3_vars

head(climate_data[,36:42])


# produce plots for all items in question 3 future plans 
plots <- map2(.x = climate_data[,36:42], .y = Q3_vars, .f = 
    ~ climate_data %>% 
    filter(climate_data[,.y] == 1) %>% 
    ggplot(aes(x = major, fill = major)) + 
    geom_bar(stat = "count") +
    theme(legend.position = "none") +
    labs(title = .y )
)

#access the first plot in plots
plots[[5]]

ggsave(filename = paste0(Q3_vars[1], ".pdf"), plot = plots[[1]])
Q3_vars[1]


# save the Q3 by major plots to working directory  
map2(.x = Q3_vars, .y = plots, 
     .f = ~ggsave(filename = paste0(.x, " bar plot by major", ".png"), plot = .y))
  
  





#year and GPA distribution
table(climate_data$Q30)
table(climate_data$Q31)
hist(climate_data$Q31)
unique(climate_data$Q31)
climate_data %>% 
  filter(Q31 != "*") %>% 
  mutate(Q31_num = as.numeric(Q31)) %>% 
  select(Q31_num) %>% 
  ggplot(aes(x = Q31_num)) + 
  geom_bar(stat = "count")


climate_data %>% 
  filter(Q31 != "*") %>% 
  mutate(Q31_num = as.numeric(Q31)) %>% 
  select(Q31_num, Q30) %>% 
  ggplot(aes(x = Q31_num)) + 
  geom_bar(stat = "count") +
  facet_grid(.~Q30)
#fourth-year particpants and fifth-year participants had different GPA distributions

#gender and GPA distributions
climate_data %>% 
  filter(Q31 != "*") %>% 
  mutate(Q31_num = as.numeric(Q31)) %>% 
  select(Q31_num, Q37) %>% 
  ggplot(aes(x = Q31_num)) + 
  geom_bar(stat = "count") +
  facet_grid(.~Q37)


#political affiliation
table(climate_data$Q32) #1038 rep, 1122 dem, 1121 ind, 702 other
climate_data %>% 
  filter(Q31 != "*") %>% 
  mutate(Q31_num = as.numeric(Q31)) %>% 
  select(Q31_num, Q32) %>% 
  ggplot(aes(x = Q31_num)) + 
  geom_bar(stat = "count") +
  facet_grid(.~Q32)
#similar GPA distributions by political affiliation


table(climate_data$Q28a)
temp_var <- 0
letter <- c("a", "b", "c", "d", "e", "f", "g", "h")
for (i in letters[1:12]){
  temp_var <- paste("Q28", i, sep = "")
  #print(temp_var)
  print(table(temp_var))
}




climate_data %>% 
  group_by(Q29) %>% 
  summarize(
    count = n(),
    mean = mean(Q10b, na.rm = TRUE), 
    sd = sd(Q10b, na.rm = TRUE),
    median = median(Q10b, na.rm = TRUE),
    IQR = IQR(Q10b, na.rm = TRUE)
  )





letters[1:5]
LETTERS[1:5]




sapply(climate_data, function(x) sum(is.na(x)))
climate_data %>% summarize(na_count = sum(is.na(Q37)))



gen_maj_tib <- climate_data %>% 
  group_by(Q29, Q37) %>% 
  count() %>% 
  ungroup()



table(climate_data$Q7a_wt_sum)
climate_data %>% 
  ggplot(aes(x = Q7a_wt_sum, fill=as.factor(Q37))) +
  geom_histogram(alpha = 0.6)

#looking at distribution of answers to Q7a by gender
climate_data %>% filter(Q37 %in% c(1, 2)) %>% 
  ggplot(aes(x = Q7a_wt_sum, fill=as.factor(Q37))) +
  geom_histogram() +
  facet_grid(Q37~.)


climate_data %>% 
  filter(Q37 %in% c(1, 2, 3, 4)) %>% 
  ggplot(aes(y = Q7a_wt_sum, x = as.factor(Q37), fill = as.factor(Q37))) +
  geom_boxplot()

Q7_sum_tib <- climate_data %>% 
  select(Q7a_wt_sum:Q7v_wt_sum, Q29, Q37)

Q7_sum_gathered <- Q7_sum_tib %>%
  filter(Q37 %in% c(1, 2, 3, 4)) %>% 
  gather(key = "item", value = "item_score", Q7a_wt_sum:Q7v_wt_sum)

Q7_sum_tib %>%
  filter(Q37 %in% c(1, 2, 3, 4)) %>% 
  gather(key = "item", value = "item_score", Q7a_wt_sum:Q7v_wt_sum) %>% 
  ggplot(aes(y = item_score, x = as.factor(Q37), fill = as.factor(Q37))) +
  geom_boxplot() +
  facet_wrap(.~item) +
  xlab("Gender") +
  ylab("Item score (0-10)") +
  ggtitle("Question 7 aggregate scores for each topic separated by gender") +
  theme(legend.position = "none")



#heatmap of Q7_sum_tib
heatmapQ7 <- Q7_sum_tib %>% 
  filter(Q37 %in% c(1, 2, 3, 4)) %>% 
  select(-c(Q37, Q29))

heatmap(as.matrix(heatmapQ7), scale = "column")
  


ggpairs(heatmapQ7)


ggcorr(heatmapQ7, method = c("everything", "pearson"))









####
#
# Random forests for Q5 and Q16 ######
#
####



# prepraing the data
# start with the climate_data dataframe


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


str(climate_data)


rf_df <- climate_data

rf_df[, Q1_vars]
rf_df <- rf_df %>% mutate_at(vars(Q1_vars), ~replace_na(., 0))
rf_df[, Q1_vars]

rf_df[, Q3_vars]
rf_df <- rf_df %>% mutate_at(vars(Q3_vars), ~ replace_na(.,0))
rf_df[, Q3_vars]

rf_df[,Q5_vars]
#rf_df <- rf_df %>% mutate_at(vars(Q5_vars), ~ replace_na(., 0))

rf_df[, Q7_vars]
rf_df <- rf_df %>% mutate_at(vars(Q7_vars), ~ replace_na(., 0))
rf_df[,Q7_vars]

rf_df[, Q35_vars]
rf_df <- rf_df %>% mutate_at(vars(Q35_vars), ~ replace_na(., 0))
rf_df[,Q35_vars]

rf_df[, Q39_vars]
rf_df <- rf_df %>% mutate_at(vars(Q39_vars), ~ replace_na(., 0))
rf_df[,Q39_vars]


rf_df[, Q28_vars]
rf_df <- rf_df %>% drop_na(Q28_vars)
rf_df[, Q28_vars]
rf_df <- rf_df %>% drop_na(Q16_vars)




# turning variables into factors

rf_df <- dmap(rf_df, as.factor)


str(rf_df)


# exploratory plots

climate_df %>% 
  filter(Q29 == 4) %>% 
  select(Q27_vars) %>% 
  summarize_all(list(~min(.), 
                     ~max(.),
                     ~mean(.), 
                     ~sd(.), 
                     ~median(.),
                     ~n(),
                     ~mode(.))) %>% 
  gather(stat, val) %>% 
  separate(stat, into = c("var", "stat"), sep = "_") %>% 
  spread(stat, val) %>% 
  select(n, mean, sd, median, min, max)


# exploratory variables

test_df <- climate_df %>% 
  filter(Q29 == 4) %>% 
  select(Q27_vars) %>% 
  summarize_all(funs(min(.), 
                     max(.),
                     mean(.), 
                     sd(.), 
                     median(.),
                     n(),
                     mode(.))) %>% 
  gather(stat, val) %>% 
  separate(stat, into = c("item", "stat"), sep = "_") %>% 
  spread(stat, val) %>% 
  select(item, n, mean, sd, median, min, max) %>% 
  mutate_at(stat_vars, as.numeric) 

test_df

Q28_var_names <- c(
  Q28a = "environ",
  Q28b = "moral",
  Q28c = "religious",
  Q28d = "soc. just.",
  Q28e = "politic",
  Q28f = "sci",
  Q28g = "eng",
  Q28h = "health",
  Q28i = "economic",
  Q28j = "security",
  Q28k = "agricul",
  Q28l = "poverty"
)

climate_df %>% 
  filter(Q29 == 4) %>% 
  select(Q28_vars) %>% 
  gather(key = "item", value = "response", Q28_vars) %>% 
  ggplot(aes(x = response)) +
  geom_histogram() + 
  facet_grid(item ~ ., labeller = labeller(item = Q28_var_names)) +
  ggtitle("Distribution of Question 28 responses for civil engineering") +
  ylab("Count") +
  xlab("Item Response") +
  theme_bw() +
  theme(plot.title = element_text(size = 12, hjust = 0.5),
        strip.text.y = element_text(size = 7))


Q28_hist <- map2(.x = Q28_vars, 
                 .y = Q28_var_names, 
                 .f = ~ climate_df %>% 
  filter(Q29 == 4) %>% 
  select(.x) %>% 
  gather(key = "item", value = "response", .x) %>% 
  ggplot(aes(x = response)) +
  geom_histogram() + 
  facet_grid(item ~ .) +
  ggtitle("Distribution of Question 28 responses for civil engineering") +
  ylab("Count") +
  xlab("Item Response") +
  theme_bw() +
  theme(plot.title = element_text(size = 12, hjust = 0.5),
        strip.text.y = element_text(size = 7)))

Q28_hist[5]

for (i in seq_along(Q28_hist)) {
  Q28_hist[[i]]
}


colnames(rf_df)

selected_vars_idx <- c(3:290, 296)
selected_vars_idx[289]




# remove columns with majority na (noticed from visual inspection)
removal_vars <- c("School", "Litho", "major", "Q8", "Q13", "Q14", "Q33_", 
                  "Q37_", "Q38_", "Q39_", "Q40", "1", "2", "4", "abe", "lm", "dfg", "hijk", 
                  "sum", "tally", "spec", "other", "ele")
removal_vars_start <- paste0("^(", paste(removal_vars, collapse="|"), ")")
removal_vars_end <- paste0("(", paste(removal_vars, collapse="|"), ")$")


# old method for removing columns with majority na
# train_df <- train_df %>% select(-starts_with("Q39_"))
# train_df <- train_df %>% select(-starts_with("1"))


# new way of removing columns with majority na (listed in the removal_vars vector)
#train_df <- train_df %>% select(-matches(removal_vars))

train_df <- rf_df %>% select(-matches(removal_vars_start))
train_df <- train_df %>% select(-matches(removal_vars_end))


# remove the original Q7 vars because they were too messy - use tallies instead
train_df <- train_df %>% select(-Q7_vars)
train_df <- train_df %>% select(-Q16_vars) # drop Q16 vars for Q5 training


# remove rows with too many nas (defined as more than 10% of variables)
# rowSums(is.na(train_df[1:15, ])) # test code
# full implementation
train_df <- train_df[!rowSums(is.na(train_df)) > ncol(train_df)*.1,] # reduced df by ~41 to 3248

write_csv(train_df, "filtered_climate_survey_20200509.csv")

str(train_df)

# impute data (not used)
# train_df.impute.list <- rfImpute(Q5a ~., data = train_df, iter = 6, ntree = 500)



####
#
## Random forest implementation for Q5 (items a-j) ----
#
####


set.seed(42)

# looking at specific outcome variables
#train_df[, "Q5a"]
#train_df[, "Q5b"]
#table(train_df$Q5b)

# create index for training set, using 70/30
# train_idx <- sample(1:nrow(train_df), 0.7*nrow(train_df))


# remove the Q16 binary variables that were created
train_df <- train_df %>% select(-Q16_bin_vars)



# creating all formulas in a list
formulas_Q5 <- map(.x = Q5_vars, .f = ~ paste0(.x, " ~."))



removal_list_Q5 <- map(.x = seq_along(Q5_vars), .f = ~ Q5_vars[-.x])
removal_list_Q5 <- map(.x = seq_along(Q5_vars), 
                        .f = ~ paste0("^(", paste(removal_list_Q5[[.x]], collapse="|"), ")"))


train_lists_Q5 <- map(.x = removal_list_Q5,
                       .f = ~ select(train_df, -matches(.x)))




# testing storing the formulas as a list
# formulas <- list("Q5a ~ .", "Q5c ~ .")

# creating all formulas in a list
formulas <- map(.x = Q5_vars, .f = ~ paste0(.x, " ~."))
# removing formula for Q5b
#formulas <- formulas[-2]

# Q5_vars <- set_names(Q5_vars)




# new method removing the other Q5 items (and Q16 vars, ideally)
model_rf_Q5 <- map2(.x = formulas_Q5, 
                     .y = train_lists_Q5,
                     .f = ~ randomForest(formula = as.formula(.x), 
                                         data = .y,
                                         ntree = 1000,
                                         proximity = TRUE,
                                         na.action = na.roughfix,
                                         importance = TRUE))


names(model_rf_Q5) <- Q5_vars




# checking first model for Q5a
#model_rf_Q5[[1]]



# checking oob error rates for models
# model_rf_Q5[[1]]$err.rate


#spot checking some models, e.g., Q5g, that had very high "1" classification error and very low "0" error
model_rf_Q5[[4]]$err.rate
model_rf_Q5[[4]]
model_rf_Q5[[4]]

# create summary table of counts of "yes" and "no" for expressing an interest in addressing topics in career
Q5_summary <- train_df %>% 
  select(starts_with("Q5")) %>% 
  gather(key = "question", value = "response") %>% 
  group_by(question, response) %>% 
  summarize(n = n())
Q5_summary
  
#checking to make sure the table Q5_summary is correct
table(train_df$Q5d)
table(train_df$Q5a)
table(train_df$Q5e)



#create a summary plot of the Q% item distributions and save the plot
Q5_sum_plot <- Q5_summary %>% ggplot(aes(x = question, y = n, fill = as.factor(response))) +
  scale_fill_brewer(name = "Item Response", palette = "Set2") +
  ggtitle("Q5  - Topics to address in career - yes and no distributions")

Q5_sum_plot

ggsave(plot = Q5_sum_plot, filename = "Q5 summary distributions plot.jpg", device = "jpeg")  




#creating code for one version of error table to use in error rate plot
oob_error_Q5[[1]] <- tibble(
  Trees = rep(1:nrow(model_rf_Q5[[1]]$err.rate), times = 3),
  Type = rep(c("OOB", "0", "1"), each  geom_bar(stat="identity", position = "dodge") +
 = nrow(model_rf[[1]]$err.rate)),
  Error = c(model_rf[[1]]$err.rate[,"OOB"],
            model_rf[[1]]$err.rate[, "0"],
            model_rf[[1]]$err.rate[, "1"]))


#attempting code for all 10 models at once that were created by default random forest settings and full dataset
oob_error_Q5 <- map(.x = model_rf_Q5, .f = ~ tibble(
  Trees = rep(1:nrow(.x$err.rate), times = 3),
  Type = rep(c("OOB", "0", "1"), each = nrow(.x$err.rate)),
  Error = c(.x$err.rate[,"OOB"],
            .x$err.rate[, "0"],
            .x$err.rate[, "1"]))
)

#testing OOB error rate plot creation for Q5a
# ggplot(data = oob_error_Q5[[1]], aes(x = Trees, y = Error)) +
#  geom_line(aes(color = Type))


#create all plots at once
oob_error_plots_Q5 <- map2(.x = oob_error_Q5, .y = names(oob_error_Q5),
    .f = ~ggplot(data = .x, aes(x = Trees, y = Error)) +
      geom_line(aes(color = Type)) + 
      ggtitle(paste0(.y, " error rate"))
)

oob_error_plots_Q5[[4]]

# save plots
Q5_error_rate_filenames <- paste0(Q5_vars, " error rate (removing other Q5 and Q16 items) plot 1000 trees full dataset.jpg")
Q5_error_rate_filenames

map2(.x = oob_error_plots_Q5, 
     .y = Q5_error_rate_filenames, 
     .f = ~ ggsave(.x, filename = .y, device = "jpeg"))




#old method: testing importance plot
#varImpPlot(model_rf_Q5[["Q5a"]])
#importance_plots <- map(.x = model_rf_Q5, .f = ~varImpPlot(.x))
#importance_plots

#model_rf_Q5$Q5b$call

# use importance function to calculate variable importance using permutation improtance (via type = 1)
imp_f <- importance(model_rf_Q5[[4]], type = 1, scale = F)

row.names(imp_f)
featureImportance <- data.frame(Feature = row.names(imp_f), Importance = imp_f[,1])

p <- featureImportance %>% 
  top_n(30, Importance) %>% 
  ggplot(aes(x = reorder(Feature, -Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "#53cfff", width = 0.65) +
  coord_flip() + 
  theme_light(base_size = 20) +
  theme(axis.title.x = element_text(size = 10, color = "black"),
        axis.title.y = element_blank(),
        axis.text.x  = element_text(size = 10, color = "black"),
        axis.text.y  = element_text(size = 10, color = "black"),
        plot.title = element_text(size = 10, hjust = 0.5)) +
  ggtitle("Random Forest Variable Importance for Career Interest in Climate Change")

p



# setup code for one importance plot
imp <- cbind.data.frame(Feature=rownames(model_rf_Q5[[4]]$importance), model_rf_Q5[[4]]$importance)
imp
g <- imp %>% 
  top_n(30, MeanDecreaseGini) %>% 
  ggplot(aes(x=reorder(Feature, -MeanDecreaseGini), y=MeanDecreaseGini))
g + 
  geom_bar(stat = 'identity') + 
  xlab('Feature') + 
  ggtitle(names(model_rf_Q5[4])) +
  coord_flip()

# create all importance plots and save to list called importance_plots
importance_plots_Q5 <- map(.x = seq(length(model_rf_Q5)), .f = ~ 
  cbind.data.frame(Feature=rownames(model_rf_Q5[[.x]]$importance), model_rf_Q5[[.x]]$importance) %>% 
  top_n(30, MeanDecreaseGini) %>% 
  ggplot(aes(x=reorder(Feature, -MeanDecreaseGini), y=MeanDecreaseGini)) +
  geom_bar(stat = 'identity') + 
  xlab('Feature') + 
  ggtitle(paste0("Importance plot for ", names(model_rf_Q5[.x]), " using full dataset")) +
  coord_flip()
)

# checking the individual plots are different
importance_plots_Q5[[1]]
importance_plots_Q5[[2]]
importance_plots_Q5[[3]]
importance_plots_Q5[[4]]
importance_plots_Q5[[5]]
importance_plots_Q5[[6]]
importance_plots_Q5[[7]]
importance_plots_Q5[[8]]
importance_plots_Q5[[9]]
importance_plots_Q5[[10]]


# saving all the importance plots

#first create the filenames
imp_plot_filenames_Q5 <- paste0(Q5_vars, " importance plot using full dataset (removing other Q5 and Q16 items).jpg")
imp_plot_filenames_Q5

# use map to save all importance plots at once
map2(.x = importance_plots_Q5, 
     .y = imp_plot_filenames_Q5,
     .f = ~ ggsave(plot = .x, 
                   filename = .y, 
                   device = "jpeg",
                   width = 7,
                   height = 5))



imp <- as_tibble(importance(model_rf_Q5), index = TRUE)
imp %>% arrange(desc(MeanDecreaseGini))














####
# 
#
### Q16 random forests ######
#
#
####

Q16_vars #checking to make sure already made above


train_df_Q16 <- train_df %>% select(-starts_with("Q13"))

# inspecting distribution of Q16 data

Q16_summary <- train_df %>% 
  select(Q16_vars) %>% 
  gather(key = "question", value = "response") %>% 
  group_by(question, response) %>% 
  summarize(n = n())


#checking to make sure the table Q5_summary is correct

table(train_df$Q16g)
table(train_df$Q16a)
table(train_df$Q16e)


#create a summary plot of the Q% item distributions and save the plot
Q16_sum_plot <- Q16_summary %>% ggplot(aes(x = question, y = n, fill = as.factor(response))) +
  geom_bar(stat="identity", position = "dodge") +
  scale_fill_brewer(name = "Item Response", palette = "Accent") +
  xlab("Question") + 
  ggtitle("Q16  - Interest in working toward solutions in career - 0 (not interested) to 4 (very interested)")

ggsave(plot = Q16_sum_plot, 
       filename = "Q16 summary distributions plot.jpg", 
       device = "jpeg",
       width = 8.5,
       height = 6)  





### creating models


set.seed(42)


# preparing to create random forests for all Q16 outcome variables
# tbt formulas <- paste0(Q16_vars, " ~ .")

# testing storing the formulas as a list
# tbt formulas <- list("Q5a ~ .", "Q5c ~ .")

train_df <- train_df %>% select(-Q16_bin_vars)

# creating all formulas in a list
formulas_Q16 <- map(.x = Q16_vars, .f = ~ paste0(.x, " ~."))


#test_list <- list_along(formulas_Q16)
#names(test_list) <- Q16_vars

removal_list_Q16 <- map(.x = seq_along(Q16_vars), .f = ~ Q16_vars[-.x])
removal_list_Q16 <- map(.x = seq_along(Q16_vars), 
                        .f = ~ paste0("^(", paste(removal_list_Q16[[.x]], collapse="|"), ")"))


train_lists_Q16 <- map(.x = removal_list_Q16,
                       .f = ~ select(train_df, -matches(.x)))



# ntree (1000), default mtry (17), no training split (full dataset used), median value for nas
model_rf_Q16 <- map2(.x = formulas_Q16, 
                     .y = train_lists_Q16,
                     .f = ~ randomForest(formula = as.formula(.x), 
                                      data = .y,
                                      ntree = 1000,
                                      proximity = TRUE,
                                      na.action = na.roughfix,
                                      importance = TRUE))


# change the names of each of the list elements
names(model_rf_Q16) <- Q16_vars
model_rf_Q16[[1]]
#model_rf_Q16[[1]]$err.rate


# code for one version of error table to use in error rate plot
oob_error_Q16[[1]] <- tibble(
  Trees = rep(1:nrow(model_rf_Q16[[1]]$err.rate), times = 6),
  Type = rep(c("OOB", "0", "1", "2", "3", "4"), each = nrow(model_rf_Q16[[1]]$err.rate)),
  Error = c(model_rf_Q16[[1]]$err.rate[,"OOB"],
            model_rf_Q16[[1]]$err.rate[, "0"],
            model_rf_Q16[[1]]$err.rate[, "1"],
            model_rf_Q16[[1]]$err.rate[, "2"],
            model_rf_Q16[[1]]$err.rate[, "3"],
            model_rf_Q16[[1]]$err.rate[, "4"]))



# code for all 10 models at once that were created by default random forest settings and full dataset
oob_error_Q16 <- map(.x = model_rf_Q16, .f = ~ tibble(
  Trees = rep(1:nrow(.x$err.rate), times = 6),
  Type = rep(c("OOB", "0", "1", "2", "3", "4"), each = nrow(.x$err.rate)),
  Error = c(.x$err.rate[,"OOB"],
            .x$err.rate[, "0"],
            .x$err.rate[, "1"],
            .x$err.rate[, "2"],
            .x$err.rate[, "3"],
            .x$err.rate[, "4"]))
)

#create one plot
#ggplot(data = oob_error_Q16[[1]], aes(x = Trees, y = Error)) +
#  geom_line(aes(color = Type))


#create all plots at once
oob_error_plots_Q16 <- map2(.x = oob_error_Q16, .y = names(oob_error_Q16),
                        .f = ~ggplot(data = .x, aes(x = Trees, y = Error)) +
                          geom_line(aes(color = Type)) + 
                          ggtitle(paste0(.y, " error rate"))
)


# save plots
filenames <- paste0(Q16_vars, " multiclass error rate plot 1000 trees full dataset.jpg")

map2(.x = oob_error_plots_Q16, .y = filenames, 
     .f = ~ ggsave(.x, filename = .y, device = "jpeg"))



#importance plots for Q16
importance_plots_Q16 <- map(.x = seq(length(model_rf_Q16)), .f = ~ 
                             cbind.data.frame(Feature=rownames(model_rf_Q16[[.x]]$importance), model_rf_Q16[[.x]]$importance) %>% 
                             top_n(30, MeanDecreaseGini) %>% 
                             ggplot(aes(x=reorder(Feature, -MeanDecreaseGini), y=MeanDecreaseGini)) +
                             geom_bar(stat = 'identity') + 
                             xlab('Feature') + 
                             ggtitle(paste0("Importance plot for ", names(model_rf_Q16[.x]), " using full dataset")) +
                             coord_flip()
)


# saving importance plots for Q16
imp_plot_filenames_Q16 <- paste0(Q16_vars, " multiclass importance plot using full dataset.jpg")
# use map to save all importance plots at once
map2(.x = importance_plots_Q16, 
     .y = imp_plot_filenames_Q16,
     .f = ~ ggsave(plot = .x, filename = .y, device = "jpeg"))







####
#
#
# Q16 as binary outcome models -----
#
#
####



# remove the original Q16 vars
train_df <- train_df %>% select(-Q16_vars)

# creating all formulas in a list
formulas_Q16_bin <- map(.x = Q16_bin_vars, .f = ~ paste0(.x, " ~."))



removal_list_Q16_bin <- map(.x = seq_along(Q16_bin_vars), .f = ~ Q16_bin_vars[-.x])
removal_list_Q16_bin <- map(.x = seq_along(Q16_bin_vars), 
                        .f = ~ paste0("^(", paste(removal_list_Q16_bin[[.x]], collapse="|"), ")"))


train_lists_Q16_bin <- map(.x = removal_list_Q16_bin,
                       .f = ~ select(train_df, -matches(.x)))



# ntree (1000), default mtry (17), no training split (full dataset used), median value for nas
model_rf_Q16_bin_class <- map2(.x = formulas_Q16_bin, 
                     .y = train_lists_Q16_bin,
                     .f = ~ randomForest(formula = as.formula(.x), 
                                         data = .y,
                                         ntree = 1000,
                                         proximity = TRUE,
                                         na.action = na.roughfix,
                                         importance = TRUE))


# from trying as regression tree (without turning all items into factors in the data processing step)
# model_rf_Q16_bin_reg <- model_rf_Q16_bin



# change the names of each of the list elements
names(model_rf_Q16_bin_reg) <- Q16_bin_vars
names(model_rf_Q16_bin_class) <- Q16_bin_vars
model_rf_Q16_bin_class[[1]]
#model_rf_Q16[[1]]$err.rate


# code for one version of error table to use in error rate plot
oob_error_Q16[[1]] <- tibble(
  Trees = rep(1:nrow(model_rf_Q16[[1]]$err.rate), times = 6),
  Type = rep(c("OOB", "0", "1", "2", "3", "4"), each = nrow(model_rf_Q16[[1]]$err.rate)),
  Error = c(model_rf_Q16[[1]]$err.rate[,"OOB"],
            model_rf_Q16[[1]]$err.rate[, "0"],
            model_rf_Q16[[1]]$err.rate[, "1"],
            model_rf_Q16[[1]]$err.rate[, "2"],
            model_rf_Q16[[1]]$err.rate[, "3"],
            model_rf_Q16[[1]]$err.rate[, "4"]))



# code for all 10 models at once that were created by default random forest settings and full dataset
oob_error_Q16_bin_class <- map(.x = model_rf_Q16_bin_class, .f = ~ tibble(
  Trees = rep(1:nrow(.x$err.rate), times = 3),
  Type = rep(c("OOB", "0", "1"), each = nrow(.x$err.rate)),
  Error = c(.x$err.rate[,"OOB"],
            .x$err.rate[, "0"],
            .x$err.rate[, "1"]))
)

#create one plot
#ggplot(data = oob_error_Q16[[1]], aes(x = Trees, y = Error)) +
#  geom_line(aes(color = Type))


#create all plots at once
oob_error_plots_Q16_bin_class <- map2(.x = oob_error_Q16_bin_class, .y = names(oob_error_Q16_bin_class),
                            .f = ~ggplot(data = .x, aes(x = Trees, y = Error)) +
                              geom_line(aes(color = Type)) + 
                              ggtitle(paste0(.y, " error rate"))
)


# save plots
filenames <- paste0(Q16_vars, " binary variable error rate plot 1000 trees full dataset.jpg")

map2(.x = oob_error_plots_Q16_bin_class, .y = filenames, 
     .f = ~ ggsave(.x, filename = .y, device = "jpeg"))



#importance plots for Q16
importance_plots_Q16_bin_class <- map(.x = seq(length(model_rf_Q16_bin_class)), .f = ~ 
                              cbind.data.frame(Feature=rownames(model_rf_Q16_bin_class[[.x]]$importance), model_rf_Q16_bin_class[[.x]]$importance) %>% 
                              top_n(30, MeanDecreaseGini) %>% 
                              ggplot(aes(x=reorder(Feature, -MeanDecreaseGini), y=MeanDecreaseGini)) +
                              geom_bar(stat = 'identity') + 
                              xlab('Feature') + 
                              ggtitle(paste0("Importance plot for ", names(model_rf_Q16_bin_class[.x]), " using full dataset")) +
                              coord_flip()
)


# saving importance plots for Q16
imp_plot_filenames_Q16_bin_class <- paste0(Q16_bin_vars, " as binary variable importance plot using full dataset.jpg")
# use map to save all importance plots at once
map2(.x = importance_plots_Q16_bin_class, 
     .y = imp_plot_filenames_Q16_bin_class,
     .f = ~ ggsave(plot = .x, filename = .y, device = "jpeg"))
















# Q33 (religion) seems to be important variable behind major
# checking distributions
table(train_df$Q33)

train_df %>% 
  drop_na(Q33) %>% 
  group_by(major) %>% 
  ggplot(aes(x = Q33)) +
  geom_bar(stat = 'identity')



hist(climate_data$Q33, na.rm = TRUE)








###
#
# Linear models for Q5 and Q16 ----
#
####
train_df <- rf_df



kruskal.test(Q16a ~ Q33, data = train_df)













####
#
#
#  Contingency tables for Q5 as a function of predictors from random forests above -------
#
#
####

# plots of Q5d (address climate change in career) and Q20d (global warming is personally important)


climate_data %>% ggplot(aes(x = Q5c, fill = major)) +
  geom_bar(stat = "count") +
  facet_wrap(. ~ major)

#changing the faceting either by Q5d or Q20d

climate_data %>% 
  ggplot(aes(x = Q20d, fill = major)) +
  geom_bar(stat = "count") +
  facet_grid(Q5d ~.) +
  ggtitle("Q20d - global warming as personally important vs Q5d - climate change")


climate_data %>% 
  ggplot(aes(x = Q5d, fill = major)) +
  geom_bar(stat = "count") +
  facet_grid(Q20d ~.) +
  ggtitle("Q20d - global warming as personally important vs Q5d - climate change")


climate_data %>% 
  ggplot(aes(x = Q5d, y = ..prop.., fill = major)) +
  geom_bar(position = "dodge")




# alternative ways to create the same plot - first way creates new dataframe of percentages
# and then passes that dataframe to ggplot
# second method just uses "fill" for position argument in geom_bar
d2 <- climate_data %>% 
  group_by(major, Q5d) %>% 
  summarize(count = n()) %>% 
  mutate(perc = count/sum(count))

table(climate_data$major)

d2 %>% 
  ggplot(aes(x = major, y = perc*100, fill = factor(Q5d))) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set2") +
  labs(x = "Major", y = "Percent", fill = "Q5d")

g <- d2 %>% 
  filter(Q5d == 1) %>% 
  ggplot(aes(x = reorder(major, -perc), y = perc*100)) +
  geom_bar(stat = "identity") +
  labs(x = "Major", y = "Percent", fill = "Q5d", title = "Percentage of students in major interested in addressing climate change in career") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
g

ggsave(filename = "Q5d percentage saying yes by major", 
       plot = g, 
       device = "jpeg", 
       width = 8,
       height = 5)    

climate_data %>% 
  ggplot(aes(x = major, fill = factor(Q5d))) +
  geom_bar(position = "fill") +
  scale_fill_brewer(palette = "Set2") +
  labs(x = "Major", y = "Percent", fill = "Q5d", title = "Proportions of Q5d (interest in addressing climate change) by major")




cont_table <- table(climate_data$Q5d, climate_data$major)
cont_table

mosaicplot(cont_table, shade = TRUE, main = "Contingency table: Major vs Climate change career interest")

contingency_tab <- xtabs(~ major + Q5d, data = climate_data, na.action = na.pass)
contingency_tab



CrossTable(climate_data$major, climate_data$Q5d, chisq = TRUE)


CrossTable(climate_data$Q5d, climate_data$major, chisq = TRUE)
























Q5d_major_gender_sat <- loglm(~ major*Q5d*Q37, data = contingency_tab)
summary(Q5d_major_gender)












Q5d_major_gender_three <- loglm(~ major + Q5d + Q37 + major:Q5d + major:Q37 + Q5d:Q37, data = contingency_tab)
summary(Q5d_major_gender_three)



Q5d_xtabs <- climate_data %>% 
  select(Q20d, Q5d) %>% 
  group_by(Q20d, Q5d) %>% 
  summarise(n = n()) %>% 
  ungroup()





##### NOTE: Use train_df for the dataframe rather than the full dataset

# using chi-square test to for Q20d and Q5d
CrossTable(train_df$Q20d, train_df$Q5d, chisq = TRUE)
xtabs(~ Q20d + Q5d, data=train_df)


# using chi-square for Q18c and Q5d
CrossTable(train_df$Q18c, train_df$Q5d, chisq = TRUE)
xtabs(~ Q18c + Q5d, data=train_df)


# using chi-square for Q18k and Q5d
CrossTable(train_df$Q18k, train_df$Q5d, chisq = TRUE)
xtabs(~ Q18k + Q5d, data=train_df)


# using chi-square for Q29 (major) and Q5d
# drop nuclear engineering and engineering physics because they have small N (responses 9 and 15)
chisq_q29 <- CrossTable(train_df$Q29, train_df$Q5d, chisq = TRUE)
summary(chisq_q29)
xtabs(~ Q29 + Q5d, data=train_df)



# using chi-square for Q29 (major) and Q5d
# drop nuclear engineering and engineering physics because they have small N (responses 9 and 15)
chisq_q33 <- CrossTable(train_df$Q33, train_df$Q5d, chisq = TRUE)
xtabs(~ Q33 + Q5d, data=train_df)



# using logistic regression for items Q28_tech and Q29_social
logistic <- glm(Q5d ~ Q28_afghk_tech + Q28_cdelj_social, data = train_df, family = "binomial")
summary(logistic)
tidy(logistic) %>% 
  kable() %>% 
  kable_styling()



typeof(train_df$Q28_afghk_tech)
test_iv <- as.numeric(train_df$Q28_afghk_tech)
test_iv_2 <- as.numeric(train_df$Q28_cdelj_social)
test_dv <- train_df$Q5d
logistic <- glm(test_dv ~ test_iv + test_iv_2, family = "binomial")
summary(logistic)



dt <- as.table(as.matrix(Q5d_xtabs))
balloonplot(dt, label = FALSE, show.margins = FALSE)



write_csv(train_df, "train_df.csv")






####
#
#
# Clustering for question 27 (global warming will start to have a serious effect on...) ----
#
####

climate_df <- read_csv("filtered_climate_survey_20200122.csv")
table(climate_df$Q29)
climate_df <- climate_df %>% drop_na(Q28_vars)
climate_df <- climate_df %>% drop_na(Q27_vars) #should have 



climate_df <- climate_df %>% 
  mutate(Q28_social = Q28c + Q28d + Q28e + Q28j + Q28l,
         Q28_tech = Q28a + Q28f + Q28g + Q28h + Q28k,
         Q28_social_norm = Q28_social / 5,
         Q28_tech_norm = Q28_tech / 5)


civil_df <- climate_df %>% 
  filter(Q29 == 4) %>% 
  mutate(Q27a_bin = case_when(Q27a == 1 | Q27a == 2 ~ "Sooner",
                              Q27a == 3 | Q27a == 4 | Q27a == 5 ~ "Later"),
         Q27b_bin = case_when(Q27b == 1 | Q27b == 2 ~ "Sooner",
                              Q27b == 3 | Q27b == 4 | Q27b == 5 ~ "Later"),
         Q27c_bin = case_when(Q27c == 1 | Q27c == 2 ~ "Sooner",
                              Q27c == 3 | Q27c == 4 | Q27c == 5 ~ "Later"),
         Q27d_bin = case_when(Q27d == 1 | Q27d == 2 ~ "Sooner",
                              Q27d == 3 | Q27d == 4 | Q27d == 5 ~ "Later"),
         Q27e_bin = case_when(Q27e == 1 | Q27e == 2 ~ "Sooner",
                              Q27e == 3 | Q27e == 4 | Q27e == 5 ~ "Later"),
         Q27f_bin = case_when(Q27f == 1 | Q27f == 2 ~ "Sooner",
                              Q27f == 3 | Q27f == 4 | Q27f == 5 ~ "Later"),
         Q27g_bin = case_when(Q27g == 1 | Q27g == 2 ~ "Sooner",
                              Q27g == 3 | Q27g == 4 | Q27g == 5 ~ "Later"),
         Q27h_bin = case_when(Q27h == 1 | Q27h == 2 ~ "Sooner",
                              Q27h == 3 | Q27h == 4 | Q27h == 5 ~ "Later"),
         Q27i_bin = case_when(Q27i == 1 | Q27i == 2 ~ "Sooner",
                              Q27i == 3 | Q27i == 4 | Q27i == 5 ~ "Later"))


# making binary variable values rather than character variable values

Q27_bin_num_vars <- paste0("Q27", letters[1:9], "_bin_num")
Q27_bin_num_vars

civil_df <- civil_df %>% 
  mutate(Q27a_bin_num = case_when(Q27a == 1 | Q27a == 2 ~ 0,
                              Q27a == 3 | Q27a == 4 | Q27a == 5 ~ 1),
         Q27b_bin_num = case_when(Q27b == 1 | Q27b == 2 ~ 0,
                              Q27b == 3 | Q27b == 4 | Q27b == 5 ~ 1),
         Q27c_bin_num = case_when(Q27c == 1 | Q27c == 2 ~ 0,
                              Q27c == 3 | Q27c == 4 | Q27c == 5 ~ 1),
         Q27d_bin_num = case_when(Q27d == 1 | Q27d == 2 ~ 0,
                              Q27d == 3 | Q27d == 4 | Q27d == 5 ~ 1),
         Q27e_bin_num = case_when(Q27e == 1 | Q27e == 2 ~ 0,
                              Q27e == 3 | Q27e == 4 | Q27e == 5 ~ 1),
         Q27f_bin_num = case_when(Q27f == 1 | Q27f == 2 ~ 0,
                              Q27f == 3 | Q27f == 4 | Q27f == 5 ~ 1),
         Q27g_bin_num = case_when(Q27g == 1 | Q27g == 2 ~ 0,
                              Q27g == 3 | Q27g == 4 | Q27g == 5 ~ 1),
         Q27h_bin_num = case_when(Q27h == 1 | Q27h == 2 ~ 0,
                              Q27h == 3 | Q27h == 4 | Q27h == 5 ~ 1),
         Q27i_bin_num = case_when(Q27i == 1 | Q27i == 2 ~ 0,
                              Q27i == 3 | Q27i == 4 | Q27i == 5 ~ 1))

climate_df %>% count(Q29)

climate_df %>% 
  filter(Q29 == 4) %>% 
  ggplot(aes(x = Q28_social_norm)) +
  geom_histogram() +
  theme_bw() +
  labs(x = "Global warming is a 'social' issue normalized score",
       y = "Number of students") +
  ylim(0, 150)



civil_27_bin_df <- civil_df %>% 
  select(Q27_bin_num_vars)


#hdbscan without dim reduction

hdbscan_cl <- hdbscan(civil_27_bin_df, minPts = 15)
hdbscan_cl



# hdbscan with dim reduction using UMAP

# Step 1: dim reduction with UMAP from nine dimensions to two dimensions

civil_umap <- umap(civil_27_bin_df)
civil_umap
names(civil_umap$layout)
names(civil_umap$layout) <- c("dim1", "dim2")

head(civil_umap$layout, 3)
head(civil_umap$layout[,1])
head(civil_umap$layout[,2])

# visualize the reduction 
plot(civil_umap$layout[,1], civil_umap$layout[,2])

plot_df <- civil_umap$layout
plot_df <- as_tibble(plot_df)
names(plot_df) <- c("dim1", "dim2")
ggplot(data = plot_df, aes(x = dim1, y = dim2)) +
  geom_point()


# perform clustering using HDBSCAN on the dim-reduced data

hdbscan_umap_cl <- hdbscan(plot_df, minPts = 15)
hdbscan_umap_cl
#hdbscan_umap_cl$cluster

# add the cluster labels to the plotting dataframe
plot_df$cluster <- hdbscan_umap_cl$cluster

# plot the reduction with the clusters
plot_df %>% 
  ggplot(aes(x = dim1, y = dim2, color = as.factor(cluster))) +
  geom_point() +
  labs(x = "UMAP projection dimension 1", 
       y = "UMAP projection dimension 2",
       color = "Cluster",
       title = "Two-dimensional projection of simplified Q27 using UMAP") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))


# add the cluster labels back to the original data set
civil_27_bin_df$cluster <- hdbscan_umap_cl$cluster

civil_27_bin_df %>% 
  ggplot(aes(x = cluster)) +
  geom_histogram() +
  labs(x = "Cluster",
       y = "Count",
       title = "Histogram of clusters for simplified Question 27") +
  theme_bw()




####
#
# Clustering using unsimplified question 27 (values 1-5) rather than simplified question 27 (binary values - sooner (0) or later (1))
#
####


civil_27_df <- civil_df %>% 
  select(Q27_vars)

# clustering on the unreduced data (pretty noisy)
hdbscan_cl <- hdbscan(civil_27_df, minPts = 15)
hdbscan_cl


# perform dimension reduction and then try clustering

# Step 1: dimension reduction from 9 dimensions to 2 dimensions using umap

civil_umap <- umap(civil_27_df)
civil_umap
#names(civil_umap$layout)
names(civil_umap$layout) <- c("dim1", "dim2")

head(civil_umap$layout, 3)
head(civil_umap$layout[,1])
head(civil_umap$layout[,2])

# visualized the reduction
plot(civil_umap$layout[,1], civil_umap$layout[,2])

plot_df <- civil_umap$layout
plot_df <- as_tibble(plot_df)
names(plot_df) <- c("dim1", "dim2")
ggplot(data = plot_df, aes(x = dim1, y = dim2)) +
  geom_point()


hdbscan_umap_cl <- hdbscan(plot_df, minPts = 30)
hdbscan_umap_cl
#hdbscan_umap_cl$cluster


plot_df$cluster <- hdbscan_umap_cl$cluster

plot_df %>% 
  ggplot(aes(x = dim1, y = dim2, color = as.factor(cluster))) +
  geom_point() +
  labs(x = "UMAP projection dimension 1", 
       y = "UMAP projection dimension 2",
       color = "Cluster",
       title = "Two-dimensional projection of unsimplified Q27 using UMAP") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) 
  


civil_27_df$cluster <- hdbscan_umap_cl$cluster




####
#
# 
### Attempt 3 - using three-way split (Now or 10 years = 0, 25 or 50 yrs = 1, Never = 2) #####
#
#
####



civil_df <- civil_df %>% 
  mutate(Q27a_tri_num = case_when(Q27a == 1 | Q27a == 2 ~ 0,
                                  Q27a == 3 | Q27a == 4 ~1, 
                                  Q27a == 5 ~ 2),
         Q27b_tri_num = case_when(Q27b == 1 | Q27b == 2 ~ 0,
                                  Q27b == 3 | Q27b == 4 ~1,
                                  Q27b == 5 ~ 2),
         Q27c_tri_num = case_when(Q27c == 1 | Q27c == 2 ~ 0,
                                  Q27c == 3 | Q27c == 4 ~ 1,
                                  Q27c == 5 ~ 2),
         Q27d_tri_num = case_when(Q27d == 1 | Q27d == 2 ~ 0,
                                  Q27d == 3 | Q27d == 4 ~ 1,
                                  Q27d == 5 ~ 2),
         Q27e_tri_num = case_when(Q27e == 1 | Q27e == 2 ~ 0,
                                  Q27e == 3 | Q27e == 4 ~ 1,
                                  Q27e == 5 ~ 2),
         Q27f_tri_num = case_when(Q27f == 1 | Q27f == 2 ~ 0,
                                  Q27f == 3 | Q27f == 4 ~ 1,
                                  Q27f == 5 ~ 2),
         Q27g_tri_num = case_when(Q27g == 1 | Q27g == 2 ~ 0,
                                  Q27g == 3 | Q27g == 4 ~ 1,
                                  Q27g == 5 ~ 2),
         Q27h_tri_num = case_when(Q27h == 1 | Q27h == 2 ~ 0,
                                  Q27h == 3 | Q27h == 4 ~ 1,
                                  Q27h == 5 ~ 2),
         Q27i_tri_num = case_when(Q27i == 1 | Q27i == 2 ~ 0,
                                  Q27i == 3 | Q27i == 4 ~ 1,
                                  Q27i == 5 ~ 2))




Q27_tri_num_vars <- paste0("Q27", letters[1:9], "_tri_num")


civil_27_tri_df <- civil_df %>% 
  select(Q27_tri_num_vars)


#hdbscan without dim reduction

hdbscan_cl <- hdbscan(civil_27_tri_df, minPts = 15)
hdbscan_cl



# hdbscan with dim reduction using UMAP

# Step 1: dim reduction with UMAP from nine dimensions to two dimensions

civil_umap <- umap(civil_27_tri_df)
civil_umap
names(civil_umap$layout)
names(civil_umap$layout) <- c("dim1", "dim2")

head(civil_umap$layout, 3)
head(civil_umap$layout[,1])
head(civil_umap$layout[,2])

# visualize the reduction 
plot(civil_umap$layout[,1], civil_umap$layout[,2])

plot_df <- civil_umap$layout
plot_df <- as_tibble(plot_df)
names(plot_df) <- c("dim1", "dim2")
ggplot(data = plot_df, aes(x = dim1, y = dim2)) +
  geom_point()


# perform clustering using HDBSCAN on the dim-reduced data

hdbscan_umap_cl <- hdbscan(plot_df, minPts = 25)
hdbscan_umap_cl
#hdbscan_umap_cl$cluster

# add the cluster labels to the plotting dataframe
plot_df$cluster <- hdbscan_umap_cl$cluster

# plot the reduction with the clusters
plot_df %>% 
  ggplot(aes(x = dim1, y = dim2, color = as.factor(cluster))) +
  geom_point() +
  labs(x = "UMAP projection dimension 1", 
       y = "UMAP projection dimension 2",
       color = "Cluster",
       title = "Two-dimensional projection of simplified Q27 using UMAP") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))


# add the cluster labels back to the original data set
civil_27_tri_df$cluster <- hdbscan_umap_cl$cluster

civil_27_tri_df %>% 
  ggplot(aes(x = cluster)) +
  geom_histogram() +
  labs(x = "Cluster",
       y = "Count",
       title = "Histogram of clusters for Question 27 trinary split") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))













### Q5d distributions by major
climate_data %>% 
  ggplot(aes(x = major, fill = as_factor(Q5d))) +
  geom_bar(position = "fill") +
  labs(title = "Question 5d responses by major",
       x = "Q29 - Major",
       y = "Proportion") +
  theme(plot.title = element_text(hjust = 0.5))



### Q5d distributions by religion
climate_data %>% 
  ggplot(aes(x = as_factor(Q33), fill = as_factor(Q5d))) +
  geom_bar(position = "fill") +
  labs(title = "Question 5d responses by religion",
       x = "Q33 - Religion",
       y = "Proportion") +
  theme(plot.title = element_text(hjust = 0.5))








### Making map of survey respondents in US -------



# choropleth for zip codes
library(choroplethr)
climate_df_zip <- climate_data %>% count(Q40)
climate_df_zip %>% arrange(desc(n))

climate_df_zip_filt <- climate_df_zip %>% 
  drop_na(Q40)
climate_df_zip <- climate_df_zip %>% 
  rename(zip = Q40, value = n) %>% 
  mutate(zip = as.character(zip))

climate_df_zip_filt <- climate_df_zip %>% 
  filter(zip != "NA")
climate_df_zip_filt %>% arrange(desc(value))

sum(climate_df_zip$value) # need to remove 900 NA values
sum(climate_df_zip_filt$value)

str(climate_df_zip)
?zip.map
str(zip.map)

zips_df <- read_csv("C:/Users/akatz4/Downloads/uszips.csv")

zips_df <- zips_df %>% 
  select(zip, state_id, state_name, lat, lng)
#  mutate(zip = as.numeric(zip))

str(zips_df)

climate_df_zip_state <- climate_df_zip_filt %>% inner_join(zips_df, by = "zip")


climate_df_zip_full <- zips_df %>% left_join(climate_df_zip_filt)
climate_df_zip_full$value <- replace_na(climate_df_zip_full$value, 0)


climate_df_zip_full <- climate_df_zip_full %>%
  select(zip, value) %>% 
  rename(region = zip)
  

library(choroplethrZip)
zip_choropleth(climate_df_zip_filt)

zip_choropleth(climate_df_zip_full)


data(zip_codes)




### Bubble plot maps -----

library(maps)
library(ggmap)


states <- map_data("state")
data <- climate_df_zip_filt %>% inner_join(zips_df, by = "zip")



# check on number of students from 
# Alaska
data %>% filter(state_id == "AK") # 7 students
#Hawaii
data %>% filter(state_id == "HI") # 8 students

filter_states <- c("AK", "HI")

data <- data %>% filter(!state_id %in% filter_states)

state_data_2 <- data(statemap)

str(states)


# map without state borders, gray background
ggplot() +
  geom_polygon(data = states, aes(x=long, y = lat, group = group), alpha=0.3) +
  geom_point( data=data, aes(x=lng, y=lat, size=value, color=value)) +
  scale_size_continuous(range=c(1,8)) +
  coord_map() +
  theme(legend.position = "none")


# bubble plot with colored states, weird line in Washington
ggplot() +
  geom_polygon(data = states, aes(x=long, y = lat, fill = region), alpha=0.3) +
  geom_path(data = states, aes(x=long, y = lat, group = group), alpha=0.3) +
  geom_point( data=data, aes(x=lng, y=lat, size=value, color=value)) +
  scale_size_continuous(range=c(1,8)) +
  coord_map() +
  theme_minimal() +
  theme(legend.position = "none")


# bubble plot with no state color fill in
ggplot() +
  geom_path(data = states, aes(x=long, y = lat, group = group), alpha=0.3) +
  geom_point( data=data, aes(x=lng, y=lat, size=value, color=value)) +
  scale_size_continuous(range=c(1,8)) +
  coord_map() +
  theme_minimal() +
  theme(legend.position = "none")



# this code works for creating map without colored states

library(usmap)

t_data <- data %>% 
  select(lng, lat, value, state_id)

transformed_data <- usmap_transform(t_data)

plot_usmap("states") +
  geom_point(data = transformed_data,
             aes(x = lng.1, y = lat.1, size = value, color = value)) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size = 14)) +
  labs(title = "Student Survey Respondent Hometowns")
  





# third method - nothing works
ggplot(data = states, aes(map_id = states))

ggplot() +
  geom_map(data=states, map = states,
           aes(long, lat))


ggplot(data, aes(map_id = state_id)) +
  geom_map(aes(fill = value), map = states)

ggplot(data = states, aes(x = long, y = lat, group = group, fill = region)) + 
  geom_polygon(color = "gray90", size = 0.1) +
  theme(legend.position = "none")




# BEST MAP METHOD - fourth method: this code works for creating map colored by state ----

library(RColorBrewer)

mycolors <- colorRampPalette(brewer.pal(9, "Pastel1"))(49)

p <- ggplot(data = states,
            mapping = aes(x = long, y = lat,
                          group = group, fill = region))

p + geom_polygon(color = "gray90", size = 0.1) +
  geom_point(data=data, aes(x=lng, y=lat, size=value, color=value), inherit.aes = FALSE) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  guides(fill = FALSE) +
  theme_minimal() +
  theme(legend.position = "none",
  axis.text.x = element_blank(),
  axis.text.y = element_blank(),
  axis.ticks = element_blank(),
  plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = mycolors) +
  labs(x = "",
       y = "",
       title = "Survey Respondent Hometown Bubble Map")




# Make map for civil engineering students ----


climate_df_zip <- climate_data %>% 
  filter(Q29 == 4) %>% 
  count(Q40)



climate_df_zip_filt <- climate_df_zip %>% 
  drop_na(Q40)
climate_df_zip <- climate_df_zip %>% 
  rename(zip = Q40, value = n) %>% 
  mutate(zip = as.character(zip))




climate_df_zip_filt <- climate_df_zip %>% 
  filter(zip != "NA")
climate_df_zip_filt %>% arrange(desc(value))





states <- map_data("state")
data <- climate_df_zip_filt %>% inner_join(zips_df, by = "zip")



filter_states <- c("AK", "HI")

data <- data %>% filter(!state_id %in% filter_states)





zips_df <- read_csv("C:/Users/akatz4/Downloads/uszips.csv")

zips_df <- zips_df %>% 
  select(zip, state_id, state_name, lat, lng)




states <- map_data("state")
data <- climate_df_zip_filt %>% inner_join(zips_df, by = "zip")








library(RColorBrewer)

mycolors <- colorRampPalette(brewer.pal(9, "Pastel1"))(49)

p <- ggplot(data = states,
            mapping = aes(x = long, y = lat,
                          group = group, fill = region))

p + geom_polygon(color = "gray90", size = 0.1) +
  geom_point(data=data, aes(x=lng, y=lat, size=value, color=value), inherit.aes = FALSE) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  guides(fill = FALSE) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = mycolors) +
  labs(x = "",
       y = "",
       title = "Civil Engineering Student Survey Respondent Hometown Bubble Map")









#### Adding state and IPEDS info for schools -----
setwd("G:/My Drive/AK Faculty/Research/Projects/project students and climate change")
climate_data <- read_csv("updated_climate_df_2.csv")

climate_data %>% count(School) %>% view()

climate_data_2 <- climate_data %>% 
  mutate(School = case_when(School == "Embry-Riddle Aeronautical University-Prescot" ~ "Embry-Riddle Aeronautical University-Prescott",
                            School == "California State University, Chico" ~ "California State University-Chico",
                            School == "California State University, East Bay" ~ "California State University-East Bay",
                            School == "California State University, Fresno" ~ "California State University-Fresno",
                            School == "California State University, Northridge" ~ "California State University-Northridge",
                            School == "Florida A&M University" ~ "Florida Agricultural and Mechanical University",
                            School == "Southern Illinois University Edwardsville" ~ "Southern Illinois University-Edwardsville",
                            School == "Arizona State University" ~ "Arizona State University-Tempe",
                            School == "The City College of New York" ~ "CUNY City College",
                            School == "New Mexico State University" ~ "New Mexico State University-Main Campus",
                            School == "St. Ambrose University" ~ "Saint Ambrose University",
                            School == "Rutgers, the State University of New Jersey" ~ "Rutgers University-New Brunswick",
                            School == "Rutgers University" ~ "Rutgers University-New Brunswick",
                            School == "University of Maryland Baltimore County" ~ "California State University-Chico",
                            School == "University of Maryland Baltimore County" ~ "University of Maryland-Baltimore County",
                            School == "University of California Davis" ~ "University of California-Davis",
                            School == "University of Cincinnati" ~ "University of Cincinnati-Main Campus",
                            School == "Cincinnati University" ~ "University of Cincinnati-Main Campus",
                            School == "University of Conneticut" ~ "University of Connecticut",
                            School == "University of Massachusetts, Amherst" ~ "University of Massachusetts-Amherst",
                            School == "University of Massachusets, Amherst" ~ "University of Massachusetts-Amherst",
                            School == "University of Massachussets, Amherst" ~ "University of Massachusetts-Amherst",
                            School == "University of Massachusetts" ~ "University of Massachusetts-Amherst",
                            School == "University of Nevada, Reno" ~ "University of Nevada-Reno",
                            School == "University of Tennessee, Knoxville" ~ "The University of Tennessee-Knoxville",
                            School == "University of Tennessee" ~ "The University of Tennessee-Knoxville",
                            School == "University of Tennessee, Chattanooga" ~ "The University of Tennessee-Chattanooga",
                            School == "Virginia Tech" ~ "Virginia Polytechnic Institute and State University",
                            School == "Virgina Tech" ~ "Virginia Polytechnic Institute and State University",
                            School == "Bob Jones Univeristy" ~ "Bob Jones University",
                            School == "New Mexico Tech" ~ "New Mexico Institute of Mining and Technology",
                            School == "Penn State" ~ "Pennsylvania State University-Main Campus",
                            School == "Southern Illinois University" ~ "Southern Illinois University-Edwardsville",
                            School == "Texas A&M University" ~ "Texas A & M University-College Station",
                            School == "Tulane University" ~ "Tulane University of Louisiana",
                            School == "Minnesota State University" ~ "Minnesota State University-Mankato",
                            School == "The Cooper Union" ~ "Cooper Union for the Advancement of Science and Art",
                            School == "University of Alabama, Huntsville" ~ "University of Alabama in Huntsville",
                            School == "University of California, Santa Cruz" ~ "University of California-Santa Cruz",
                            School == "University of Colorado at Colorado Springs" ~ "University of Colorado Colorado Springs",
                            School == "University of Missouri, Columbia" ~ "University of Missouri-Columbia",
                            School == "University of Missouri" ~ "University of Missouri-Columbia",
                            School == "University of Texas, Arlington" ~ "The University of Texas at Arlington",
                            School == "University of Virginia, Wise" ~ "The University of Virginia's College at Wise",
                            School == "State University of New York" ~ "SUNY College of Environmental Science and Forestry",
                            School == "University of Puerto Rico" ~ "Universidad Politecnica de Puerto Rico",
                            TRUE ~ School))



climate_data_2 %>% count(School) %>% view()



original_school_list <- climate_data %>% distinct(School)

original_school_list

new_school_list <- climate_data_2 %>% distinct(School)

new_school_list

setwd("G:/My Drive/AK Faculty/Research/IDEEAS Lab/projects/project ethics courses in engineering curricula/data/")
list.files()

school_info <- read_csv("ipeds_inst_char_2018.csv")

#school_info <- school_info %>% select(School, `IPEDS UnitID`, State, `Sort Key`)

school_info <- school_info %>% distinct(INSTNM, .keep_all = TRUE)

test_join <- climate_data_2 %>% inner_join(school_info, by = c("School" = "INSTNM"))

test_join %>% count(School) %>% view()

anti_join(new_school_list, test_join %>% distinct(School), by = "School")


climate_data_2 %>% filter(School == "University of Tennessee") %>% select(Q29) %>% view()


write_csv(test_join, "climate_data_and_ipeds.csv")

























#packageurl <- "http://cran.r-project.org/src/contrib/Archive/rstan/rstan_2.19.3.tar.gz"
#install.packages(packageurl, repos = NULL, type = "source")



#### Bayesian analysis
library(brms)
library(rstanarm)
library(here)

mod_robust <- brm(
  bf(Q23_bin_total ~ Q29_binned, sigma ~ Q29_binned),
  family = student,
  data = climate_df
#  file = here::here("brms_test")
)
mod_robust
#example(stan_model, package = "rstan", run.dontrun = TRUE)

library(rstanarm)




post1 <- stan_aov(Q23_bin_total ~ major, data = climate_df, 
                  prior = R2(location = 0.3), adapt_delta = 0.999,
                  seed = 12345)
post1
