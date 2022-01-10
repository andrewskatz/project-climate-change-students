
library(tidyverse)
library(dbscan)
library(umap)
library(psych)




setwd("G:/My Drive/AK Faculty/Research/Projects/project students and climate change")

#climate_df <- read_csv("filtered_climate_survey_20200122.csv")
climate_df_2 <- read_csv("updated_climate_df_2.csv")

climate_df <- climate_df %>% rowid_to_column(var = "student_id")


Q27_vars <- paste0("Q27", letters[1:9])
Q28_vars <- paste0("Q28", letters[1:12])

####
#
#
# Clustering for question 27 (global warming will start to have a serious effect on...) ----
#
####


table(climate_df$Q29)
climate_df <- climate_df %>% drop_na(all_of(Q28_vars))
climate_df <- climate_df %>% drop_na(all_of(Q27_vars)) #should have 3313 observations



climate_df <- climate_df %>% 
  mutate(Q28_social = Q28c + Q28d + Q28e + Q28j + Q28l,
         Q28_tech = Q28a + Q28f + Q28g + Q28h + + Q28i + Q28k,
         Q28_social_norm = Q28_social / 5,
         Q28_tech_norm = Q28_tech / 6)



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
climate_df <- climate_df %>% drop_na(Q28_vars)
climate_df[, Q28_vars]
climate_df <- climate_df %>% drop_na(Q16_vars)




## Add in major as string instead of number

climate_df <- climate_df %>% 
  mutate(major = case_when(Q29 == 1 ~ "Aer/Oce",
                           Q29 == 2 ~ "Agr/Biol",
                           Q29 == 3 ~ "Bio",
                           Q29 == 4 ~ "Civ",
                           Q29 == 5 ~ "Che",
                           Q29 == 6 ~ "Con",
                           Q29 == 7 ~ "Comp",
                           Q29 == 8 ~ "Ele",
                           Q29 == 9 ~ "EngPhy",
                           Q29 == 10 ~ "Env/Eco",
                           Q29 == 11 ~ "Ind",
                           Q29 == 12 ~ "Mat",
                           Q29 == 13 ~ "Mec",
                           Q29 == 14 ~ "Min",
                           Q29 == 15 ~ "Nuc",
                           Q29 == 16 ~ "Softw",
                           Q29 == 17 ~ "Str/Arc",
                           Q29 == 18 ~ "Gen"))



# check on the counts of the majors for the remaining observations
climate_df %>% count(major, sort = TRUE)

cutoff <- 20

major_list <- climate_df %>% count(major, sort = TRUE) %>% filter(n > cutoff) %>% select(major)
climate_df %>% filter(major %in% major_list$major) %>% count(major, sort = TRUE)


## Plot Q28 distributions


# Histograms of Q28 distributions

climate_df %>% 
  filter(Q29 == 4) %>% 
  ggplot(aes(x = Q28_social_norm)) +
  geom_histogram() +
  theme_bw() +
  labs(x = "Global warming is a 'social' issue normalized score",
       y = "Number of students",
       title = "Student perceptions of Global Warming as a 'Social' Issue by Major") +
  ylim(0, 150)



climate_df %>% 
  filter(Q29 == 4) %>% 
  ggplot(aes(x = Q28_tech_norm)) +
  geom_histogram() +
  theme_bw() +
  labs(x = "Global warming is a 'technical' issue normalized score",
       y = "Number of students",
       title = "Student perceptions of Global Warming as a 'Technical' Issue by Major") +
  ylim(0, 150)




climate_df %>% 
  ggplot(aes(x = Q28_social_norm)) +
  geom_histogram() +
  facet_wrap(. ~ major) +
  theme_bw() +
  labs(x = "Global warming is a 'social' issue normalized score",
       y = "Number of students",
       title = "Student perceptions of Global Warming as a 'Social' Issue by Major") +
  ylim(0, 150)


climate_df %>% 
  ggplot(aes(x = Q28_tech_norm)) +
  geom_histogram() +
  facet_wrap(. ~ major) +
  theme_bw() +
  labs(x = "Global warming is a 'technical' issue normalized score",
       y = "Number of students",
       title = "Student perceptions of Global Warming as a 'Technical' Issue by Major") +
  ylim(0, 150)



# boxplots of Q28



climate_df %>% 
  filter(!is.na(major)) %>% 
  filter(major %in% major_list$major) %>% 
  ggplot(aes(x = Q28_social_norm, y = fct_reorder(major, Q28_social_norm))) +
  geom_boxplot() +
  theme_bw() +
  labs(x = "Global warming is a 'social' issue normalized score",
       y = "Number of students",
       title = "Student perceptions of Global Warming as a 'Social' Issue by Major") +
  theme(plot.title = element_text(hjust = 0.5))

climate_df %>% 
  filter(!is.na(major)) %>% 
  filter(major %in% major_list$major) %>% 
  ggplot(aes(x = Q28_tech_norm, y = fct_reorder(major, Q28_tech_norm))) +
  geom_boxplot() +
  theme_bw() +
  labs(x = "Global warming is a 'technical' issue normalized score",
       y = "Number of students",
       title = "Student perceptions of Global Warming as a 'Technical' Issue by Major") +
  theme(plot.title = element_text(hjust = 0.5))




# making binary variable values rather than character variable values

Q27_bin_num_vars <- paste0("Q27", letters[1:9], "_bin_num")
Q27_bin_num_vars

climate_df <- climate_df %>% 
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






civil_27_bin_df <- climate_df %>% 
  select(Q27_bin_num_vars)


#hdbscan without dim reduction

hdbscan_cl <- hdbscan(civil_27_bin_df %>% select(Q27a_bin_num, Q27b_bin_num, Q27c_bin_num), minPts = 15)
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


climate_27_df <- climate_df %>% 
  select(Q27_vars)

# clustering on the unreduced data (pretty noisy)
hdbscan_cl <- hdbscan(climate_27_df, minPts = 15)
hdbscan_cl


# perform dimension reduction and then try clustering

# Step 1: dimension reduction from 9 dimensions to 2 dimensions using umap

climate_27_df <- drop_na(climate_27_df)

climate_umap <- umap(climate_27_df)
climate_umap
#names(civil_umap$layout)
names(climate_umap$layout) <- c("dim1", "dim2")

head(climate_umap$layout, 3)
head(climate_umap$layout[,1])
head(climate_umap$layout[,2])

# visualized the reduction
plot(climate_umap$layout[,1], climate_umap$layout[,2])

plot_df <- climate_umap$layout
plot_df <- as_tibble(plot_df)
names(plot_df) <- c("dim1", "dim2")
ggplot(data = plot_df, aes(x = dim1, y = dim2)) +
  geom_point()


hdbscan_umap_cl <- hdbscan(plot_df, minPts = 60)
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



climate_27_df$cluster <- hdbscan_umap_cl$cluster




####
#
# 
### Attempt 3 - using three-way split (Now or 10 years = 0, 25 or 50 yrs = 1, Never = 2) #####
#
#
####

climate_df %>% drop_na(Q27a) %>% 
  ggplot(aes(x = Q27a)) + 
  geom_histogram()

climate_df <- climate_df %>% 
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

climate_df <- climate_df %>% 
  rowid_to_column(var = "student_id")

climate_27_tri_df <- climate_df %>% 
  select(student_id, Q5a, Q5b, Q5c, Q5d, Q5e, Q5f, Q5g, Q5h, Q5i, Q5j, Q27_tri_num_vars)




# hdbscan with dim reduction using UMAP

# step 0: hdbscan without dim reduction
hdbscan_cl <- hdbscan(climate_27_tri_df %>% select(Q27a_tri_num, Q27b_tri_num, Q27c_tri_num), minPts = 30)
hdbscan_cl #only two groups - not enough resolution

climate_27_tri_df$unreduced_cluster <- hdbscan_cl$cluster

climate_27_tri_df %>% filter(unreduced_cluster == 1) %>% view() #the 189 in cluster 1 said "never" for all categories

climate_27_tri_df %>% filter(unreduced_cluster == 2) %>% view()

# Step 1: dim reduction with UMAP from nine dimensions to two dimensions


climate_27_tri_df <- drop_na(climate_27_tri_df)

last_letter <- 9
Q27_tri_num_vars <- paste0("Q27", letters[1:last_letter], "_tri_num")

# try clustering on unreduced space
hdbscan_umap_cl <- climate_27_tri_df %>% select(Q27_tri_num_vars) %>% hdbscan(minPts = 40) 
hdbscan_umap_cl # not enough resolution - just breaks down into two groups


#Q27_tri_num_vars <- c("Q27a_tri_num", "Q27b_tri_num", "Q27c_tri_num")

climate_27_tri_umap <- climate_27_tri_df %>% select(Q27_tri_num_vars) %>% umap(random_state=123)
climate_27_tri_umap
names(climate_27_tri_umap$layout)
names(climate_27_tri_umap$layout) <- c("dim1", "dim2")

head(climate_27_tri_umap$layout, 3)
head(climate_27_tri_umap$layout[,1])
head(climate_27_tri_umap$layout[,2])

# visualize the reduction 
plot(climate_27_tri_umap$layout[,1], climate_27_tri_umap$layout[,2])

plot_df <- climate_27_tri_umap$layout
plot_df <- as_tibble(plot_df)
names(plot_df) <- c("dim1", "dim2")
ggplot(data = plot_df, aes(x = dim1, y = dim2)) +
  geom_point()


# perform clustering using HDBSCAN on the dim-reduced data

hdbscan_umap_cl <- hdbscan(plot_df, minPts = 230) 
# groups stay pretty similar from minPts 70 up to 150, when it drops from 9 to 6 clusters
# 230 drops down to 0-4 clusters (cluster 0 seems to be the "never" group)

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
climate_27_tri_df$cluster <- hdbscan_umap_cl$cluster

climate_27_tri_df %>% 
  ggplot(aes(x = cluster)) +
  geom_histogram() +
  labs(x = "Cluster",
       y = "Count",
       title = "Histogram of clusters for Question 27 trinary split") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))


climate_27_tri_df %>% filter(cluster == 0) %>% view()

climate_27_tri_df %>% filter(cluster == 7) %>% view()

climate_27_tri_df %>% filter(cluster == 2) %>% view()

climate_27_tri_df %>%
  pivot_longer(cols = Q27a_tri_num:Q27i_tri_num, names_to = "survey_item", values_to = "response") %>% 
  ggplot(aes(x = response)) +
  geom_histogram(aes(fill = as_factor(cluster))) +
  facet_wrap(.~ survey_item, scales = "free") +
  theme_bw() +
  labs(x = "Converted Ternary Response (0 = Now, 1 = Later, 2 = Never)",
       y = "Count",
       title = "Histogram of when will climate change affect different groups?",
       fill = "Cluster") +
  theme(plot.title = element_text(hjust = 0.5))
  


q27_item_list <- list(
  'Q27a_tri_num' = "Me personally",
  "Q27b_tri_num" = "My family",
  "Q27c_tri_num" = "People in my comm.",
  "Q27d_tri_num" = "People in US",
  "Q27e_tri_num" = "Other indust. countr.",
  "Q27f_tri_num" = "Developing countries",
  "Q27g_tri_num" = "Plant + animal species",
  "Q27h_tri_num" = "World's poor",
  "Q27i_tri_num" = "Natural environment"
)

q27_labs <- c("Me personally", 
              "My family",
              "People in my comm.",
              "People in US",
              "Other indust. countr.",
              "Developing countries",
              "Plant + animal species",
              "World's poor",
              "Natural environment")

names(q27_labs) <- c("Q27a_tri_num",
                     "Q27b_tri_num",
                     "Q27c_tri_num",
                     "Q27d_tri_num",
                     "Q27e_tri_num",
                     "Q27f_tri_num",
                     "Q27g_tri_num",
                     "Q27h_tri_num",
                     "Q27i_tri_num")


climate_27_tri_df %>%
  pivot_longer(cols = Q27a_tri_num:Q27i_tri_num, names_to = "survey_item", values_to = "response") %>% 
  ggplot(aes(x = as_factor(response))) +
  geom_bar(aes(fill = as_factor(cluster))) +
  facet_wrap(.~ survey_item, 
             scales = "free",
             labeller = labeller(survey_item = q27_labs)) +
  theme_bw() +
  labs(x = "Converted Ternary Response (0 = Now, 1 = Later, 2 = Never)",
       y = "Count",
       title = "Histogram of when will climate change affect different groups?",
       fill = "Cluster") +
  theme(plot.title = element_text(hjust = 0.5))













# perform clustering using HDBSCAN on the dim-reduced data

hdbscan_umap_cl_140pt <- hdbscan(plot_df, minPts = 140) 
# groups stay pretty similar from minPts 70 up to 150, when it drops from 9 to 6 clusters

hdbscan_umap_cl_140pt
#hdbscan_umap_cl$cluster

# add the cluster labels to the plotting dataframe
plot_df$cluster_7grp <- hdbscan_umap_cl_140pt$cluster

# plot the reduction with the clusters
plot_df %>% 
  ggplot(aes(x = dim1, y = dim2, color = as.factor(cluster_7grp))) +
  geom_point() +
  labs(x = "UMAP projection dimension 1", 
       y = "UMAP projection dimension 2",
       color = "Cluster",
       title = "Two-dimensional projection of simplified Q27 using UMAP") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))


# add the cluster labels back to the original data set
climate_27_tri_df$cluster_7grp <- hdbscan_umap_cl_140pt$cluster

climate_27_tri_df %>% 
  ggplot(aes(x = cluster_7grp)) +
  geom_histogram() +
  labs(x = "Cluster",
       y = "Count",
       title = "Histogram of clusters for Question 27 trinary split") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))


climate_27_tri_df %>% filter(cluster_7grp == 1) %>% view()

climate_27_tri_df %>% filter(cluster_7grp == 0) %>% view()

climate_27_tri_df %>% filter(cluster_7grp == 9) %>% view()


climate_27_tri_df %>%
  pivot_longer(cols = Q27a_tri_num:Q27i_tri_num, names_to = "survey_item", values_to = "response") %>% 
  ggplot(aes(x = response)) +
  geom_histogram(aes(fill = as_factor(cluster_7grp))) +
  facet_wrap(.~ survey_item, scales = "free") +
  theme_bw() +
  labs(x = "Converted Ternary Response (0 = Now, 1 = Later, 2 = Never)",
       y = "Count",
       title = "Histogram of when will climate change affect different groups?",
       fill = "Cluster") +
  theme(plot.title = element_text(hjust = 0.5))



q27_item_list <- list(
  'Q27a_tri_num' = "Me personally",
  "Q27b_tri_num" = "My family",
  "Q27c_tri_num" = "People in my comm.",
  "Q27d_tri_num" = "People in US",
  "Q27e_tri_num" = "Other indust. countr.",
  "Q27f_tri_num" = "Developing countries",
  "Q27g_tri_num" = "Plant + animal species",
  "Q27h_tri_num" = "World's poor",
  "Q27i_tri_num" = "Natural environment"
)

q27_labs <- c("Me personally", 
              "My family",
              "People in my comm.",
              "People in US",
              "Other indust. countr.",
              "Developing countries",
              "Plant + animal species",
              "World's poor",
              "Natural environment")

names(q27_labs) <- c("Q27a_tri_num",
                     "Q27b_tri_num",
                     "Q27c_tri_num",
                     "Q27d_tri_num",
                     "Q27e_tri_num",
                     "Q27f_tri_num",
                     "Q27g_tri_num",
                     "Q27h_tri_num",
                     "Q27i_tri_num")


climate_27_tri_df %>%
  pivot_longer(cols = Q27a_tri_num:Q27i_tri_num, names_to = "survey_item", values_to = "response") %>% 
  ggplot(aes(x = as_factor(response))) +
  geom_bar(aes(fill = as_factor(cluster_7grp))) +
  facet_wrap(.~ survey_item, 
             scales = "free",
             labeller = labeller(survey_item = q27_labs)) +
  theme_bw() +
  labs(x = "Converted Ternary Response (0 = Now, 1 = Later, 2 = Never)",
       y = "Count",
       title = "Histogram of when will climate change affect different groups?",
       fill = "Cluster") +
  theme(plot.title = element_text(hjust = 0.5))







climate_27_tri_df %>% 
  select(student_id, cluster_7grp, cluster_9grp) %>% 
  inner_join(climate_df, by = "student_id") %>% 
  write_csv("climate_df_with_clusters_20200715.csv")




#chi square tests for cluster and career topic interests
climate_df <- climate_27_tri_df %>% select(student_id, cluster) %>% inner_join(climate_df, by = "student_id")
climate_df %>% count(cluster, sort = TRUE)



# Q5a
cont_table_Q5a <- table(climate_df$cluster, climate_df$Q5a)
cont_table_Q5a
chisq.test(cont_table_Q5a)


mosaicplot(cont_table_Q5a, shade = TRUE, main = "Q27 Cluster vs Q5a (energy)")



# Q5d
cont_table_Q5d <- table(climate_df$cluster, climate_df$Q5d)
cont_table_Q5d
chisq.test(cont_table_Q5d)


mosaicplot(cont_table_Q5d, shade = TRUE, main = "Q27 Cluster vs Q5d (climate change)")


# Q5f
cont_table_Q5f <- table(climate_df$cluster, climate_df$Q5f)
cont_table_Q5f
chisq.test(cont_table_Q5f)

mosaicplot(cont_table_Q5f, shade = TRUE, main = "Q27 Cluster vs Q5f (water supply)")




# Q5j
cont_table_Q5j <- table(climate_df$cluster, climate_df$Q5j)
cont_table_Q5j
chisq.test(cont_table_Q5j)

mosaicplot(cont_table_Q5j, shade = TRUE, main = "Q27 Cluster vs Q5j (environmental degradation)")



#Q29 (major)

cont_table_Q29 <- table(climate_df$cluster, climate_df$Q29)
cont_table_Q29
chisq.test(cont_table_Q29)

mosaicplot(cont_table_Q29, shade = TRUE, main = "Q27 Cluster vs Q29 (environmental degradation)")






## Logistic regression with clusters as predictor----

climate_27_tri_df <- climate_27_tri_df %>% mutate(cluster = as.factor(cluster),
                                                  Q5d = as.factor(Q5d))

lr_mod <- glm(Q5d ~ cluster, data = climate_27_tri_df, family = binomial(link = 'logit'))
summary(lr_mod)

climate_27_tri_df %>% filter(cluster == 2) %>% view()
climate_27_tri_df %>% filter(cluster == 1) %>% view()




climate_27_tri_df %>% 
  ggplot(aes(x = cluster, fill = as_factor(Q5d))) +
  geom_bar(position = "fill") +
  labs(title = "Question 5d responses by cluster",
       x = "Cluster assignment",
       y = "Proportion") +
  theme(plot.title = element_text(hjust = 0.5))





climate_df %>% group_by(Q29) %>% 
  summarize(n = n(),
            total_Q5d = sum(Q5d)) %>% 
  mutate(freq = total_Q5d / n)



climate_df %>% 
  ggplot(aes(x = major, fill = factor(Q5d))) +
  geom_bar(position = "fill") +
  scale_fill_brewer(palette = "Set2") +
  labs(x = "Major", y = "Percent", fill = "Q5d", title = "Proportions of Q5d (interest in addressing climate change) by major")




# Q5 career topic interests for all ten topics in Q5 by major (or cluster)

career_interest_df <- climate_df  %>% 
  select(student_id, Q5a:Q5j, major, cluster) %>% pivot_longer(cols = Q5a:Q5j, names_to = "Q5_item", values_to = "Q5_resp")


climate_df <- climate_df %>% 
  mutate(Q5_total = Q5a + Q5b + Q5c + Q5d + Q5e + Q5f + Q5g + Q5h + Q5i + Q5j)

climate_df %>% 
  select(Q5a:Q5j, Q5_total, major) %>% 
  group_by(major) %>% 
  summarize(n = n(),
            avg_Q5_total = mean(Q5_total)) %>% 
  arrange(desc(avg_Q5_total))

climate_df %>% 
  ggplot(aes(x = Q5_total)) +
  geom_histogram() +
  facet_wrap(. ~ major) +
  labs(x = "Total Number of Q5 topics",
       y = "Count",
       title = "Number of Q5 topics identified by major") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
  


## Plot the career interest proportions broken down by topic and major

q5_labs <- c("Energy (supply/demand)", 
              "Disease",
              "Poverty and wealth dist.",
              "Climate change",
              "Terrorism and war",
              "Water supply",
              "Food availability",
              "Opp. for future gen",
              "Opp. for women and/or min.",
             "Environmental degradation")

names(q5_labs) <- c("Q5a",
                     "Q5b",
                     "Q5c",
                     "Q5d",
                     "Q5e",
                     "Q5f",
                     "Q5g",
                     "Q5h",
                     "Q5i",
                    "Q5j")



career_interest_df %>%   
  ggplot(aes(x = major, fill = factor(Q5_resp))) +
  geom_bar(position = "fill") +
  scale_fill_brewer(palette = "Set2") +
  facet_wrap(. ~ Q5_item, 
             scales = "free",
             labeller = labeller(Q5_item = q5_labs)) +
  labs(x = "Major", y = "Proportion", fill = "", title = "Proportions of Q5 topic by major") +
  coord_flip() +
  theme(plot.title = element_text(hjust = 0.5))


## repeat above but now for clusters instead of majors

climate_df %>% 
  select(Q5a:Q5j, Q5_total, cluster) %>% 
  group_by(cluster) %>% 
  summarize(n = n(),
            avg_Q5_total = mean(Q5_total)) %>% 
  arrange(desc(avg_Q5_total))



climate_df %>% 
  ggplot(aes(x = Q5_total)) +
  geom_histogram() +
  facet_wrap(. ~ cluster) +
  labs(x = "Total Number of Q5 topics",
       y = "Count",
       title = "Number of Q5 topics identified by cluster") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))



career_interest_df %>%   
  ggplot(aes(x = cluster, fill = factor(Q5_resp))) +
  geom_bar(position = "fill") +
  scale_fill_brewer(palette = "Set2") +
  facet_wrap(. ~ Q5_item, 
             scales = "free",
             labeller = labeller(Q5_item = q5_labs)) +
  labs(x = "Cluster", y = "Proportion", fill = "", title = "Proportions of Q5 topic by cluster") +
  coord_flip() +
  theme(plot.title = element_text(hjust = 0.5))




climate_df_2 %>% drop_na(Q29, Q32, Q37)

climate_df_2 %>% drop_na(Q29) %>% drop_na(Q27_vars)


climate_df_2 <- climate_df_2 %>% drop_na(Q29, Q18k, Q20a, Q20b, Q20d, Q18k, Q22) %>% 
  drop_na(Q23a, Q23b, Q23c, Q23d, Q23e, Q23f, Q23g, Q23h, Q23i, Q23j) %>% 
  drop_na(Q24a, Q24b, Q24c, Q24d, Q24e, Q24f, Q24g, Q24h, Q24i, Q24j, Q24k) %>% 
  drop_na(Q25a, Q25b, Q25c) %>% 
  drop_na(Q26a, Q26b, Q26c, Q26d, Q26e, Q26f, Q26g, Q26h) %>%
  drop_na(Q27_vars) %>% 
  drop_na(Q28_vars)

climate_df %>% count(Q29, sort = TRUE)







#####
####
### draw map of clusters ---
##
#

library(maps)

states <- map_data("state")

ggplot() +
  geom_polygon(data = states, aes(x=long, y = lat, group = group), fill = "grey", alpha = 0.3)



#### NOTE: climate_df should have the zip code and cluster assignments

## create dataframe with rankings for cluster by how soon each cluster believes climate change will affect groups
cluster_ranking_df <- climate_df %>% 
  mutate(Q27_tri_total = Q27a_tri_num + Q27b_tri_num + Q27c_tri_num + Q27d_tri_num + Q27e_tri_num +
           Q27f_tri_num + Q27g_tri_num + Q27h_tri_num + Q27i_tri_num) %>% 
  group_by(cluster) %>% 
  summarize(cluster_avg = mean(Q27_tri_total)) %>% 
  arrange(cluster_avg) %>% 
  rowid_to_column(var = "cluster_time_rank")

cluster_ranking_df

climate_df <- climate_df %>% 
  inner_join(cluster_ranking_df, by = "cluster")



climate_df %>% drop_na(Q40) # drops ~ 200 responses


climate_df_zip <- climate_df %>% 
  drop_na(Q40) %>% 
  count(Q40, cluster, cluster_time_rank) 


climate_df_zip %>% arrange(desc(n))


# climate_df_zip_filt <- climate_df_zip %>% 
#   drop_na(Q40)

climate_df_zip <- climate_df_zip %>% 
  rename(zip = Q40, value = n) %>% 
  mutate(zip = as.character(zip))

# make sure no residual na values for zip
climate_df_zip_filt <- climate_df_zip %>% 
  filter(zip != "NA")

climate_df_zip_filt %>% arrange(desc(value))

# quick sanity check on counts
sum(climate_df_zip$value) 
sum(climate_df_zip_filt$value)

str(climate_df_zip_filt)
# zip.map
# str(zip.map)


# old zip code csv file
#zips_df <- read_csv("C:/Users/akatz4/Downloads/uszips.csv") # this may have been missing some zips - DONT USE

#new zip code csv file from https://public.opendatasoft.com/explore/dataset/us-zip-code-latitude-and-longitude/export/
zips_df <- read_csv("C:/Users/akatz4/Downloads/zip_code_database.csv")


zips_df <- zips_df %>% 
  select(zip, state, latitude, longitude)
#  mutate(zip = as.numeric(zip))
zips_df <- zips_df %>% mutate(zip = str_remove(zip, "^0+"))


str(zips_df)

# something seems wrong here - dropped from 2136 to 1786
climate_df_zip_state <- climate_df_zip_filt %>% inner_join(zips_df, by = "zip")
sum(climate_df_zip_state$value) # sum is now 2499 -- much closer to the 2522

# see which zip codes are creating problems
climate_df_zip_filt %>% anti_join(zips_df, by = "zip")  # returns 23 zip codes -- they seem to be outsise US
missing_zips <- climate_df_zip_filt %>% anti_join(zips_df, by = "zip") # returns 350 zip codes
missing_zips
sum(missing_zips$value) # accounts for 373 students, which is the difference between 2149 and 2522
# this means that there are 350 missing zip codes from Q40 that are not in the zips_df
zips_df %>% arrange(zip)


climate_df_zip_full <- zips_df %>% left_join(climate_df_zip_filt)
climate_df_zip_full$value <- replace_na(climate_df_zip_full$value, 0)
climate_df_zip_full$cluster <- replace_na(climate_df_zip_full$cluster, 0)
sum(climate_df_zip_full$value) #2149, but should be 2522


climate_df_zip_full <- climate_df_zip_full %>%
  select(zip, value) %>% 
  rename(region = zip)





### Bubble plot maps -----

library(maps)
library(ggmap)


states <- map_data("state")

data <- climate_df_zip_filt %>% inner_join(zips_df, by = "zip")
sum(data$value) #has 2499 but should have 2522 (the missing 23 seem to be international)


# check on number of students from 
# Alaska
data %>% filter(state == "AK") # 4 students
#Hawaii
data %>% filter(state == "HI") # 5 students

filter_states <- c("AK", "HI")

data <- data %>% filter(!state %in% filter_states)
sum(data$value)

state_data_2 <- data(statemap)

str(states)



# BEST MAP METHOD - fourth method: this code works for creating map colored by state ----


library(RColorBrewer)

#color_pal <- colorRampPalette(c("red", "green"))

state_colors <- colorRampPalette(brewer.pal(9, "Pastel1"))(49)

#original method
cluster_num <- length(unique(climate_df$cluster))

cluster_colors <- colorRampPalette(brewer.pal(9, "Set1"))(cluster_num)

#new method
cluster_num <- length(unique(climate_df$cluster_time_rank))

cluster_colors <- colorRampPalette(c("#F11D08", "#2550CB"))(cluster_num)



p <- ggplot(data = states,
            mapping = aes(x = long, y = lat,
                          group = group, fill = region))

p + geom_polygon(color = "gray90", size = 0.1) +
  geom_point(data=data, 
             aes(x=longitude, y=latitude, size=value, color=as_factor(cluster_time_rank)),
             alpha = 0.5,
             inherit.aes = FALSE) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  guides(fill = FALSE) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = state_colors) +
  scale_color_manual(values = cluster_colors) +
  labs(x = "",
       y = "",
       title = "Survey Respondent Hometown Bubble Map",
       color = "Smaller Number = Sooner Effects",
       size = "Cluster size")




# Make map for civil engineering students ----


climate_df_zip <- climate_df %>% 
  filter(!is.na(Q40)) %>% 
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







climate_df %>% 
  ggplot(aes(x = as_factor(cluster_time_rank), y = Q7c_wt_sum)) + 
  geom_boxplot()

library(purrr)

Q7_wt_sum_vars <- paste0("Q7", letters[0:22], "_wt_sum")
Q7_wt_sum_vars[1]

test_plots <- purrr::map(.x = Q7_wt_sum_vars,
                          .f = ~  ggplot(data = climate_df, 
                                         aes(x = as_factor(cluster_time_rank), y = eval(as.name(.x)))) + 
                    geom_boxplot())

test_plots[8]





