library(tidyverse)
library(here)
#library(xgboost)
library(qwraps2)
library(caret)
#library(Ckmeans.1d.dp)

DATA_ROUND <- 'round1'
OUTPUT_DIR_FIGS <- here("output", "figures", DATA_ROUND, "cluster_features")
OUTPUT_DIR_DATA <- here("output", "data", DATA_ROUND, "cluster_features")
if(!dir.exists(OUTPUT_DIR_FIGS)) {dir.create(OUTPUT_DIR_FIGS)}
if(!dir.exists(OUTPUT_DIR_DATA)) {dir.create(OUTPUT_DIR_DATA)}

gather_participant_data <- function() {
  df <- left_join(read_csv(here("./output/data", DATA_ROUND, "items_clusters.csv")),
            read_tsv(here("./output/data", DATA_ROUND, "cleaned_demo_behavioral.tsv")),
                     by = 'qualtrics_id') %>%
    distinct(participant_id, .keep_all=T) %>%
    drop_na(cluster, High, Low, span.sentence.count)
  return(df)
}
(participants <- gather_participant_data())

gather_mediadiet <- function(qualtrics_ids) {
  df <- read_tsv(here("./output/data", DATA_ROUND, "media_diet.tsv"),
                 col_types=cols_only(
                   qualtrics_id=col_character(),
                   media_type=col_character(),
                   name=col_character(),
                   media_type=col_character()
                   )
                 ) %>%
    filter(qualtrics_id %in% qualtrics_ids) %>%
    mutate(name=gsub(':', '', name)) %>%
    mutate(full_name=paste(name, media_type, sep=':'))
  print(df)
  
  df %>%
    #group_by(name) %>%
    group_by(full_name) %>%
    filter(n() >= 2) %>%
    ungroup() %>%
    count(qualtrics_id, full_name) %>%
    #filter(n >= 1) %>%
    mutate(n=if_else(n>0, 1, 0)) %>%
    pivot_wider(
      id_cols='qualtrics_id',
      names_from=full_name,
      values_from=n,
      values_fill=0
    ) #%>%
    #janitor::clean_names()
}
mediadiet <- gather_mediadiet(qualtrics_ids=participants$qualtrics_id)
mediadiet

clusters_with_mediadiet <- participants %>% 
  group_by(cluster) %>%
  filter(n() >= 5) %>%
  ungroup() %>%
  select(qualtrics_id, cluster) %>%
  left_join(mediadiet, by='qualtrics_id')
clusters_with_mediadiet[is.na(clusters_with_mediadiet)] <- 0

unique_clusters <- clusters_with_mediadiet %>%
  pull(cluster) %>%
  unique()

features <- clusters_with_mediadiet %>%
  select(-qualtrics_id, -cluster) %>%
  as.matrix()

cluster_feat_df <- data.frame(
  cluster_num=integer(),
  feature_name=character(),
  f1=numeric(),
  ppv=numeric(),
  sensitivity=numeric()
)

for(cluster_num in unique_clusters) {
  clusters <- as.integer(clusters_with_mediadiet$cluster == cluster_num)
  class_val <- factor(clusters)
  
  print(paste("Cluster Num", cluster_num))
  for(i in 1:ncol(features)) {
    
    feature_val <- features[,i]
    feat_name <- colnames(features)[i]
    a <- confusion_matrix(x=factor(feature_val), y=class_val, positive='1')
    
    f1 <- a$stats['F1','Est']
    precision <- a$stats['PPV','Est']
    recall <- a$stats['Sensitivity','Est']
    cluster_feat_df <- rbind(cluster_feat_df, 
                         data.frame(
                           cluster=cluster_num,
                           feature_name=feat_name,
                           f1=f1,
                           ppv=precision,
                           sensitivity=recall
                         )
    )
  }
}

cluster_feat_df %>%
  as_tibble() %>%
  separate(feature_name, into=c("media_name", "media_type"), sep=':') %>%
  left_join(participants %>%
              count(cluster), by='cluster') %>%
  mutate(cluster_str=paste("Cluster ", cluster, " (N=", n, ")", sep='')) %>%
  group_by(cluster_str) %>%
  filter(f1 > 0) %>%
  arrange(cluster, -f1) %>%
  mutate(feature_name=factor(media_name, levels=unique(media_name))) %>%
  ungroup() -> all_feature_summary 

all_feature_summary %>%
  group_by(cluster, n) %>%
  mutate(num_items_per_cluster=n()) %>%
  ungroup() %>%
  group_by(cluster, n, num_items_per_cluster, media_type) %>%
  summarize(media_type_count=n())  %>%
    mutate(media_type_prop=media_type_count/num_items_per_cluster,
           media_type_fmt=sprintf("%1.0f%%", 100*media_type_prop)) %>%
  add_column(value_type="All") %>%
  ungroup() %>%
  select(cluster, media_type, media_type_fmt, value_type) -> all_feature_summary_long

all_feature_summary %>%
  filter(f1>0.1) %>%
  group_by(cluster, n) %>%
  mutate(num_items_per_cluster=n()) %>%
  ungroup() %>%
  group_by(cluster, n, num_items_per_cluster, media_type) %>%
  summarize(media_type_count=n())  %>%
    mutate(media_type_prop=media_type_count/num_items_per_cluster,
           media_type_fmt=sprintf("%1.0f%%", 100*media_type_prop)) %>%
  add_column(value_type="F1_Thresh") %>%
  ungroup() %>%
  select(cluster, media_type, media_type_fmt, value_type) -> f1_thresh_feature_summary_long
all_feature_summary %>%
  group_by(cluster, n) %>%
  slice_max(order_by=f1, n=10, with_ties=FALSE) %>%
  mutate(num_items_per_cluster=n()) %>%
  ungroup() %>%
  group_by(cluster, n, num_items_per_cluster, media_type) %>%
  summarize(media_type_count=n())  %>%
    mutate(media_type_prop=media_type_count/num_items_per_cluster,
           media_type_fmt=media_type_count) %>% #sprintf("%1.0f%%", 100*media_type_prop)) %>%
  add_column(value_type="top_20") %>%
  ungroup() %>%
  select(cluster, media_type, media_type_fmt, value_type) -> top_20_feature_summary_long

library(ggrepel)



recode_for_viz <- list(
  `Harry Potter and the Sorcerer's Stone`="Harry Potter 1",
  `Harry Potter and the Chamber of Secrets`="Harry Potter 2",
  `Harry Potter and the Prisoner of Azkaban`="Harry Potter 3",
  `Harry Potter and the Goblet of Fire`="Harry Potter 4",
  `Harry Potter and the Deathly Hallows Part 1`="Harry Potter 7",
  `Harry Potter and the Deathly Hallows Part 2`="Harry Potter 7"
)

num_clusters <- length(unique(all_feature_summary$cluster))
all_feature_summary %>%
  group_by(cluster, n) %>%
  slice_max(order_by=f1, n=10, with_ties=FALSE) %>%
  mutate(media_name=recode(media_name, !!!recode_for_viz)) %>%
  ggplot(aes(x=ppv, y=sensitivity, label=media_name)) +
    geom_point() +
    geom_text_repel(size=3, force=5) + #aes(alpha=f1/max(f1))) +
    facet_wrap(~cluster_str, ncol=2, scales = 'free') +
  labs(x='Proportion of Item Positives Occurring in Cluster (Precision)', 
       y="Proportion of Cluster Members Reporting Item (Recall)") +
  theme_bw() +
  ggsave(file.path(output_dir_figures, "cluster_feature_importance.png"),
         width=7, height=9, units = 'in')
  #add_column(idx=rep(1:10, times=8)) %>%
  #mutate(media_name_fmt=sprintf("%s (%0.2f)", media_name, f1)) %>%
  #pivot_wider(id_cols=idx, names_from=cluster_str, values_from=media_name_fmt) %>%
  #select(-idx) -> name_summary_df

name_summary_df

name_summary_df[, 1:4] %>%
  xtable::xtable() %>%
  print(include.rownames=FALSE)

name_summary_df[, 5:8] %>%
  xtable::xtable() %>%
  print(include.rownames=FALSE)

top_20_feature_summary_long %>%
  right_join(f1_thresh_feature_summary_long, by = c("cluster", "media_type")) %>%
  #right_join(all_feature_summary_long, by=c("cluster", "media_type"))
  replace_na(replace=list(media_type_fmt.x=0,
                          media_type_fmt.y='--')) %>%
  mutate(formatted_all_mediatype=paste(media_type_fmt.x, ' (',
                                       media_type_fmt.y, ')', sep='')) %>%
  select(cluster, media_type, formatted_all_mediatype) %>%
  pivot_wider(id_cols=media_type, 
              names_from='cluster', 
              values_from='formatted_all_mediatype', 
              values_fill='--/--') %>%
  xtable::xtable() %>%
  print(include.rownames=FALSE)

top_20_feature_summary_long %>%
  pivot_wider(id_cols=media_type, 
              names_from='cluster', 
              values_from='media_type_fmt', 
              values_fill=0) %>%
  xtable::xtable() %>%
  print(include.rownames=FALSE)

demo_summary <- list(
  "Schooling Level" =
    list(
      "HS or Below"= ~ qwraps2::n_perc(schooling_level_str %in% c("HS or GED", "Some HS")),
      "Some College"= ~ qwraps2::n_perc(schooling_level_str == 'Some college'),
      "College Degree"= ~qwraps2::n_perc(schooling_level_str %in% c("Associate Degree", "Bachelor's Degree")),
      "Advanced Degree"= ~qwraps2::n_perc(schooling_level_str %in% c("Master's Degree", "Professional/Doctorate"))
    ),
  "Age" = 
    list(
      "Median (Quartiles)"=~qwraps2::median_iqr(age, digits = 1),
      "Mean (SD)"=~mean_sd(age)
    )
)


  ggplot(aes(x=media_name, y=f1)) +
    geom_bar(stat='identity') +
    coord_flip() +
    facet_wrap(~cluster_str, ncol=2, scales='free')

# https://www.r-bloggers.com/2016/01/r-setup-a-grid-search-for-xgboost/
fit_xgb <- function(df, cluster_num) {
  
  #df <- clusters_with_mediadiet
  #cluster_num <- 7
  
  features <- df %>%
    select(-qualtrics_id, -cluster.id.items) %>%
    as.matrix()
  clusters <- as.integer(df$cluster.id.items == cluster_num)

  xgb_grid_1 = expand.grid(
    nrounds = 50,
    eta = c(0.01, 0.001, 0.0001),
    max_depth = c(2, 3, 4),
    gamma = c(0, 1), 
    colsample_bytree=c(0.3, 0.5, 0.7),
    min_child_weight=5,
    subsample=c(0.3, .5, 0.7)
  )
  xgb_trcontrol_1 = trainControl(
    method = "cv",
    number = 2,
    verboseIter = TRUE,
    returnData = FALSE,
    returnResamp = "all", # save losses across all models
    classProbs = TRUE, # set to TRUE for AUC to be computed
    summaryFunction = twoClassSummary,
    allowParallel = TRUE
  )
  
  y <- recode_factor(as.factor(clusters), `1`='positive', `0`='negative')
  xgb_train_1 <- train(
    x=features,
    y=y,
    trControl = xgb_trcontrol_1,
    metric="ROC",
    tuneGrid = xgb_grid_1,
    method="xgbTree"
  )
  xgb_train_1$finalModel
  vars_imps <- varImp(xgb_train_1)
  
  saveRDS(xgb_train_1, file=file.path(OUTPUT_DIR_DATA, paste('xgb_model_', cluster_num, '.RData', sep='')))
  feature_df <- vars_imps$importance %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var='var_name') %>%
    filter(Overall > 0) %>%
    arrange(Overall) %>%
    mutate(var_name=factor(var_name, levels=unique(var_name)))
  write_csv(feature_df, path=file.path(OUTPUT_DIR_DATA, paste(cluster_num, ".csv", sep='')))
  
  p <- ggplot(feature_df, 
              aes(x=var_name, y=Overall)) +
    geom_bar(stat='identity') +
    theme_minimal() +
    coord_flip()
  ggsave(filename = file.path(OUTPUT_DIR_FIGS, paste(cluster_num, ".png", sep='')), 
         plot = p)
}

unique_clusters

for(cluster_id in c(2)) {
  print(cluster_id)
  fit_xgb(clusters_with_mediadiet, cluster_id)
}

ggplot(top_coef, 
       aes(x=Feature, y=Gain)) +
geom_bar(stat='identity') +
coord_flip() +
facet_wrap(.~cluster_id, scales='free', ncol=3) +
  ggsave("./output/figures/round1/feature_importances_r1.png")
#importance_r2 <- fit_xgb(r2)

top_coef %>%
  group_by(Feature) %>%
  summarise(num_repeats=n(),
            avg_weight_gain=mean(Gain))
  write_csv("./output/data/round1/feature_importances_r1.csv")

print(importance_r1$plot)
ggsave("./output/figures/round1/feature_importances.png")
readr::write_csv(importance_r1$matrix, "./output/data/round1/feature_importances.csv")
print(importance_r2$plot)
ggsave("./output/figures/round2/feature_importances.png")
readr::write_csv(importance_r2$matrix, "./output/data/round2/feature_importances.csv")

