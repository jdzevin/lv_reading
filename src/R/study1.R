library(reticulate)
library(hablar)
library(qwraps2)
library(here)

load_data <- function(data_round="round1", min_cluster_size=5) {
  
  output_dir_data <- here("output", "data", data_round)
  #output_dir_features <- file.path(output_dir_data, "features")
  #output_dir_figures <- here("output", "figures", DATA_ROUND)
  
  demo_df <- read_tsv(file.path(output_dir_data, "demographics_and_behavioral.tsv"),
                      col_types = cols(
                        political_scale_str = col_factor(levels=rev(c("Very Liberal", 
                                                                      "Moderately Liberal", 
                                                                      "Slightly Liberal", 
                                                                      "Neutral", 
                                                                      "Slightly Conservative", 
                                                                      "Moderately Conservative", 
                                                                      "Very Conservative"))),
                        schooling_level_str=col_factor(levels=c("Professional/Doctorate", 
                                                                "Master's Degree", 
                                                                "Bachelor's Degree", 
                                                                "Associate Degree",
                                                                "Some college", 
                                                                "HS or GED", 
                                                                "Some HS")),
                        political_interest=col_factor(levels=1:7)))
  
  full.participants <- demo_df %>%
    mutate(age.std=as.vector(scale(age)),
           ses.std=as.vector(scale(SES))) %>%
    mutate(political_interest=recode_factor(political_interest, `1`="Very Disinterested",
                                            `7`="Very Interested"),
           political_interest=factor(political_interest,
                                     levels=rev(c("Very Disinterested", 2, 3, 4, 5, 6, 
                                                  "Very Interested")))) %>%
    mutate(race.nih=replace(race.nih, race.nih == 'Void', NA),
           gender.clean=replace(gender.clean, gender.clean == 'Void', NA),
           SES=replace(SES, SES > 999, 999),
           SES=replace(SES, SES < 1, 1)) %>%
    tidyr::replace_na(list(ethnicity.nih="NR", 
                           race.nih="NR", 
                           gender.clean="NR", 
                           sex_or="NR")) %>%
    mutate(gender.clean=recode(gender.clean, male="Male", female="Female")) %>%
    mutate(age.discrete=cut_number(age, n=8),
           ses.discrete=cut_number(SES, n=8),
           age.discrete=factor(age.discrete, levels=rev(levels(age.discrete))),
           ses.discrete=factor(ses.discrete, levels=rev(levels(ses.discrete)))) %>%
    inner_join(read_csv(file.path(output_dir_data, "items_clusters.csv")), 
               by='qualtrics_id') %>%
    rename(cluster=`cluster.id.items`) %>%
    mutate(cluster=factor(cluster, levels=sort(unique(cluster)))) %>%
    dplyr::select(-date, -survey_version, -lived_cities, -sexual_orientation,
                  -race_ethnicity, -attention_check1, -attention_check2, 
                  -schooling_level, -gender, -list_languages, -political_scale)  %>%
    distinct(participant_id, .keep_all=TRUE)
  
  full.participants <- full.participants %>%
    mutate(High.perc=High/25,
           Low.perc=Low/25)
  
  dropped.participants <- full.participants %>%
    group_by(cluster) %>%
    filter(n() < min_cluster_size) %>%
    ungroup()
  
  participants <- full.participants %>%
    group_by(cluster) %>%
    filter(n() >= min_cluster_size) %>%
    ungroup() %>%
    droplevels
  
  map_clust_names <- list()
  uniq_clust <- as.vector(levels(participants$cluster))
  for(i in 0:(length(levels(participants$cluster))-1)) {
    x <- uniq_clust[i+1]
    map_clust_names[x] <- i
  }
  
  participants <- participants %>%
    mutate(cluster=recode_factor(cluster, !!!map_clust_names))
  
  participants %>%
    write_tsv(file.path(output_dir_data, "cleaned_demo_behavioral.tsv"))
  
  data_list <- list(
    "participants" = participants, 
    "full.participants" = full.participants, 
    "dropped.participants" = dropped.participants,
    "path" = file.path(output_dir_data, "cleaned_demo_behavioral.tsv"))
  
  return(data_list)
}

summarize_demographics <- function(participants, frmt_str="") {
  
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
      ),
    "Socio-Economic Status" = 
      list(
        "Median (Quartiles)"=~qwraps2::median_iqr(SES, digits = 1),
        "Mean (SD)"=~mean_sd(SES)
      ), 
    "Political Ideology" = 
      list(
        "Liberal"=~qwraps2::n_perc(political_scale_str %in% c("Slightly Liberal",
                                                              "Moderately Liberal",
                                                              "Very Liberal")),
        "Neutral"=~qwraps2::n_perc(political_scale_str=='Neutral'),
        "Conservative"=~qwraps2::n_perc(political_scale_str %in% c("Slightly Conservative",
                                                                   "Moderately Conservative",
                                                                   "Very Conservative"))
      ), 
    "Race" =
      list(
        "American Indian"=~qwraps2::n_perc(race.nih=="American Indian"),
        "Asian"=~qwraps2::n_perc(race.nih=="Asian"),
        "Black"=~qwraps2::n_perc(race.nih=="Black"),
        "White"=~qwraps2::n_perc(race.nih=="White"),
        "Multiple"=~qwraps2::n_perc(race.nih=="Multiple Races")
      ),
    "Ethnicity" =
      list(
        "Hispanic"=~qwraps2::n_perc(ethnicity.nih=="Hispanic"),
        "Non-Hispanic"=~qwraps2::n_perc(ethnicity.nih=="Not Hispanic")
      )
  )
  t <- summary_table(participants %>% group_by(subject_pool), demo_summary)
  
  return(t)
}


summarize_spin_and_span <- function(participants) {
  
  dv_summary <- list(
    "SPIN (High Predictability)" = 
      list(
        "Median (Quartiles)"=~qwraps2::median_iqr(High.perc, digits = 2),
        "Mean (SD)"=~mean_sd(High.perc)
      ),
    "SPIN (Low Predictability)" = 
      list(
        "Median (Quartiles)"=~qwraps2::median_iqr(Low.perc, digits = 2),
        "Mean (SD)"=~mean_sd(Low.perc)
        ),
    "SPAN (Correct Sentences)" = 
      list(
        "Median (Quartiles)"=~qwraps2::median_iqr(span.sentence.count, digits = 1),
        "Mean (SD)"=~mean_sd(span.sentence.count)
        )
  )
  t <- summary_table(participants %>% group_by(subject_pool), dv_summary)
  return(t)
}

preprocess_participants_for_plotting <- function(participants) {
  
  plt_df <- participants %>%
    mutate(schooling_level_str=recode(schooling_level_str,
                                      `HS or GED`="HS/GED",
                                      `Professional/Doctorate`="Professional, Doctorate")) %>%
    mutate_at(.vars=vars(political_scale_str, 
                         schooling_level_str,
                         political_interest), 
              list(~gsub(' ', '\n', .))) %>%
    mutate(political_scale_str=factor(political_scale_str,
                                      levels=rev(c("Very\nLiberal",
                                               "Moderately\nLiberal",
                                               "Slightly\nLiberal",
                                               "Neutral",
                                               "Slightly\nConservative",
                                               "Moderately\nConservative",
                                               "Very\nConservative"))),
          schooling_level_str=factor(schooling_level_str,
                                      levels=rev(c("Some\nHS",
                                               "HS/GED",
                                               "Some\ncollege",
                                               "Associate\nDegree",
                                               "Bachelor's\nDegree",
                                               "Master's\nDegree",
                                               "Professional,\nDoctorate"))),
          political_interest=factor(political_interest,
                                      levels=rev(c("Very\nDisinterested",
                                                   "2", "3", "4", "5", "6",
                                               "Very\nInterested"))))
  return(plt_df)
    
}

plot_demo_as_bar_charts <- function(plt_df, output_dir) {
  
  if(!dir.exists(output_dir)) {
    dir.create(output_dir)
  }
  print(paste("Writing plots to ", output_dir))
  
  generate_stacked_bar(plt_df, "subject_pool", 
                       "Subject Pool", 
                       file.path(output_dir, "subject_pool.png"), 
                       is_ordinal = F) 
  generate_stacked_bar(plt_df, "political_scale_str", 
                       "Political Scale", 
                       file.path(output_dir, "pol_scale.png"), 
                       is_ordinal = T) 
   
  generate_stacked_bar(plt_df, "political_interest", 
             "Political Interest", 
             file.path(output_dir, "pol_interest.png"), 
             is_ordinal = T)
  generate_stacked_bar(plt_df, "schooling_level_str", "Schooling Level", file.path(output_dir, "schooling.png"), is_ordinal = T)
  
  generate_stacked_bar(plt_df, "english_best", "English is\nPreferred Language", file.path(output_dir, "english_best.png"))
  generate_stacked_bar(plt_df, "race.nih", "Race (NIH)", file.path(output_dir, "race_nih.png"))
  generate_stacked_bar(plt_df, "ethnicity.nih", "Ethnicity (NIH)", file.path(output_dir, "ethnicity_nih.png"))
  generate_stacked_bar(plt_df, "sex_or", "Sexual Orientation", file.path(output_dir, "sex_orientation.png"))
  generate_stacked_bar(plt_df, "gender.clean", "Gender", file.path(output_dir, "gender.png"))
  
  generate_stacked_bar(plt_df, "age.discrete", "Age", file.path(output_dir, "age.png"),  is_ordinal = T)
  generate_stacked_bar(plt_df, 
                       var="ses.discrete", 
                       title_text="Socio-Economic Status", 
                       is_ordinal = TRUE,
                       out_path=file.path(output_dir, "ses.png"))
  
}

compute_ari_stats <- function (participant_df,  eval_type='all1' ) {
  
  source_python(here("src", "python", "clustering.py"))
  
  cluster_cols <- c('cluster') 
  demo_cols <- c('age.discrete', 'ses.discrete', 'schooling_level_str', 'gender.clean', 'english_best', 'sex_or', 'subject_pool', 'political_interest', 'political_scale_str', 'ethnicity.nih', 'race.nih')
  
  all_cols <- unique(c(demo_cols, cluster_cols))
  n_cols <- length(demo_cols)
  
  sim_df <- participant_df[,all_cols]
  sim_df[all_cols] <- lapply(sim_df[all_cols], factor)
  
  M <- matrix(0, nrow=length(cluster_cols), ncol=n_cols)
  for(i in 1:length(cluster_cols)) {
    cluster_col <- cluster_cols[i]
    for(j in 1:length(demo_cols)) {
      demo_col <- demo_cols[j]
      M[i,j] <- ari(sim_df[[cluster_col]], sim_df[[demo_col]], eval_type)
    }
  }
  renamed_demo_cols <- c("Age", "SES", "Schooling",
                         "Gender", "English Preferred", "Sexual Orientation", "Subject Pool",
                         'Political Interest', 'Political Scale', 'Ethnicity', 'Race')
  colnames(M) <- renamed_demo_cols
  M_df <- as.data.frame(M)
  rownames(M_df) <- cluster_cols
  return(M_df %>% 
           tibble::rownames_to_column("Var1") %>% 
           as_tibble() %>%
           pivot_longer(cols=-Var1, names_to="Var2", values_to="ARI"))
}

plot_ari <- function(ari_mat) {
  p <-  ari_mat %>%
    filter(Var1 == 'cluster', 
           Var1 != Var2) %>%
    arrange(ARI) %>%
    mutate(Var2=factor(Var2, levels=unique(Var2))) %>%
    ggplot(aes(x=Var2, y=ARI)) +
    geom_bar(stat='identity') +
    coord_flip() +
    ylab("Adjusted Rand Index with Media Diet Clusters") +
    jtools::theme_apa(remove.x.gridlines = FALSE) +
    theme(text=element_text(family='Georgia'),
          axis.title.y = element_blank(),
          axis.title.x = element_text(size=10, lineheight = 2)) 
  return(p)
}

get_add1_table <- function(base_model, formula_) {
  t <- add1(base_model, scope=formula_, test='Chi') %>%
    as.data.frame() %>%
    rownames_to_column("predictor") %>%
    as_tibble() %>%
    rename(p=`Pr(>Chi)`) %>%
    mutate(lrt=paste("$\\chi^2$(", Df, ") = ", 
                     sprintf("%.2f", round(LRT, 2)),
                     sep=""),
           lrt=if_else(p < 0.1,  
                       paste(lrt, "$^{", 
                             stringr::str_trim(gtools::stars.pval(p)), 
                             "}$", 
                             sep=""),
                       lrt)) %>%
    dplyr::select(predictor, lrt) %>%
    mutate(predictor=VARIABLE_RENAMER[predictor])
  return(t)
}

get_drop1_table <- function(model) {
  t <- drop1(model, test='Chi') %>%
    as.data.frame() %>%
    rownames_to_column("predictor") %>%
    as_tibble() %>%
    rename(p=`Pr(>Chi)`)
  t <- t %>%
    mutate(lrt=paste("$\\chi^2$(", Df, ") = ", 
                     sprintf("%.2f", round(LRT, 2)),
                     sep=""),
           lrt=if_else(p < 0.1,  
                       paste(lrt, "$^{", 
                             stringr::str_trim(gtools::stars.pval(p)), 
                             "}$", 
                             sep=""),
                       lrt)) %>%
    dplyr::select(predictor, lrt) %>%
    mutate(predictor=VARIABLE_RENAMER[predictor])
  return(t)
}

VARIABLE_RENAMER <- wrapr::qc(subject_pool="Subject Pool", 
              english_best="English Preferred", 
              schooling_level_str="Education",
              age.std="Age",
              ses.std="SES",
              gender.clean=Gender,
              sex_or="Sexual Orientation",
              race.nih="Race (NIH)",
              ethnicity.nih="Ethnicity (NIH)",
              political_interest="Political Interest",
              political_scale_str="Political Scale",
              avg_lastword_rank="Avg LM Likelihood Rank",
              cluster="Media Diet Cluster")

run_spin_models <- function(participants) {
  
  spin_formula <- .~subject_pool+age.std+ses.std+english_best+schooling_level_str+
               gender.clean+sex_or+race.nih+ethnicity.nih+political_interest+political_scale_str+cluster
  # excluding non-English-preferring participants, and excluding variable from model:
  spin_formula_eng <- .~subject_pool+age.std+ses.std+schooling_level_str+
               gender.clean+sex_or+race.nih+ethnicity.nih+political_interest+political_scale_str+cluster
  
  # null models for SPiN High and Low
  spin.l.base <- glm(Low~1, data=participants, family='poisson')
  spin.h.base <- glm(High~1, data=participants, family='poisson')
  
  # null models while filtering on language preference
  spin.l.base_eng <- glm(Low~1, data=participants %>% filter(english_best=="Yes"), family='poisson')
  spin.h.base_eng <- glm(High~1, data=participants %>% filter(english_best=="Yes"), family='poisson')
  
  spin.l.cluster <- update(spin.l.base, .~.+cluster)
  spin.h.cluster <- update(spin.h.base, .~.+cluster)
  
  spin.h <- update(spin.h.base, spin_formula)
  spin.l <- update(spin.l.base, spin_formula)
  spin.h_eng <- update(spin.h.base_eng, spin_formula_eng)
  spin.l_eng <- update(spin.l.base_eng, spin_formula_eng)
  
  #add1(spin.h.base, scope=.~.+cluster, test="Chisq")
  #add1(spin.l.base, scope=.~.+cluster, test="Chisq")
  
  performance::check_overdispersion(spin.h)
  performance::check_overdispersion(spin.l)
  # no overdispersion detected
  
  # Get pseudo-R2 values
  # not used in present version (9/1/2021)
  #rcompanion::nagelkerke(spin.h)
  #rcompanion::nagelkerke(update(spin.h.base, .~.+cluster))
  
  #rcompanion::nagelkerke(update(spin.h, .~.-cluster))
  #rcompanion::nagelkerke(spin.h, null=update(spin.h, .~.-cluster))
  #rcompanion::nagelkerke(spin.l)
  #rcompanion::nagelkerke(update(spin.l, .~.-cluster))
  #rcompanion::nagelkerke(spin.l, null=update(spin.l, .~.-cluster))
  
  spin.h.table <- get_add1_table(spin.h.base, spin_formula) %>%
      rename(add=lrt) %>%
      left_join(get_drop1_table(spin.h)) %>%
      rename(drop=lrt) %>% 
      filter(predictor!='<none>') %>%
      left_join(get_drop1_table(spin.h_eng)) %>%
      rename(drop_eng=lrt) %>%
      replace_na(list(drop_eng="---"))
  spin.l.table <- get_add1_table(spin.l.base, spin_formula) %>%
      rename(add=lrt) %>%
      left_join(get_drop1_table(spin.l)) %>%
      rename(drop=lrt) %>%
      filter(predictor!='<none>') %>%
      left_join(get_drop1_table(spin.l_eng)) %>%
      rename(drop_eng=lrt) %>% 
      replace_na(list(drop_eng="---"))
  
  
  ###### SPAN ######
  
  
  span_formula <- .~subject_pool+age.std+ses.std+english_best+schooling_level_str+
               gender.clean+sex_or+race.nih+ethnicity.nih+political_interest+political_scale_str+cluster
  span_formula_eng <- .~subject_pool+age.std+ses.std+schooling_level_str+
               gender.clean+sex_or+race.nih+ethnicity.nih+political_interest+political_scale_str+cluster
  
  span.base <- glm(span.sentence.count~1, data=participants, family='poisson')
  span.base.eng <- glm(span.sentence.count~1, 
                       data=participants %>% filter(english_best == "Yes"), 
                       family='poisson')
  span_overdisp <- performance::check_overdispersion(span.base)
  if(span_overdisp$p_value < 0.001) {
    span.base <- MASS::glm.nb(span.sentence.count~1, data=participants)
    span.base.eng <- MASS::glm.nb(span.sentence.count~1, 
                                  data=participants %>% filter(english_best == "Yes"))
  }
  span_model <- update(span.base, span_formula)
  span_model.eng <- update(span.base.eng, span_formula_eng)
  
  span.cluster <- update(span.base, .~.+cluster)
  span_cluster_anova <- anova(span.base, span.cluster, test="Chi")
  
  span.table <- get_add1_table(span.base, span_formula) %>%
      rename(add=lrt) %>%
      left_join(get_drop1_table(span_model)) %>%
      rename(drop=lrt) %>%
      filter(predictor!='<none>') %>%
      left_join(get_drop1_table(span_model.eng)) %>%
      replace_na(list(lrt="---"))
  
  results <- list(
    "spin_high_table"=spin.h.table, 
    "spin.h.cluster"=spin.h.cluster,
    "spin_low_table"=spin.l.table,
    "spin.l.cluster"=spin.l.cluster,
    "span_table"=span.table,
    "span.cluster"=span.cluster,
    "span_cluster_anova"=span_cluster_anova
    )
  
  return(results)
}

plot_emm_spin_and_spin <- function(res) {
  
  emm.spin.h <- emmeans::emmeans(res$spin.h.cluster, "cluster", type='response') %>%
    as.data.frame() %>%
    rename(response=rate) %>%
    add_column(task="SPiN-High")
  emm.spin.l <- emmeans::emmeans(res$spin.l.cluster, "cluster", type='response') %>%
    as.data.frame() %>%
    rename(response=rate) %>%
    add_column(task="SPiN-Low")
  emm.span <- emmeans::emmeans(res$span.cluster, "cluster", type='response') %>%
    as.data.frame() %>%
    add_column(task="SPAN")
  
  emm_df <- rbind(emm.spin.h, emm.spin.l, emm.span)
  cluster_order <- sort(unique(emm.spin.h$cluster))
  
  p <- ggplot(emm_df %>%
              mutate(cluster=factor(cluster, levels=levels(cluster_order)),
                     task=factor(task, levels=c("SPiN-High", "SPiN-Low", "SPAN"))),
              aes(x=cluster, y=response)) + 
    geom_point(shape=7) +
    geom_errorbar(aes(ymin=asymp.LCL,
                      ymax=asymp.UCL),
                  width=.4) +
    facet_grid(cols=vars(task)) +
    jtools::theme_apa(remove.y.gridlines = F, remove.x.gridlines = F) +
    xlab("Cluster") + ylab("Predicted Scores")
  return(p)

}


#add1(spin.h.base, spin_formula, test="Chi") %>%
  #as.data.frame() %>%
  #filter(`Pr(>Chi)` < 0.05) %>%
  #tibble::rownames_to_column('var_name') %>%
  #pull(var_name)-> sig_vars
#sig_vars_eng <- sig_vars[sig_vars != 'english_best']

#reform_spin <- as.formula(paste('.~.+', paste(sig_vars, collapse='+'), sep=''))
#reform_spin_eng <- as.formula(paste('.~.+', paste(sig_vars_eng, collapse='+'), sep=''))
#spin.h <- update(spin.h.base, reform_spin)
#spin.h_eng <- update(spin.h.base, reform_spin_eng)

#library(sjPlot)
#library(sjmisc)
#library(multcomp)
#library(ordinal)