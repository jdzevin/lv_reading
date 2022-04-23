
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
                              cluster="Media Diet Cluster",
                              spin_high_avg_trigram_likelihood="Language Model Cloze Probability")

load_study2_data <- function(round) {
  
  output_dir_data <- here("output", "data", round)
  participants <- read_tsv(file.path(output_dir_data, "cleaned_demo_behavioral.tsv"))
  
  ngram_ppl_by_individual <- read_csv(here("output", "lm", round, "by_individual", "ppl_by_item.csv")) %>%
    mutate(sentence_ppl=log(sentence_ppl),
           batch_name=recode(batch_name, SPAN='span.ngrams', `SPIN-HIGH`='spin.high.ngrams', `SPIN-LOW`='spin.low.ngrams'),
           N=recode(N, `1`='unigram', `2`='bigram', `3`='trigram', `4`='4-gram', `5`='5-gram')) %>%
    dplyr::select(qualtrics_id, batch_name, TrialNum, sentence, N, sentence_ppl) %>%
    filter(N %in% c("unigram", "bigram", "trigram"))
  
  ngram_lastword_by_individual <- read_csv(here("output", "lm", round, "by_individual", "ll_by_item.csv")) %>%
    mutate(batch_name=recode(batch_name, SPAN='span.ngrams', `SPIN-HIGH`='spin.high.ngrams', `SPIN-LOW`='spin.low.ngrams'),
           N=recode(N, `1`='unigram', `2`='bigram', `3`='trigram', `4`='4-gram', `5`='5-gram')) %>%
    dplyr::select(qualtrics_id, batch_name, TrialNum, sentence, N, loglikelihood) %>%
    filter(N %in% c("unigram", "bigram", "trigram"))
  
  ngram_ppl_by_cluster <- read_csv(here("output", "lm", round, "by_cluster", "ppl_by_item.csv")) %>%
    mutate(sentence_ppl=log(sentence_ppl),
           batch_name=recode(batch_name, SPAN='span.ngrams', `SPIN-HIGH`='spin.high.ngrams', `SPIN-LOW`='spin.low.ngrams'),
           N=recode(N, `1`='unigram', `2`='bigram', `3`='trigram', `4`='4-gram', `5`='5-gram')) %>%
    dplyr::select(cluster_id, batch_name, TrialNum, sentence, N, sentence_ppl)

  ngram_lastword_by_cluster <- read_csv(here("output", "lm", round, "by_cluster", "ll_by_item.csv")) %>%
    mutate(batch_name=recode(batch_name, SPAN='span.ngrams', `SPIN-HIGH`='spin.high.ngrams', `SPIN-LOW`='spin.low.ngrams'),
           N=recode(N, `1`='unigram', `2`='bigram', `3`='trigram', `4`='4-gram', `5`='5-gram')) %>%
    dplyr::select(cluster_id, batch_name, TrialNum, sentence, N, loglikelihood)

  spin.df <- read_tsv(file.path(output_dir_data, "spin_by_item.tsv")) %>%
    left_join(read_csv(file.path(output_dir_data, 'items_clusters.csv')) %>%
                rename(cluster_id=cluster.id.items)) %>%
    convert(chr(TrialNum)) %>%
    rename(sent_correct=spin_sentence_correct)
  # Some prior formatting issues left trial number blank in Round 1
  if(round == "round1") {
    num_na <- nrow(spin.df[is.na(spin.df$TrialNum),'TrialNum'])
    spin.df[is.na(spin.df$TrialNum),'TrialNum'] = as.character(rep(1:50, num_na/50))
  }
  spin.df <- spin.df %>%
    filter(qualtrics_id %in% unique(participants$qualtrics_id))
  
  spin_item_aggs <- spin.df %>%
    group_by(Item) %>%
    summarize(mean_item_score=mean(sent_correct)) %>%
    filter(mean_item_score > 0.0) %>%
    pull(Item)
  spin_item_excluded <- spin.df %>%
    group_by(Item) %>%
    summarize(mean_item_score=mean(sent_correct)) %>%
    filter(mean_item_score == 0.0) %>%
    pull(Item)
  print(spin_item_excluded)
  spin.df <- spin.df %>% filter(Item %in% spin_item_aggs)
  
  #span.df <- read_tsv(here("output", "data", "round1", "span_by_item.tsv")) %>%
  span.df <- read_tsv(file.path(output_dir_data, "span_by_item.tsv")) %>%
    left_join(read_csv(file.path(output_dir_data, 'items_clusters.csv'))) %>%
    filter(qualtrics_id %in% unique(participants$qualtrics_id)) %>%
    mutate(TrialNum=paste(Set, Trial, Sentence, sep='-'),
           sent_correct=if_else(sent_correct>0, 1, 0)) %>%
    group_by(TrialNum) %>%
    mutate(num_responses=n()) %>% 
    ungroup() %>%
    rename(cluster_id=cluster.id.items)
  
  lastword.wide.cluster <- ngram_lastword_by_cluster %>%
    pivot_wider(id_cols=c(cluster_id, batch_name, sentence, TrialNum),
                names_from=N,
                values_from=loglikelihood,
                names_prefix="likelihood.by_cluster."
                ) %>%
    mutate(batch_name=gsub("span", "span.NA", batch_name)) %>%
    separate(batch_name, into=c("test_name", "Predictability", "lm")) %>%
    mutate(test_name=recode(test_name, spin="SPiN", span="SPAN"),
           Predictability=recode(Predictability, high="High", low="Low"))

  ppl.wide.cluster <- ngram_ppl_by_cluster %>%
    pivot_wider(id_cols=c(cluster_id, batch_name, sentence, TrialNum),
                names_from=N,
                values_from=sentence_ppl,
                names_prefix="perplexity.by_cluster.") %>%
    mutate(batch_name=gsub("span", "span.NA", batch_name)) %>%
    separate(batch_name, into=c("test_name", "Predictability", "lm")) %>%
    mutate(test_name=recode(test_name, spin="SPiN", span="SPAN"),
           Predictability=recode(Predictability, high="High", low="Low"))

  lastword.wide.individual <- ngram_lastword_by_individual %>%
    pivot_wider(id_cols=c(qualtrics_id, batch_name, sentence, TrialNum), 
                names_from=N,
                values_from=loglikelihood,
                names_prefix="likelihood.by_individual."
    ) %>%
    mutate(batch_name=gsub("span", "span.NA", batch_name)) %>%
    separate(batch_name, into=c("test_name", "Predictability", "lm")) %>%
    mutate(test_name=recode(test_name, spin="SPiN", span="SPAN"),
           Predictability=recode(Predictability, high="High", low="Low"))
  
  ppl.wide.individual <- ngram_ppl_by_individual %>%
    pivot_wider(id_cols=c(qualtrics_id, batch_name, sentence, TrialNum), 
                names_from=N,
                values_from=sentence_ppl,
                names_prefix="perplexity.by_individual.") %>%
    mutate(batch_name=gsub("span", "span.NA", batch_name)) %>%
    separate(batch_name, into=c("test_name", "Predictability", "lm")) %>%
    mutate(test_name=recode(test_name, spin="SPiN", span="SPAN"),
           Predictability=recode(Predictability, high="High", low="Low"))
  
  spin.ngram <- spin.df %>%
    left_join(lastword.wide.cluster %>% filter(test_name=="SPiN")) %>%
    left_join(ppl.wide.cluster %>% filter(test_name=="SPiN")) %>%
    left_join(lastword.wide.individual %>% filter(test_name=="SPiN")) %>%
    left_join(ppl.wide.individual %>% filter(test_name=="SPiN"))
  
  print(span.df %>% distinct(Stimulus))
  span.ngram <- span.df %>%
    left_join(lastword.wide.cluster %>% filter(test_name=="SPAN")) %>%
    left_join(ppl.wide.cluster %>% filter(test_name=="SPAN")) %>%
    left_join(lastword.wide.individual %>% filter(test_name=="SPAN")) %>%
    left_join(ppl.wide.individual %>% filter(test_name=="SPAN")) %>%
    filter(num_responses >= 10) 
  print(span.ngram %>% distinct(Stimulus))
  
  full_df <- bind_rows(
    spin.ngram %>%
      left_join(participants %>% 
                  dplyr::select(qualtrics_id, subject_pool, english_best, political_scale_str, 
                                schooling_level_str, sex_or, language.clean, gender.clean,
                                race.nih, ethnicity.nih, age.std, ses.std)),
    span.ngram %>%
      left_join(participants %>% 
                  dplyr::select(qualtrics_id, subject_pool, english_best, political_scale_str, 
                                schooling_level_str, sex_or, language.clean, gender.clean,
                                race.nih, ethnicity.nih, age.std, ses.std))
  )
  
  return(full_df)
  
}

run_binomial_model <- function(df,
                               ivs,
                               rand.slopes=FALSE) {
  
  if(length(ivs) == 0) {
    f <- formula("sent_correct ~ (1|sentence)")
  } else {
    f <- formula(paste0("sent_correct ~ ", paste(ivs, sep='+', collapse='+'), "+(1|sentence)"))
  }
    
  m <- glmer(
    formula=f,
    data=df,
    family=binomial,
    control=glmerControl(optimizer="bobyqa"),
    nAGQ=ifelse(rand.slopes, 1, 20)
  )
  return(m)
}

run_study2_model <- function(
  participant_df, 
  test="SPiN", 
  predictability="High", 
  lm_var_type="likelihood",
  lm_grouping="by_cluster",
  N='trigram', 
  english_only=FALSE,
  save_root="./", 
  cntrl_vars=c("subject_pool", "age.std", "race.nih", "schooling_level_str", "political_scale_str"),
  overwrite=FALSE
  ) {
  
  subset_df <- participant_df %>%
    filter(test_name == test)
  if(test == "SPiN") {
    subset_df <- subset_df %>% filter(Predictability==predictability)
  }
  if(english_only) {
    subset_df <- subset_df %>% filter(english_best == "Yes")
  }
  
  lm_var <- paste(lm_var_type, lm_grouping, N, sep='.')
  model_types <- c("intercept", "lm", "demo", "lm_plus_demo")
  english_str <- ifelse(english_only, "LangEnglish", "LangAll")
  dv_name <- ifelse(test=="SPiN", paste(test, predictability, sep='_'), test)
  
  subset_df <- subset_df %>%
    drop_na(all_of(lm_var), all_of(cntrl_vars))
  
  for(model_type in model_types) {
    save_name <- paste(dv_name, lm_var_type, lm_grouping, N, model_type, english_str, sep='.')
    if(!file.exists(file.path(save_root, save_name)) | overwrite) {
      if(model_type == "intercept") {
        ivs <- c()
      } else if(model_type == "lm") {
        ivs <- c(lm_var)
      } else if(model_type == "demo") {
        if(english_only) {
          ivs <- cntrl_vars
        } else {
          ivs <- c(cntrl_vars, "english_best")
        }
      } else if(model_type == "lm_plus_demo") {
        if(english_only) {
          ivs <- c(cntrl_vars, lm_var)
        } else {
          ivs <- c(cntrl_vars, "english_best", lm_var)
        }
      }
      m <- run_binomial_model(subset_df, ivs=ivs)
      print(save_name)
      saveRDS(m, file.path(save_root, save_name))
    }
  }
}

run_study2_models <- function(
  df,
  save_root,
  tests=c("SPiN", "SPAN"),
  conditions=c("High", "Low"),
  lm_var_types=c("likelihood", "perplexity"),
  lm_groupings=c("by_cluster", "by_individual"),
  Ns=c("unigram", "bigram", "trigram"),
  english_only_options=c(TRUE, FALSE),
  overwrite=FALSE
) {
  
  for(test in tests) {
    if(test == "SPiN") {
      use_conditions <- conditions
    } else {
      use_conditions <- c("")
    }
    for(predictability in use_conditions) {
      for(lm_var_type in lm_var_types) {
        for(lm_grouping in lm_groupings) {
          for(N in Ns) {
            for(english_only in english_only_options) {
              run_study2_model(df, test=test, predictability=predictability, lm_var_type=lm_var_type,
                               lm_grouping=lm_grouping, N=N, english_only=english_only, save_root=save_root, overwrite=overwrite)
            }
          }
        }
      }
    }
  }
}

load_study2_models <- function(save_root="./") {
  
  models <- list()
  
  for(f in list.files(save_root)) {
    models[[f]] <- readRDS(file.path(save_root, f))
  }
  
  return(models)
  
}

viz_item_intercepts <- function(m) {
  
  se <- sqrt(diag(vcov(m)))
  tab <- exp(cbind(Est = fixef(m), 
                   LL = fixef(m) - 1.96 * se, 
                   UL = fixef(m) + 1.96 * se
  )
  )
  
  lattice::dotplot(ranef(m, which = "sentence", condVar = TRUE))
  
}

run_spin_models <- function(participants, vars, lm_var) {
  
  participants <- aggregate_study2_data(dataframes)
  vars <- c("subject_pool","age.std","ses.std","schooling_level_str", "english_best",
              "gender.clean",'sex_or',"race.nih","ethnicity.nih","political_interest","political_scale_str")
  lm_var <- "spin_high_avg_trigram_likelihood"
  
  spin_formula <- formula(paste0(".~", paste(c(vars, lm_var), sep='+', collapse='+')))
  # excluding non-English-preferring participants, and excluding variable from model:
  eng_vars <- vars[!vars %in% c("english_best")]
  spin_formula_eng <- formula(paste0(".~", paste(c(eng_vars, lm_var), sep='+', collapse='+')))
  
  # null models for SPiN High and Low
  spin.l.base <- glm(Low~1, data=participants, family='poisson')
  spin.h.base <- glm(High~1, data=participants, family='poisson')
  
  # null models while filtering on language preference
  spin.l.base_eng <- glm(Low~1, data=participants %>% filter(english_best=="Yes"), family='poisson')
  spin.h.base_eng <- glm(High~1, data=participants %>% filter(english_best=="Yes"), family='poisson')
  
  spin.l.likelihood <- update(spin.l.base, formula(paste0(".~.+", lm_var)))
  spin.h.likelihood <- update(spin.h.base, formula(paste0(".~.+", lm_var))) 
  
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
  
  spin.h.table
  
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





# lm_df_reference <- parse_lm_data(
#   file.path(output_dir_data, 
#             "roberta_base_cluster_spin_span_perplexity_ranks.csv"),
#   grouping_variable='cluster_id'
# ) 
# lm_df_reference$spin <- lm_df_reference$spin %>%
#   distinct(spin_item_num, perplexity, likelihood, lastword_rank)
# 
# lm_dfs_with_rank <- parse_lm_data(file.path(output_dir_data, 
#                                            "cluster_spin_span_perplexity_ranks.csv"), 
#                                  grouping_variable = 'cluster_id')
# spin.df <- read_tsv(file.path(output_dir_data, "spin_by_item.tsv")) %>%
#   left_join(read_csv(file.path(output_dir_data, 'items_clusters.csv')))
# 
# num_na <- nrow(spin.df[is.na(spin.df$TrialNum),'TrialNum'])
# spin.df[is.na(spin.df$TrialNum),'TrialNum'] = rep(1:50, num_na/50)
# 
# spin.df <- spin.df %>%
#   mutate(spin_item_num=TrialNum-1) %>%
#   rename(cluster=cluster.id.items) %>%
#   filter(cluster %in% unique(participants$cluster))
# 
# lm_df_spin <- lm_dfs_with_rank$spin %>%
#   left_join(spin.df %>% 
#               distinct(spin_item_num, Predictability)) %>%
#   left_join(lm_df_reference$spin,
#             by='spin_item_num', suffix=c('', '.ref'))
# spin.df %>%
#   group_by(Item)  %>%
#   summarize(sd_=sd(spin_sentence_correct))  %>%
#   filter(sd_ != 0) %>%
#   pull(Item) -> valid_items
# 
# spin.df %>%
#   group_by(Predictability, subject_pool, Item) %>%
#   summarize(avg_spin=mean(spin_sentence_correct)) %>%
#   pivot_wider(id_col=c(Item, Predictability), 
#               names_from=subject_pool, values_from=avg_spin) %>%
#   arrange(max(USC, Community))  %>%
#   mutate(Item=factor(Item, levels=unique(Item))) %>%
#   ggplot(aes(x=reorder(Item, USC), color=Community>USC)) +
#   geom_point(aes(y=Community), shape=1) + 
#   geom_point(aes(y=USC), shape=2) + 
#   geom_segment(aes(x=Item, xend=Item, y=Community, yend=USC)) +
#   coord_flip() +
#   facet_wrap(~Predictability, ncol=1, scales='free') +
#   theme_minimal()
# 
# 
# ```
# 
# spin_formula_item <- spin_sentence_correct~cluster+subject_pool+schooling_level_str+age.std+perplexity.improve+likelihood.improve+wordrank.improve
# spin_formula_item_add <- .~.+cluster+subject_pool+schooling_level_str+age.std+perplexity.improve+likelihood.improve+wordrank.improve
# 
# spin.df %>%
#   mutate(cluster=factor(cluster)) %>%
#   left_join(participants, by=c('qualtrics_id', 'subject_pool', 'cluster')) %>%
#   left_join(lm_df_spin %>%
#     rename(cluster=cluster_id)) %>%
#   filter(Item %in% valid_items) %>%
#   mutate(likelihood=likelihood+1e-10,
#          wordrank.improve=lastword_rank.ref/lastword_rank,
#          perplexity.improve=perplexity.ref/perplexity,
#          likelihood.improve=likelihood.ref/likelihood) %>%
#   group_by(Predictability, Item) %>%
#   nest_legacy(-Item) %>%
#   mutate(
#     #drop1_lrt=map(data, ~ drop1(glm(spin_formula_item, family=binomial, data=.x), test='Chi')),
#     add1_lrt=map(data, ~ add1(glm(spin_sentence_correct~1, 
#                                   family=binomial, 
#                                   data=.x), 
#                               spin_formula_item_add,
#                               test='Chi')),
#     tidied.full= map(add1_lrt, broom::tidy),
#   )   %>%
#   unnest(tidied.full) %>%
#   filter(!is.na(p.value)) %>%
#   mutate(sig=gtools::stars.pval(p.value)) %>%
#   group_by(Predictability, Item) %>%
#   mutate(item_avg_lrt=mean(LRT)) %>%
#   ungroup(Item) %>%
#   arrange(item_avg_lrt) %>%
#   mutate(Item=factor(Item, levels=unique(Item))) %>%
#   ggplot(aes(Item, shape=sig, color=Predictability)) +
#     geom_point(aes(y=LRT), size=2) +
#   coord_flip() +
#     facet_grid(rows=vars(Predictability),
#                cols=vars(term), scales='free_y') +
#   theme_bw() +
#   scale_color_manual(values=c("black", "blue"))
# 
# 
# # Notes...instead of empirical diff, effect size of statistical comparison/model of performance ~ demo. We can observe if including cluster as a variable moderates this effect, per item
# 
# 
# 
# p.clusteravg.perf <- spin.df %>%
#   rename(cluster_id=cluster) %>%
#   convert(fct(cluster_id)) %>%
#   left_join(lm_df_spin) %>%
#   group_by(cluster_id, Predictability, Item) %>%
#   summarize(cluster_avg_correct=mean(spin_sentence_correct)) %>%
#   drop_na(cluster_id, Item) %>%
#   ungroup(cluster_id) %>%
#   group_by(Predictability, Item) %>%
#   mutate(item_min_by_cluster=min(cluster_avg_correct),
#          item_max_by_cluster=max(cluster_avg_correct),
#          item_avg_by_cluster=mean(cluster_avg_correct)) %>%
#   ungroup() %>%
#   arrange(item_avg_by_cluster) %>%
#   mutate(Item=factor(Item, levels=unique(Item))) %>%
#   ggplot(aes(x=Item)) +
#     geom_point(aes(y=cluster_avg_correct, color=cluster_id), shape=15, size=2) +
#     geom_segment(data=. %>% 
#                    distinct(Item, item_avg_by_cluster, item_min_by_cluster, item_max_by_cluster, Predictability) %>%
#                    arrange(item_max_by_cluster),
#                  aes(x=Item, xend=Item, y=item_min_by_cluster, yend=item_max_by_cluster)) +
#     coord_flip() +
#   theme_bw() +
#   coord_flip() +
#   labs(y='Cluster-level Average Performance')  +
#   theme(axis.title.y=element_blank()) +
#   scale_color_brewer(type='qual') +
#   facet_wrap(~Predictability, ncol=1, scales='free', labeller = label_both) +
#   labs(title="Cluster average SPIN performance [0--1]") +
#   theme(legend.position='none')
# 
# item_order <- levels(p.clusteravg.perf$data$Item)
# 
# p.clusteravg.lms <- spin.df %>%
#   mutate(cluster=factor(cluster)) %>%
#   left_join(participants, by=c('qualtrics_id', 'subject_pool', 'cluster')) %>%
#   left_join(lm_df_spin %>% rename(cluster=cluster_id)) %>%
#   filter(Item %in% valid_items) %>%
#   mutate(likelihood=likelihood+1e-10,
#          wordrank.improve=log(lastword_rank.ref/lastword_rank),
#          perplexity.improve=log(perplexity.ref/perplexity),
#          likelihood.improve=log(likelihood.ref/likelihood)) %>%
#   group_by(cluster, Predictability, Item) %>%
#   mutate(cluster_avg_correct=mean(likelihood.improve)) %>%
#   drop_na(cluster, Item) %>%
#   ungroup(cluster) %>%
#   group_by(Predictability, Item) %>%
#   mutate(item_min_by_cluster=min(likelihood.improve),
#          item_max_by_cluster=max(likelihood.improve),
#          item_avg_by_cluster=mean(likelihood.improve)) %>%
#   mutate(Item=factor(Item, levels=item_order)) %>%
#   ggplot(aes(x=Item)) +
#     geom_point(aes(y=cluster_avg_correct, color=cluster), shape=15, size=2) +
#     geom_segment(data=. %>% 
#                    distinct(Item, item_avg_by_cluster, item_min_by_cluster, item_max_by_cluster, Predictability),
#                  aes(x=Item, xend=Item, y=item_min_by_cluster, yend=item_max_by_cluster)) +
#     coord_flip() +
#   theme_bw() +
#   coord_flip() +
#   labs(y='Cluster Average WordRank improvement')  +
#   theme(axis.title.y=element_blank()) +
#   scale_color_brewer(type='qual') +
#   facet_wrap(~Predictability, ncol=1, scales='free', labeller = label_both) +
#   labs(title="Item-level AVG LM WordRank") +
#   theme(axis.text.y = element_blank())
# 
# library(patchwork)
# 
# p.clusteravg.perf + p.clusteravg.lms
# 
# spin.df %>%
#   mutate(cluster=factor(cluster)) %>%
#   left_join(participants, by=c('qualtrics_id', 'subject_pool', 'cluster')) %>%
#   left_join(lm_df_spin %>% rename(cluster=cluster_id)) %>%
#   filter(Item %in% valid_items) %>%
#   mutate(likelihood=likelihood+1e-10,
#          wordrank.improve=log(lastword_rank.ref/lastword_rank),
#          perplexity.improve=log(perplexity.ref/perplexity),
#          likelihood.improve=log(likelihood.ref/likelihood)) %>%
#   group_by(Predictability, Item) %>%
#   summarize(rank.correlation=cor(perplexity.improve, spin_sentence_correct, method='spearman')) %>%
#   ggplot(aes(reorder(Item, rank.correlation), y=rank.correlation)) +
#     geom_point() +
#     coord_flip() +
#     geom_hline(yintercept=0) +
#     facet_wrap(~Predictability, ncol=1, scales='free') +
#   theme_bw() +
#   theme(axis.title.y=element_blank())
# 
# spin.df %>%
#   mutate(cluster=factor(cluster)) %>%
#   left_join(participants, by=c('qualtrics_id', 'subject_pool', 'cluster')) %>%
#   left_join(lm_df_spin %>% rename(cluster=cluster_id)) %>%
#   filter(Item %in% valid_items) %>%
#   mutate(likelihood=likelihood+1e-10,
#          wordrank.improve=log(lastword_rank.ref/lastword_rank),
#          perplexity.improve=log(perplexity.ref/perplexity),
#          likelihood.improve=log(likelihood.ref/likelihood)) %>%
#   pivot_longer(cols=ends_with('improve'), names_to='metric_type', values_to='lm_metric') %>%
#   group_by(Predictability, cluster, metric_type) %>%
#   summarize(rank.correlation=cor(lm_metric, spin_sentence_correct, method='spearman')) %>%
#   ggplot(aes(factor(cluster, level=rev(unique(cluster))), rank.correlation)) +
#     geom_point() +
#     coord_flip() +
#     geom_hline(yintercept=0) +
#     facet_grid(metric_type~Predictability) +#, ncol=1, scales='free_y') +
#   theme_bw() +
#   theme(axis.title.y=element_blank())
# 
# 
# 
# 
# 
# 
# lm_df_spin %>%
#   rename(cluster=cluster_id) %>%
#   inner_join(spin.df %>%
#                #group_by(cluster) %>%
#                #mutate(cluster_size=n()) %>% ungroup() %>%
#                convert(fct(cluster)) %>%
#                group_by(cluster, Item, spin_item_num) %>%
#                summarize(SpinSum=sum(spin_sentence_correct),
#                          cluster_size=n())) %>%
#   mutate(likelihood=likelihood+1e-10,
#          wordrank.improve=lastword_rank.ref/lastword_rank,
#          perplexity.improve=perplexity.ref/perplexity,
#          likelihood.improve=likelihood.ref/likelihood) -> temp_df
# 
# lm_df_spin %>%
#   rename(cluster=cluster_id) %>%
#   left_join(spin.df %>%
#                convert(fct(cluster))) %>%
#   mutate(likelihood=likelihood+1e-10,
#          wordrank.improve=log(lastword_rank.ref/lastword_rank),
#          perplexity.improve=log(perplexity.ref/perplexity),
#          likelihood.improve=log(likelihood.ref/likelihood)) -> itemlevel_df
#   #mutate(perplexity.improve=cut_number(perplexity.improve, n=10))-> itemlevel_df
#          #perplexity.improve=scale(perplexity.improve)[,1]) -> itemlevel_df
# 
# library(purrr)
# itemlevel_df %>%
#   pivot_longer(cols=ends_with('improve'), names_to="metric", values_to="improvement_ratio") %>%
#   group_by(cluster, Predictability, qualtrics_id, metric) %>%
#   nest_legacy(improvement_ratio, spin_sentence_correct) %>%
#   mutate(
#     test = purrr::map(data, ~ cor.test(.x$improvement_ratio, .x$spin_sentence_correct, method='spearman')),
#     tidied= map(test, broom::tidy)
#   ) %>%
#   unnest(tidied) %>%
#   select(-data, -test) %>%
#   group_by(cluster, Predictability, metric) %>%
#   mutate(mean_estimate=mean(estimate)) %>%
#   ungroup() %>%
#   mutate(cluster=factor(cluster, levels=0:(length(levels(cluster))-1))) %>%
#   ggplot(aes(x=cluster, y=mean_estimate, color=mean_estimate>0)) +#, shape=p.value.str)) +
#   geom_segment(aes(x=cluster, xend=cluster, y=0, yend=mean_estimate), show.legend = FALSE, size=1, color='black', 
#                arrow=arrow(length=unit(1, 'mm'))) +
#   geom_jitter(aes(x=cluster, y=estimate), width = .25, alpha=.25, show.legend = FALSE)  +
#   labs(y="Rank correlation") +
#   theme_bw() +
#   geom_hline(yintercept=0, linetype=2) +
#   facet_grid(metric~Predictability, labeller = label_both)
#   
# itemlevel_df %>%
#   nest_by(cluster, Predictability) %>%
#   mutate(fit_model = list(glm(as.formula("spin_sentence_correct ~ perplexity.improve"), family=binomial, data=data)))  %>%
#   summarise(broom::tidy(fit_model)) %>%
#   filter(term != "(Intercept)") %>%
#   #mutate(p.value.bonf=p.adjust(p.value, method='bonferroni')) %>%
#   ungroup() %>%
#   arrange(p.value) %>%
#   mutate(cluster = factor(cluster, levels = unique(cluster))) %>%
#   filter(estimate <= 10) %>%
#   ggplot(aes(x=cluster, y=estimate, color=Predictability, ymin=estimate-std.error, ymax=estimate+std.error)) + 
#   geom_errorbar() +
#   coord_flip() 
# 
# ggplot(itemlevel_df, aes(x=perplexity.improve)) +
#   geom_histogram() +
#   facet_grid(cluster~Predictability, scales='free') + 
#   theme_minimal()
# 
# lik.high <- glmer(spin_sentence_correct~perplexity+likelihood+Predictability+(1+Predictability|cluster),
#            family=binomial, 
#            data=itemlevel_df %>% 
#              mutate(perplexity=log(perplexity), 
#                     likelihood.ref=exp(-likelihood.ref),
#                                         likelihood=exp(-likelihood)))
# per.high <- glmer(spin_sentence_correct~likelihood.improve*Predictability+ (1+likelihood.improve|cluster),
#            family=binomial, 
#            data=itemlevel_df %>%
#              mutate(perplexity=log(perplexity)))
# sjPlot::plot_model(per.high, type='int')
# sjPlot::plot_model(lik.high, type='pred', terms=c("likelihood", "Predictability"), pred.type = 'fe') +
#   theme_minimal()
# sjPlot::plot_model(per.high, type='pred', terms=c("perplexity.improve", "Predictability"), pred.type = 'fe') +
#   theme_minimal()
# sjPlot::plot_model(per.high, type='pred', terms=c("likelihood.improve", "Predictability"), pred.type = 'fe') +
#   theme_minimal()
# sjPlot::plot_model(per.high, type='re') -> p
# 
# sort(as.integer(unique(p$data$term)))
# p$data %>%
#   mutate(term=as.character(term), 
#          term=factor(term, levels=rev(sort(unique(term))))) %>%
#   ggplot(aes(x=term, y=estimate, ymin=conf.low, ymax=conf.high)) +
#   geom_pointrange() +
#   coord_flip() + 
#   geom_hline(yintercept=1, linetype=2) +
#   facet_wrap(~facet, scales='free')
# 
# itemlevel_df %>%
#   ggplot(aes(x=perplexity.improve)) +
#   geom_histogram() + facet_wrap(~cluster)
#          #likelihood.improve=if_else(likelihood.exp>likelihood.ref.exp,
#                                     #"MoreLikely",
#                                     #"LessLikely")) -> temp_df
# map_clust_names <- list()
# uniq_clust <- as.vector(levels(participants$cluster))
# for(i in 0:(length(levels(participants$cluster))-1)) {
#   x <- uniq_clust[i+1]
#   map_clust_names[x] <- i
# }
# itemlevel_df <- itemlevel_df %>%
#   mutate(cluster=recode_factor(cluster, !!!map_clust_names))
# 
# # word rank for clusters
# itemlevel_df %>%
#   mutate(cluster=factor(cluster, levels=rev(sort(levels(cluster))))) %>%
#   ggplot(aes(x=cluster, y=log(wordrank.improve))) +
#   geom_jitter() +
#   geom_boxplot(outlier.shape=NA) +
#   coord_flip() +
#   geom_hline(yintercept=0)+
#   theme_bw() +
#   facet_wrap(~Predictability, ncol=1) + #, scales='free') +
#   theme(axis.title.y = element_blank()) +
#   labs(title="Improvement in Word Rank by Cluster",
#        y='log(ReferenceWordRank/WordRank)')
# 
# # likelihood for clusters
# temp_df %>%
#   mutate(cluster=factor(cluster, levels=rev(sort(levels(cluster))))) %>%
#   ggplot(aes(x=cluster, y=log(likelihood.improve))) +
#   geom_jitter() +
#   geom_boxplot(outlier.shape=NA) +
#   coord_flip() +
#   geom_hline(yintercept=0)+
#   theme_bw() +
#   facet_wrap(~Predictability, ncol=1) +#, scales='free') +
#   theme(axis.title.y = element_blank()) +
#   labs(title="Improvement in Likelihood by Cluster",
#        y='log(ReferenceLikelihood/Likelihood)')
# 
# # perplexity for clusters
# temp_df %>%
#   mutate(cluster=factor(cluster, levels=rev(sort(levels(cluster))))) %>%
#   ggplot(aes(x=cluster, y=log(perplexity.improve))) +
#   geom_jitter() +
#   geom_boxplot(outlier.shape=NA) +
#   coord_flip() +
#   geom_hline(yintercept=0)+
#   theme_bw() +
#   facet_wrap(~Predictability, ncol=1) +#, scales='free') +
#   theme(axis.title.y = element_blank()) +
#   labs(title="Improvement in Perplexity by Cluster",
#        y='log(ReferencePerplexity/Perplexity)')
# ##### Now by item #####
# temp_df %>%
#   ggplot(aes(x=Item, y=log(wordrank.improve))) +
#   geom_jitter() +
#   geom_boxplot(outlier.shape=NA) +
#   coord_flip() +
#   geom_hline(yintercept=0)+
#   theme_bw() +
#   facet_wrap(~Predictability, ncol=1) + #, scales='free') +
#   theme(axis.title.y = element_blank()) +
#   labs(title="Improvement in word rank (reference / rank): Higher is better",
#        subtitle="Points represent the cluster's improvement on a single item",
#        y='Relative improvement in word rank  (Greater than 1 indicates improvement)')
# 
# # perplexity for clusters
# temp_df %>%
#   ggplot(aes(x=Item, y=log(perplexity.improve))) +
#   geom_boxplot(outlier.shape=NA) +
#   geom_jitter(alpha=0.5) +
#   coord_flip() +
#   geom_hline(yintercept=0)+
#   theme_bw() +
#   facet_wrap(~Predictability, ncol=2, scales='free_y') +
#   theme(axis.title.y = element_blank()) +
#   labs(title="Improvement in perplexity: log(ReferencePerplexity/Perplexity)",
#        subtitle="Points represent the cluster's improvement on a single item",
#        y='log(ReferenceLMPerplexity/TunedPerplexity)')
# 
# temp_df %>%
#   distinct(Item, perplexity.improve, likelihood.improve, wordrank.improve) %>%
#   mutate(perplexity.improve.log=log(perplexity.improve),
#          likelihood.improve.log=log(likelihood.improve),
#          wordrank.improve.log=log(wordrank.improve)) %>%
#   pivot_longer(cols=-Item) %>%
# ggplot(aes(x=value)) +
#   geom_density() +
#   facet_wrap(~name, ncol=2, scales = 'free') +
#   theme_bw()
# # word rank for items
# temp_df %>%
#   group_by(Item) %>%
#   mutate(item.median.rank=median(wordrank.improve)) %>% ungroup() %>%
#   group_by(Predictability) %>%
#   arrange(Predictability, item.median.rank) %>%
#   mutate(Item=factor(Item, levels=unique(Item))) %>%
#   ggplot(aes(x=Item, y=log(wordrank.improve))) +
#   geom_jitter() +
#   geom_boxplot(outlier.shape=NA) +
#   coord_flip() +
#   geom_hline(yintercept=0)+
#   theme_bw() +
#   facet_wrap(~Predictability, ncol=1, scales='free_y') +
#   theme(axis.title.y = element_blank()) +
#   labs(title="Improvement in ranking last word: reference / rank (Higher is better)",
#        y='log(ReferenceWordRank/WordRank)')
# # perplexity for items
# 
# itemlevel_df %>%
#   group_by(cluster, Predictability) %>%
#   summarize(r=cor(perplexity.improve, spin_sentence_correct, method='spearman')) %>%
#   ggplot(aes(cluster, r)) + 
#   geom_bar(stat='identity') + 
#   coord_flip() +
#   facet_wrap(~Predictability, ncol=1) + 
#   theme_minimal()
# 
# temp_df %>%
#   group_by(Item) %>%
#   mutate(item.median.perplexity=median(perplexity.improve)) %>% ungroup() %>%
#   group_by(Predictability) %>%
#   arrange(Predictability, item.median.perplexity) %>%
#   mutate(Item=factor(Item, levels=unique(Item))) %>%
#   ggplot(aes(x=Item, y=log(perplexity.improve))) +
#   geom_jitter() +
#   geom_boxplot(outlier.shape=NA) +
#   coord_flip() +
#   geom_hline(yintercept=0)+
#   theme_bw() +
#   facet_wrap(~Predictability, ncol=1, scales='free_y') +
#   theme(axis.title.y = element_blank()) +
#   labs(title="Improvement in sentence perplexity (reference / rank): Higher is better",
#        y='log(ReferencePerplexity/Perplexity)')
# 
# # likelihood for items
# temp_df %>%
#   group_by(Item) %>%
#   mutate(item.median.likelihood=median(likelihood.improve)) %>% ungroup() %>%
#   group_by(Predictability) %>%
#   arrange(Predictability, item.median.likelihood) %>%
#   mutate(Item=factor(Item, levels=unique(Item))) %>%
#   ggplot(aes(x=Item, y=log(likelihood.improve))) +
#   geom_jitter() +
#   geom_boxplot(outlier.shape=NA) +
#   coord_flip() +
#   geom_hline(yintercept=0)+
#   theme_bw() +
#   facet_wrap(~Predictability, ncol=1, scales='free_y') +
#   theme(axis.title.y = element_blank()) +
#   labs(title="Improvement in last word likelihood (reference / rank): Higher is better",
#        subtitle="Points represent the cluster's improvement on a single item",
#        y='log(ReferenceLikelihood/Likelihood)')
# 
# 
# 
# 
# temp_df %>%
#   mutate(cluster=factor(cluster, levels=sort(levels(cluster)))) %>%
#   ggplot(aes(log(lastword_rank), log(lastword_rank.ref))) +
#   geom_point() +
#   geom_abline(slope=1, intercept=0) +
#   facet_grid(cols=vars(Predictability), rows=vars(cluster), scales='free') +
#   labs(title="LastWord Likelihood for Cluster LMs: Reference vs Tuned",
#        y="Baseline LM word-rank (log)", x="Tuned LM word-rank (log)") +
#   theme_bw()  
#   #coord_cartesian(xlim = c(1, 11), ylim=c(1, 11))
# 
# stat_df <- temp_df %>%
#   mutate(wordrank.improve=log(wordrank.improve),
#         perplexity.improve=log(perplexity.improve),
#         likelihood.improve=log(likelihood.improve))
#   mutate(wordrank.improve=scale(wordrank.improve)[,1],
#          likelihood.improve=scale(likelihood.improve)[,1],
#          perplexity.improve=scale(perplexity.improve)[,1])
# stat_df %>%
#   group_by(Item) %>%
#   summarize(item_sd=sd(SpinSum)) %>%
#   arrange(item_sd)  %>%
#   filter(item_sd > 0) %>% 
#   distinct(Item) %>%
#   pull(Item) -> valid_items
# stat_df <- stat_df %>%
#   filter(Item %in% valid_items)
# 
# stats_high <- stat_df %>%
#   filter(Predictability == 'High')
# stats_low <- stat_df %>%
#   filter(Predictability == 'Low')
# 
# library(lmerTest)
# stat_df %>%
#   ggplot(aes(x=cluster_size, y=SpinSum)) + geom_jitter()
# # ADJUST (OFFSET) FOR CLUSTER SIZE!
# byitem.wordrank <- lme4::glmer.nb(SpinSum~offset(log(cluster_size))+wordrank.improve + (1+wordrank.improve|Item),
#               data=stat_df)
# byitem.perplexity <- glmer.nb(SpinSum~offset(log(cluster_size)) + 
#                                 perplexity.improve + 
#                                 (1+perplexity.improve|Item), data=stat_df)
# 
# byitem.perplexity <- glmer.nb(SpinSum~offset(log(cluster_size)) + 
#                                 perplexity.improve + 
#                                 (1+perplexity.improve|Item), data=stat_df)
# byitem.perplexity <- MASS::glm.nb(SpinSum~offset(log(cluster_size)) + 
#                                 perplexity.improve*Predictability, data=stat_df)
# bycluster.perplexity <- lmer(SpinSum~#offset(log(cluster_size)) + 
#                                 perplexity.improve*Predictability +
#                                 (1|cluster),  data=stat_df)
# summary(bycluster.perplexity)
# #hist(stats_high$SpinSum, breaks = 60)
# sjPlot::plot_model(bycluster.perplexity, type='int')
# sjPlot::plot_model(bycluster.perplexity, type='pred', terms=c('perplexity.improve'))
# sjPlot::plot_model(bycluster.perplexity, type='pred', pred.type = 're', terms=c('perplexity.improve', 'cluster')) +
#   scale_y_continuous() +
#   theme_minimal() +
#   labs(y="Estimated Marginal Mean (# Correct on Item, Per Cluster)",
#        x="Cluster-level Improvement in Item Perplexity versus Reference LM")
# 
# stat_df %>%
#   ggplot(aes(perplexity.improve, SpinSum)) +
#   geom_point() +
#   geom_smooth() +
#   facet_wrap(~Item, scales='free') +
#   theme_minimal()
# 
# summary(byitem.perplexity)
# 
# 
# #summary(byitem.perplexity)
# (p.wordrank <- sjPlot::plot_model(byitem.wordrank, type='re'))
# p.wordrank$data %>% 
#   rename(Item=term) %>%
#   left_join(stat_df) %>%
#   filter(facet!='Item (Intercept)') %>%
#   group_by(Predictability) %>%
#   arrange(estimate) %>%
#   mutate(Item=factor(Item, levels=unique(Item))) %>%
#   ggplot(aes(x=Item, y=estimate, ymin=conf.low, ymax=conf.high)) +
#   geom_hline(yintercept=1, linetype=1) +
#     geom_pointrange(aes(color=group), shape=15, show.legend = FALSE) +
#     coord_flip() +
#     facet_wrap(~Predictability, ncol=1, scales='free') +
#     scale_color_brewer(palette='Set1') +
#   labs(y="Log-Odds Ratio") +
#    theme(axis.title.y = element_blank())  +
#   scale_y_continuous(trans='log10') +
#   jtools::theme_nice() ->p.wordrank.left
# item_order <- levels(p.wordrank.left$data$Item)
# p.wordrank$data %>%
#   rename(Item=term) %>%
#   left_join(stat_df) %>%
#   mutate(Item=factor(Item, levels=item_order)) %>%
#   ggplot(aes(Item, y=wordrank.improve)) + 
#     geom_boxplot() +
#   coord_flip() +
#   geom_hline(yintercept=0, linetype=1) +
#   facet_wrap(~Predictability, ncol=1, scales='free_y') +
#   labs(y="WordRank Improvement Log-Ratio") +
#   theme(axis.title.y=element_blank()) +
#   jtools::theme_nice() -> p.wordrank.right
# 
# p.wordrank.left+p.wordrank.right
# library(patchwork)
# 
# (p.perplexity <- sjPlot::plot_model(byitem.perplexity, type='re'))
# p.perplexity$data %>% 
#   rename(Item=term) %>%
#   left_join(stat_df) %>%
#   filter(facet!='Item (Intercept)') %>%
#   group_by(facet, Predictability) %>%
#   arrange(estimate) %>%
#   mutate(Item=factor(Item, levels=unique(Item))) %>%
#   ggplot(aes(x=Item, y=estimate, ymin=conf.low, ymax=conf.high)) +
#   geom_hline(yintercept=1, linetype=1) +
#     geom_pointrange(aes(color=group), shape=15, show.legend = FALSE) +
#     coord_flip() +
#     facet_wrap(~Predictability, ncol=1, scales='free') +
#     scale_color_brewer(palette='Set1') +
#   labs(y="Log-Odds Ratio") +
#    theme(axis.title.y = element_blank())  +
#   scale_y_continuous(trans='log') +
#   jtools::theme_nice() ->p.perplexity.left
# item_order <- levels(p.perplexity.left$data$Item)
# p.perplexity$data %>%
#   rename(Item=term) %>%
#   left_join(stat_df) %>%
#   mutate(Item=factor(Item, levels=item_order)) %>%
#   ggplot(aes(Item, y=perplexity.improve)) + 
#     geom_boxplot() +
#   coord_flip() +
#   geom_hline(yintercept=0, linetype=1) +
#   facet_wrap(~Predictability, ncol=1, scales='free_y') +
#   labs(y="Perplexity Improvement Log-Ratio") +
#   theme(axis.title.y=element_blank()) +
#   jtools::theme_nice() -> p.perplexity.right
# 
# p.perplexity.left+p.perplexity.right
# 
# 
# 
# 
# participants <- read_tsv(file.path(output_dir_data, "cleaned_demo_behavioral.tsv")) %>%
#   convert(fct(cluster))
# 
# participants.add_lm_spin_high <- participants %>%
#   left_join(agg_lm_spin_high)
# participants.add_lm_spin_low <- participants %>%
#   left_join(agg_lm_spin_low)
# 
# spin.h.lm.base <- glm(High~1,
#                 family='poisson',
#                 data=participants.add_lm_spin_high)
# spin.h.lm.base_eng <- glm(High~1,
#                 family='poisson',
#                 data=participants.add_lm_spin_high %>% 
#                   filter(english_best=='Yes'))
# spin_formula_lm <- update(spin_formula, ~.-cluster+avg_lastword_rank)
# spin_formula_lm_eng <- update(spin_formula_eng, ~.-cluster+avg_lastword_rank)
# spin.h.lm <- update(spin.h.lm.base, spin_formula_lm)
# spin.h.lm_eng <- update(spin.h.lm.base_eng, spin_formula_lm_eng)
# ##### SPIN LOW #####
# spin.l.lm.base <- glm(Low~1,
#                 family='poisson',
#                 data=participants.add_lm_spin_low)
# spin.l.lm.base_eng <- glm(Low~1,
#                 family='poisson',
#                 data=participants.add_lm_spin_low %>% 
#                   filter(english_best=='Yes'))
# spin.l.lm <- update(spin.l.lm.base, spin_formula_lm)
# spin.l.lm_eng <- update(spin.l.lm.base_eng, spin_formula_lm_eng)
# 
# add1(spin.h.lm.base, spin_formula_lm, test="Chi")
# drop1(spin.h.lm, test="Chi")
# drop1(spin.h.lm_eng, test="Chi")
# 
# (spin.h.lm.table <- get_add1_table(spin.h.lm.base, spin_formula_lm) %>%
#     rename(add=lrt) %>%
#     left_join(get_drop1_table(spin.h.lm)) %>%
#     rename(drop=lrt) %>% 
#     filter(predictor!='<none>') %>%
#     left_join(get_drop1_table(spin.h.lm_eng)) %>%
#     rename(drop_eng=lrt) %>%
#     replace_na(list(drop_eng="---")))
# 
# add1(spin.l.lm.base, spin_formula_lm, test="Chi")
# drop1(spin.l.lm, test="Chi")
# drop1(spin.l.lm_eng, test="Chi")
# (spin.l.lm.table <- get_add1_table(spin.l.lm.base, spin_formula_lm) %>%
#     rename(add=lrt) %>%
#     left_join(get_drop1_table(spin.l.lm)) %>%
#     rename(drop=lrt) %>% 
#     filter(predictor!='<none>') %>%
#     left_join(get_drop1_table(spin.l.lm_eng)) %>%
#     rename(drop_eng=lrt) %>%
#     replace_na(list(drop_eng="---")))
# 
# ```