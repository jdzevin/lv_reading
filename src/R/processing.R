
library(tidyverse)
library(hablar)


parse_lm_data <- function(
  path,
  grouping_variable
) {
  if(grouping_variable=='cluster_id') {
    cluster_data <- readr::read_csv(path, 
                                    col_types = list(cluster_id=col_factor()))
  }
  else {
    cluster_data <- readr::read_csv(path) %>%
      rename(qualtrics_id=grouping_variable)
    grouping_variable = "qualtrics_id"
  }
  
  spin <- cluster_data %>%
    dplyr::select(all_of(grouping_variable), starts_with('spin'))
  
  (spin_topwords <- spin %>%
      dplyr::select(all_of(grouping_variable), contains("candidate")) %>%
      pivot_longer(cols=contains('candidate'), names_to="meta_raw", values_to="lastword_rank") %>%
      mutate(spin_item_num=gsub('spin_', '', meta_raw),
             spin_item_num=gsub('_candidate_[0-9]+', '', spin_item_num),
             rank_num=gsub('spin_[0-9]+_candidate_', '', meta_raw)) %>%
      convert(int(spin_item_num, rank_num)) %>%
      select(-meta_raw))
  (spin_lastword_rank <- spin %>%
      dplyr::select(all_of(grouping_variable), ends_with("last_word_rank")) %>%
      pivot_longer(cols=ends_with('last_word_rank'), names_to="meta_raw", values_to="lastword_rank") %>%
      mutate(spin_item_num=gsub('spin_', '', meta_raw),
             spin_item_num=gsub('_last_word_rank', '', spin_item_num)) %>%
      convert(int(spin_item_num, lastword_rank)) %>%
      select(-meta_raw))
  
  spin_perplexity <- spin %>%
    dplyr::select(all_of(grouping_variable), ends_with("perplexity")) %>%
      pivot_longer(cols=ends_with('perplexity'), names_to="spin_item_num", values_to="perplexity") %>%
      mutate(spin_item_num=gsub('spin_item_', '', spin_item_num),
             spin_item_num=gsub('_perplexity', '', spin_item_num)) %>%
      convert(int(spin_item_num))

  spin_ll <- spin %>%
    dplyr::select(all_of(grouping_variable), ends_with("likelihood")) %>%
      pivot_longer(cols=ends_with('likelihood'), names_to="spin_item_num", values_to="likelihood") %>%
      mutate(spin_item_num=gsub('spin_', '', spin_item_num),
             spin_item_num=gsub('_last_word_loglikelihood', '', spin_item_num)) %>%
      convert(int(spin_item_num))

  span <- cluster_data %>%
    dplyr::select(all_of(grouping_variable), starts_with('span'))
  span_topwords <- span %>%
      dplyr::select(all_of(grouping_variable), contains("candidate")) %>%
      mutate_all(funs(as.character(.))) %>%
      pivot_longer(cols=contains('candidate'), names_to="meta_raw", values_to="lastword_rank") %>%
      mutate(span_item_num=gsub('span_', '', meta_raw),
             span_item_num=gsub('_candidate_[0-9]+', '', span_item_num),
             rank_num=gsub('span_[0-9]+_candidate_', '', meta_raw)) %>%
      convert(int(span_item_num, rank_num)) %>%
      select(-meta_raw)
  span_lastword_rank <- span %>%
      dplyr::select(all_of(grouping_variable), ends_with("last_word_rank")) %>%
      pivot_longer(cols=ends_with('last_word_rank'), names_to="meta_raw", values_to="lastword_rank") %>%
      mutate(span_item_num=gsub('span_', '', meta_raw),
             span_item_num=gsub('_last_word_rank', '', span_item_num)) %>%
      convert(int(span_item_num, lastword_rank)) %>%
      select(-meta_raw)
  
  span_perplexity <- span %>%
    dplyr::select(all_of(grouping_variable), ends_with('perplexity')) %>%
      pivot_longer(cols=ends_with('perplexity'), names_to="span_item_num", values_to="perplexity") %>%
      mutate(span_item_num=gsub('span_item_', '', span_item_num),
             span_item_num=gsub('_perplexity', '', span_item_num)) %>%
      convert(int(span_item_num))

  span_ll <- span %>%
    dplyr::select(all_of(grouping_variable), ends_with('likelihood')) %>%
      pivot_longer(cols=ends_with('likelihood'), names_to="span_item_num", values_to="likelihood") %>%
      mutate(span_item_num=gsub('span_', '', span_item_num),
             span_item_num=gsub('_last_word_loglikelihood', '', span_item_num)) %>%
      convert(int(span_item_num))

  
  span_full <- span_perplexity %>%
    inner_join(span_ll, by=c(grouping_variable, "span_item_num")) %>% 
    inner_join(span_lastword_rank, by=c(grouping_variable, "span_item_num"))
  spin_full <- spin_perplexity %>%
    inner_join(spin_ll, by=c(grouping_variable, "spin_item_num")) %>% 
    inner_join(spin_lastword_rank, by=c(grouping_variable, "spin_item_num"))


  #spin_stimuli <- readr::read_csv(here("./output/data/spin_stimuli.csv")) %>%
    #janitor::clean_names() %>%
    #rename(spin_item_num=item_no) %>%
    #mutate(spin_item_num=spin_item_num-1)
  return(list(span=span_full,
              spin=spin_full))

}


#common_langs <- full_df %>%
  #count(language.clean) %>%
  #slice_max(order_by = n, n=7, with_ties=F) %>%
  #pull(language.clean)
#full_df <- full_df %>%
  #mutate(language.reduced=if_else(language.clean %in% common_langs, language.clean, "Other"))
