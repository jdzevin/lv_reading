library(tidyverse)

source("src/R/analyze_study1.R")

cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

load_dataframes(min_per_cluster = 2)

study1_df %>%
  count(schooling_level)

generate_stacked_bar <- function(var, title_text, color_palette=cbPalette) {

  ggplot(study1_df %>%
    pivot_longer(cols=c(lda.cluster.id, types.cluster.id, items.cluster.id), 
                 names_to="ClusterMethod", 
                 values_to="ClusterAssignment") %>%
    mutate(ClusterMethod=factor(ClusterMethod, 
                                levels=c("items.cluster.id", "types.cluster.id", "lda.cluster.id")),
           ClusterMethod=dplyr::recode(ClusterMethod,
                                       items.cluster.id='Items',
                                       types.cluster.id='Counts',
                                       lda.cluster.id='Text'),
           ClusterAssignment=factor(ClusterAssignment)),
         aes(x = ClusterAssignment)) + 
    geom_bar(aes_string(fill = var), position = 'stack') +
    coord_flip() +
    scale_fill_manual(values=color_palette) +
    facet_grid(rows=vars(ClusterMethod), scales='free_y', space = 'free') +
    jtools::theme_apa(remove.y.gridlines = F) +
    labs(title = title_text,
         x = "Cluster Assignment") + 
    theme(legend.position = "bottom",
          axis.title.x = element_blank(),
          legend.text = element_text(size=10)) +
    guides(fill=guide_legend(reverse = TRUE)) 
  ggsave(paste("./figures/",splus2R::lowerCase(title_text),'.png'), dpi=300)
}

generate_stacked_bar("political_scale", "Political Scale")
generate_stacked_bar("english_best", "Is English Your Primary Language?")
generate_stacked_bar("race", "Self-Reported Race")
generate_stacked_bar("ethnicity", "Self-Reported Ethnicity")
generate_stacked_bar("schooling_level", "Schooling Level")


