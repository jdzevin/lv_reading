
library(RColorBrewer)
#cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cbPalette <- brewer.pal(8, 'Set2')
#diverge_palette <- c("#8c510a", "#d8b365", "#f6e8c3", "#ffffbf", "#c7eae5", "#5ab4ac", "#01665e")
diverge_palette <- brewer.pal(8, 'PuBu')
library(extrafont)

generate_stacked_bar <- function(df, var, title_text, out_path, is_ordinal=FALSE,
                                 color_palette=cbPalette) {

  plot_df <- df %>%
      mutate(ClusterAssignment=factor(cluster, levels=rev(sort(unique(cluster)))))
  agg_df <- plot_df %>%
    group_by(ClusterAssignment) %>%
    mutate(cluster_size=n()) %>%
    ungroup() %>%
    #group_by(across(c("ClusterAssignment", var, "subject_pool")))
    group_by(across(c("ClusterAssignment", var)))
  agg_df <- agg_df %>%
    summarize(freq=n(),
              with_cluster_prop=n()/cluster_size) %>%
    distinct(.keep_all = T)
  p <- ggplot(agg_df, aes_string("ClusterAssignment", fill=var)) + 
    geom_bar(aes(y=with_cluster_prop), color='black',
             size=0.1, stat='identity', position = 'fill') +
    #facet_grid(cols = vars(subject_pool)) +
    geom_text(aes(label=freq,
                  y=with_cluster_prop),
               #fill='white',
                  family='serif', 
                  color='black',
                  fontface='bold',
                  size=2.5,
              position=position_fill(vjust=0.5)) +
    coord_flip() +
    jtools::theme_apa(remove.y.gridlines = F) +
    labs(x = "Cluster Assignment")  +
    theme(legend.key.height=unit(.4, 'cm'),
          axis.title.x = element_blank(),
          text = element_text(family='serif', color='black', size=12),
          legend.text = element_text(size=10),
          strip.background=element_blank(),
          strip.text.y = element_text(size=11)) +
    scale_y_continuous(labels = scales::percent)
  
  p <- p + 
    guides(fill=guide_legend(title=title_text, reverse = TRUE)) +
    scale_fill_brewer(type='seq', palette='RdGy') 

  ggsave(plot=p, filename=out_path, dpi=300, width = 4, height=2, units = "in")
  return(p)
}

plot_study1_emm <- function() {
  
  extract_emm_df <- function(target, cluster_method) {
    lm_fit <- get(paste(target, cluster_method, sep="."))
    emm <- emmeans(lm_fit, as.formula(paste("~", cluster_method, ".cluster.id", sep='')))
    
    if(target == 'span') plot.df <- plot(emm, horizontal=T, CIs=T, plotit=F, type='response')
    else  plot.df <- plot(emm, horizontal=T, CIs=T, plotit=F)
    
    plot.df$method <-  cluster_method
    if(target == "span") plot.df$target <- "SPAN"
    if(target == 'spin.h') plot.df$target = "SPiN[HIGH]"
    if(target == 'spin.l') plot.df$target = "SPiN[LOW]"
    plot.df <- plot.df %>%
      rename(cluster=paste(cluster_method, ".cluster.id", sep=''))
    if(target == "span") {
      plot.df <- plot.df %>%
        rename(lower.CL="asymp.LCL",
               upper.CL="asymp.UCL")
    }
    return(plot.df)
  }

  emm.df <- extract_emm_df("spin.h", "types") %>%
      rbind(extract_emm_df("spin.l", "types")) %>%
      rbind(extract_emm_df("spin.h", "items")) %>%
      rbind(extract_emm_df("spin.l", "items")) %>%
      rbind(extract_emm_df("spin.h", "lda")) %>%
      rbind(extract_emm_df("spin.l", "lda")) %>%
      rbind(extract_emm_df("span", "types")) %>%
      rbind(extract_emm_df("span", "items")) %>%
      rbind(extract_emm_df("span", "lda"))
  
  (p <- ggplot(emm.df %>%
              mutate(method=recode_factor(method, lda="Text", types="Type~Counts", items="Item~Indicators"),
                     method=factor(method, levels=rev(levels(method))),
                     cluster=factor(cluster, levels=rev(sort(levels(cluster))))),
            aes(x=the.emmean, y=cluster)) +
        geom_point() +
        geom_errorbar(aes(xmin=lower.CL, xmax=upper.CL), width=.3) +
        facet_grid(method~target, scales = 'free', labeller=label_parsed) +
        jtools::theme_apa(remove.x.gridlines = F) + 
        ylab("Cluster Assignment") + xlab("Estimated Marginal Means") + 
        theme(strip.text.x = element_text(size=14),
              strip.text.y = element_text(size=14),
              axis.text = element_text(size=11),
              axis.title.x = element_text(size=14),
              axis.title.y = element_text(size=14)) +
        theme(panel.spacing.x = unit(6, "mm")))
  return(p)
  #ggsave("./figures/study1_emm.png", dpi=300)
}



