
print_study1_spin_results <- function() {
  fit_spin_models_study1()
  model_row <- function(m, inner_model=NULL) {
    
    if(is.null(inner_model)) {
      f_stat <- summary(m)$fstatistic
      p_value <- pf(f_stat[1],f_stat[2],f_stat[3],lower.tail=FALSE)
      f_value <- sprintf("%.2f", round(f_stat[1], 2))
      df1 <- f_stat[2]
      df2 <- f_stat[3]
    }
    else {  # model comparison
      aov_comp <- anova(inner_model, m, test='F')
      df2 <- aov_comp["Res.Df"][[1]][2]
      df1 <- aov_comp["Df"][[1]][2]
      f_value <- sprintf("%.2f", round(aov_comp["F"][[1]][2], 2))
      p_value <- aov_comp["Pr(>F)"][[1]][2]
    }
    
    if(p_value >= 0.05) {
      f_stat_str <- paste("{F$_{", df1, ",", df2, "}$ = }", f_value, "{", gtools::stars.pval(p_value), "}", sep="")
    }
    else {
      f_stat_str <- paste("{F$_{", df1, ",", df2, "}$ = }", f_value, "{$^{", gtools::stars.pval(p_value), "}$}", sep="")
    }
    radj <- sprintf("%.2f", round(summary(m)$adj.r.squared, 2))
    aic_str <- sprintf("%.2f", round(AIC(m), 2))
    return(c(f_stat_str, radj, aic_str))
  }
  table_rows <- list(c("Demographics", model_row(spin.h.demo), model_row(spin.l.demo)),
                     c("Clusters$_{\\text{items}}$", model_row(spin.h.items), model_row(spin.l.items)), 
                     c("Clusters$_{\\text{types}}$", model_row(spin.h.types), model_row(spin.l.types)),
                     c("Clusters$_{\\text{LDA}}$", model_row(spin.h.lda), model_row(spin.l.lda)),
                     c("Demographics + Clusters$_{\\text{items}}$", model_row(spin.h.demo.items, spin.h.demo), model_row(spin.l.demo.items, spin.l.demo)),
                     c("Demographics + Clusters$_{\\text{types}}$", model_row(spin.h.demo.types, spin.h.demo), model_row(spin.l.demo.types, spin.l.demo)),
                     c("Demographics + Clusters$_{\\text{LDA}}$", model_row(spin.h.demo.lda, spin.h.demo), model_row(spin.l.demo.lda, spin.l.demo)))
  
  table_df <- as.data.frame(do.call(rbind, table_rows))
  names(table_df) <- c("", "$F$", "R$_{adj}^{2}$", "AIC", "$F$", "R$_{adj}^{2}$", "AIC")
  print(xtable(table_df, type='latex'), 
        sanitize.text.function = function(x) {x},
        include.rownames=F)
}



print_study1_span_results <- function() {
  nagelkerke(span.demo)
  nagelkerke(span.items)
  nagelkerke(span.types)
  nagelkerke(span.lda)
  
  AIC(span.demo, span.items, span.types, span.lda)
  AIC(span.demo, span.demo.items, span.demo.types, span.demo.lda)
  
  anova(span.demo, span.demo.items, test="Chisq")
  anova(span.demo, span.demo.types, test="Chisq")
  anova(span.demo, span.demo.lda, test="Chisq")
    
}