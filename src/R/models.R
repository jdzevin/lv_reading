
fit_spin_models_study1 <- function(df) {
  
  spin.h.demo <<- lm(High~lang+subject_pool+gender+age+SES+schooling_level+race+ethnicity+political_interest+political_scale,  
                     data=df)
  spin.l.demo <<- update(spin.h.demo, Low~.)
  spin.h.types <<- lm(High~types.cluster.id, data=df)
  spin.l.types <<- lm(Low~types.cluster.id, data=df)
  spin.h.items <<- lm(High~items.cluster.id, data=df)
  spin.l.items <<- lm(Low~items.cluster.id, data=df)
  spin.h.lda <<- lm(High~lda.cluster.id, data=df)
  spin.l.lda <<- lm(Low~lda.cluster.id, data=df)
  
  spin.h.demo.types <<- update(spin.h.demo, .~.+types.cluster.id)
  spin.l.demo.types <<- update(spin.l.demo, .~.+types.cluster.id)
  spin.h.demo.items <<- update(spin.h.demo, .~.+items.cluster.id)
  spin.l.demo.items <<- update(spin.l.demo, .~.+items.cluster.id)
  spin.h.demo.lda <<- update(spin.h.demo, .~.+lda.cluster.id)
  spin.l.demo.lda <<- update(spin.l.demo, .~.+lda.cluster.id)
  
}

fit_span_models_study1 <- function(df) {

  span.demo <<- glm(span.count ~ english_best+subject_pool+gender+age+SES+
                  schooling_level+race+ethnicity+political_scale+political_interest,
              data=df, family=poisson)
  span.items <<- glm(span.count ~ items.cluster.id, data=study1_df, family=poisson)
  span.types <<- glm(span.count ~ types.cluster.id, data=study1_df, family=poisson)
  span.lda <<- glm(span.count ~ lda.cluster.id, data=study1_df, poisson)
  
  span.demo.types <<- update(span.demo, .~.+types.cluster.id)
  span.demo.items <<- update(span.demo, .~.+items.cluster.id)
  span.demo.lda <<- update(span.demo, .~.+lda.cluster.id)
  
}
