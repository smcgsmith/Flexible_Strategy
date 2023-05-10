.prcc = function(prcc.parameters, data){
  #Num_Inf PRCC model
  transNumInf<-usdos_scaled_subset[c(prcc.parameters, "Num_Inf")]
  prcc.transNumInf<-epi.prcc(transNumInf, sided.test = 2)
  
  #Duration PRCC model
  transDur<-usdos_scaled_subset[c(prcc.parameters, "Duration")]
  prcc.transDur<-epi.prcc(transDur, sided.test = 2)
  
  #naffCounties PRCC model
  transEpi<-usdos_scaled_subset[c(prcc.parameters, "nAffCounties")]
  prcc.transEpi<-epi.prcc(transEpi, sided.test = 2)
  
  #Over PRCC model
  transOver<-usdos_scaled_subset[c(prcc.parameters, "Over5000")]
  prcc.transOver<-epi.prcc(transOver, sided.test = 2)
  
  #Fadeout PRCC model
  transOut<-usdos_scaled_subset[c(prcc.parameters, "Fade.Out")]
  prcc.transOut<-epi.prcc(transOut, sided.test = 2)
  
  prcc.trans <- cbind(NumInf = prcc.transNumInf, Dur = prcc.transDur$est, Epi = prcc.transEpi$est, Out = prcc.transOut$est, Over = prcc.transOver$est) %>%
    mutate(parameters = prccParameters)
  
  return(prcc.trans)
}

.linearModel = function(model, data){
  
  #Num_Inf: No interaction regression 
  model.infections = update.formula(model, Num_Inf ~ .)
  lm.infections.PRCC = lm(model.infections, data=usdos_scaled_subset)
  
  lmNumInfScale<-lm.infections.PRCC %>%
    tidy() %>%
    filter(term != "(Intercept)") %>%
    mutate(values = estimate/max(abs(estimate), na.rm =TRUE)) %>%
    select(values, term) %>%
    filter(!is.na(values)) %>%
    column_to_rownames("term")
  
  #Duration: No interaction regression 
  model.infections = update.formula(model, Duration ~ .)
  lm.infections.PRCC = lm(model.infections, data=usdos_scaled_subset)
  
  lmDurationScale<-lm.infections.PRCC %>%
    tidy() %>%
    filter(term != "(Intercept)") %>%
    mutate(values = estimate/max(abs(estimate), na.rm =TRUE)) %>%
    select(values, term) %>%
    filter(!is.na(values)) %>%
    column_to_rownames("term")
  
  #Epi Extent: No interaction regression 
  model.infections = update.formula(model, nAffCounties ~ .)
  lm.infections.PRCC = lm(model.infections, data=usdos_scaled_subset)
  
  lmEpiExtScale <- lm.infections.PRCC %>%
    tidy() %>%
    filter(term != "(Intercept)") %>%
    mutate(values = estimate/max(abs(estimate), na.rm =TRUE)) %>%
    select(values, term) %>%
    filter(!is.na(values)) %>%
    column_to_rownames("term")
  
  #Fade.Out: No interaction regression 
  model.infections = update.formula(model, Fade.Out ~ .)
  lm.infections.PRCC = glm(model.infections, data=usdos_scaled_subset, family=binomial(link="logit"))
  
  lmFadeoutScale <- lm.infections.PRCC %>%
    tidy() %>%
    filter(term != "(Intercept)") %>%
    mutate(values = estimate/max(abs(estimate), na.rm =TRUE)) %>%
    select(values, term) %>%
    filter(!is.na(values)) %>%
    column_to_rownames("term")
  
  #Over5000: No interaction regression 
  model.infections = update.formula(model, Over5000 ~ .)
  lm.infections.PRCC = glm(model.infections, data=usdos_scaled_subset, family=binomial(link="logit"))
  
  lmOver5000Scale <- lm.infections.PRCC %>%
    tidy() %>%
    filter(term != "(Intercept)") %>%
    mutate(values = estimate/max(abs(estimate), na.rm =TRUE)) %>%
    select(values, term) %>%
    filter(!is.na(values)) %>%
    column_to_rownames("term")
  
  lmScale <- cbind(NumInf=lmNumInfScale, Dur=lmDurationScale, Epi=lmEpiExtScale, Out=lmFadeoutScale, Over=lmOver5000Scale) %>%
    set_names("NumInf","Dur","Epi","Over","Out")
  
  return(lmScale)
}
