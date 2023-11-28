.prcc = function(prcc.parameters, data){
  #Num_Inf PRCC model
  transNumInf<-data[c(prcc.parameters, "Num_Inf")]
  prcc.transNumInf<-epi.prcc(transNumInf, sided.test = 2)
  
  #Duration PRCC model
  transDur<-data[c(prcc.parameters, "Duration")]
  prcc.transDur<-epi.prcc(transDur, sided.test = 2)
  
  #naffCounties PRCC model
  transEpi<-data[c(prcc.parameters, "nAffCounties")]
  prcc.transEpi<-epi.prcc(transEpi, sided.test = 2)
  
  #Over PRCC model
  transOver<-data[c(prcc.parameters, "Over5000")]
  prcc.transOver<-epi.prcc(transOver, sided.test = 2)
  
  #Fadeout PRCC model
  transOut<-data[c(prcc.parameters, "Fade.Out")]
  prcc.transOut<-epi.prcc(transOut, sided.test = 2)
  
  prcc.trans <- cbind(NumInf = prcc.transNumInf, Dur = prcc.transDur$est, Epi = prcc.transEpi$est, Out = prcc.transOut$est, Over = prcc.transOver$est) %>%
    mutate(parameters = prccParameters)
  
  return(prcc.trans)
}

.linearModel = function(model, data, filter_pvalue = NULL){
  
  #Num_Inf: No interaction regression 
  model.infections = update.formula(model, Num_Inf ~ .)
  lm.infections.PRCC = lm(model.infections, data=data)
  
  lmNumInfScale<-lm.infections.PRCC %>%
    tidy() %>%
    filter(term != "(Intercept)") %>%
    mutate(values = estimate/max(abs(estimate), na.rm =TRUE)) %>%
    select(values, term, p.value) %>%
    column_to_rownames("term")
    #filter(!is.na(values)) %>%
    #mutate(variable = "NumInf")
  
  #Duration: No interaction regression 
  model.infections = update.formula(model, Duration ~ .)
  lm.infections.PRCC = lm(model.infections, data=data)
  
  lmDurationScale<-lm.infections.PRCC %>%
    tidy() %>%
    filter(term != "(Intercept)") %>%
    mutate(values = estimate/max(abs(estimate), na.rm =TRUE)) %>%
    select(values, term, p.value) %>%
    column_to_rownames("term")
    #filter(!is.na(values)) %>%
    #mutate(variable = "Dur")
  
  #Epi Extent: No interaction regression 
  model.infections = update.formula(model, nAffCounties ~ .)
  lm.infections.PRCC = lm(model.infections, data=data)
  
  lmEpiExtScale <- lm.infections.PRCC %>%
    tidy() %>%
    filter(term != "(Intercept)") %>%
    mutate(values = estimate/max(abs(estimate), na.rm =TRUE)) %>%
    select(values, term, p.value) %>%
    column_to_rownames("term")
    #filter(!is.na(values)) %>%
    #mutate(variable = "Epi")
  
  #Fade.Out: No interaction regression 
  model.infections = update.formula(model, Fade.Out ~ .)
  lm.infections.PRCC = glm(model.infections, data=data, family=binomial(link="logit"))
  
  lmFadeoutScale <- lm.infections.PRCC %>%
    tidy() %>%
    filter(term != "(Intercept)") %>%
    mutate(values = estimate/max(abs(estimate), na.rm = TRUE)) %>%
    select(values, term, p.value) %>%
    column_to_rownames("term")
    #filter(!is.na(values)) %>%
    #mutate(variable = "Out")
  
  #Over5000: No interaction regression 
  model.infections = update.formula(model, Over5000 ~ .)
  lm.infections.PRCC = glm(model.infections, data=data, family=binomial(link="logit"))
  
  lmOver5000Scale <- lm.infections.PRCC %>%
    tidy() %>%
    filter(term != "(Intercept)") %>%
    mutate(values = estimate/max(abs(estimate), na.rm = TRUE)) %>%
    select(values, term, p.value) %>%
    column_to_rownames("term")
    #filter(!is.na(values)) %>%
    #mutate(variable = "Over")
  
  
  if (is.null(filter_pvalue))  {
    
    lmScale <- cbind(NumInf = lmNumInfScale, Dur = lmDurationScale, Epi = lmEpiExtScale, Out = lmFadeoutScale, Over = lmOver5000Scale)
    
    # lmScale <- bind_rows(lmNumInfScale, lmDurationScale, lmEpiExtScale, lmFadeoutScale, lmOver5000Scale) %>%
    #   dplyr::rename(parameters = term, value = values)
    
  } else {
    
    lmScale <- bind_rows(lmNumInfScale, lmDurationScale, lmEpiExtScale, lmFadeoutScale, lmOver5000Scale)

    lmScale <- lmScale %>%
      filter(p.value < filter_pvalue )
    
  }
  
  return(lmScale)
}

# test.model = formula(RESPONSE ~ priority * priority2 +
#                          priority * threshold +
#                          priority * threshold2 +
#                          priority * target +
#                          priority * target2 +
#                          priority2 * threshold +
#                          priority2 * threshold2 +
#                          priority2 * target +
#                          priority2 * target2 +
#                          threshold * threshold2 +
#                          threshold * target +
#                          threshold * target2 +
#                          threshold2 * target +
#                          threshold2 * target2 +
#                          out.shipments * target +
#                          out.shipments * target2 +
#                          out.shipments * threshold +
#                          out.shipments * threshold2 +
#                          out.shipments * priority +
#                          out.shipments * priority2 +
#                          out.shipments * in.shipments +
#                          out.shipments * clustering +
#                          out.shipments * density +
#                          out.shipments * premises.size +
#                          out.shipments * num.large.prems +
#                          in.shipments * target +
#                          in.shipments * target2 +
#                          in.shipments * threshold +
#                          in.shipments * threshold2 +
#                          in.shipments * priority +
#                          in.shipments * priority2 +
#                          in.shipments * clustering +
#                          in.shipments * density +
#                          in.shipments * premises.size +
#                          in.shipments * num.large.prems +
#                          clustering * target +
#                          clustering * target2 +
#                          clustering * threshold +
#                          clustering * threshold2 +
#                          clustering * priority +
#                          clustering * priority2 +
#                          clustering * density +
#                          clustering * premises.size +
#                          clustering * num.large.prems +
#                          density * target +
#                          density * target2 +
#                          density * threshold +
#                          density * threshold2 +
#                          density * priority +
#                          density * priority2 +
#                          density * premises.size +
#                          density * num.large.prems +
#                          premises.size * target +
#                          premises.size * target2 +
#                          premises.size * threshold +
#                          premises.size * threshold2 +
#                          premises.size * priority +
#                          premises.size * priority2 +
#                          premises.size * num.large.prems +
#                          num.large.prems * target +
#                          num.large.prems * target2 +
#                          num.large.prems * threshold +
#                          num.large.prems * threshold2 +
#                          num.large.prems * priority +
#                          num.large.prems * priority2)
