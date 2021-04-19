compute_linear_IRR = function(model) {
  alpha = as.matrix(model,pars="(Intercept)")
  beta_1 = as.matrix(model,pars="ssep_d")
  beta_2 = 0
  if(any(grepl("ssep_d:periodJuly to October",names(model$coefficients)))) {
    beta_2 = as.matrix(model,pars="ssep_d:periodJuly to October")
  } 
  
  rr = NULL
  for(i in 1:10) {
    rr = bind_rows(rr,
                   tibble(ssep_d=i,
                          per1_med=median(exp(beta_1*i)),
                          per1_lb=quantile(exp(beta_1*i),.025),
                          per1_ub=quantile(exp(beta_1*i),.975),
                          per2_med=median(exp(beta_1*i + beta_2*i)),
                          per2_lb=quantile(exp(beta_1*i + beta_2*i),.025),
                          per2_ub=quantile(exp(beta_1*i + beta_2*i),.975)))
    
  }
  return(rr)
}