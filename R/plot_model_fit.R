plot_model_fit = function(model,offset,name) {
  dd = model$data
  ofs = dd[,offset]
  rr = posterior_predict(model,
                         draws = 300,
                         offset= log(ofs),
                         newdata = dd) %>%
    t(.) %>%
    as.data.frame() %>%
    bind_cols(dd,.) %>%
    as_tibble() %>%
    group_by(ssep_d) %>%
    summarise_at(vars(starts_with("V")),sum) %>%
    pivot_longer(!ssep_d) %>%
    group_by(ssep_d) %>%
    summarise(
      med = median(value),
      lb = quantile(value,.025),
      ub = quantile(value,.975)) 
  names(rr)[2:5] = paste0(name,"_",names(rr)[2:5])
  return(rr)
}