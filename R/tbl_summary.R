tbl_summary = function(model,to.print=FALSE) {
  r = summary(model,probs = c(0.025, 0.5, 0.975)) %>%
    as.data.frame() %>%
    rownames_to_column() %>%
    as_tibble() %>%
    dplyr::mutate(par=rowname,
                  RR=exp(`50%`),
                  lb=exp(`2.5%`),
                  ub=exp(`97.5%`),
                  increase_text=paste0(sprintf("%.1f",100*(RR-1)),"% (95%CrI: ",sprintf("%.1f",100*(lb-1))," to ",sprintf("%.1f",100*(ub-1)),")")) %>%
    dplyr::select(par,RR,lb,ub,n_eff,Rhat,increase_text) %>%
    dplyr::filter(!(par %in% c("(Intercept)","mean_PPD","log-posterior","reciprocal_dispersion")))
  if(to.print) r = dplyr::select(r,par,increase_text)
  return(r)
}