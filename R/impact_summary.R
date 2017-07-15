#' Present impacts summary objects as a model matrix
#' 
#' @param sumimpacts An object created from summary(impacts(...))
#' 
impact_summary <- function(sumimpacts){
  
  effects <- c("total_sum", "direct_sum", "indirect_sum")
  
  outlist <- lapply(effects, function(effect){
    coef <- sumimpacts[[effect]]$statistics[,1]
    se <- sumimpacts[[effect]]$statistics[,2]
    tstt <-  coef/se
    pval <- 2 * (1 - pnorm(abs(tstt)))
    
    stars <- symnum(pval, corr = FALSE, na = FALSE, 
                    cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
                    symbols = c("^{***}", "^{**}", "^*", "^\\dagger", " "))
    
    coef_table <- cbind(coef, se, tstt, pval)
    
    list(coefTable = coef_table, stars = stars)
  })
  
  names(outlist) <- effects
  
  return(outlist)
  
}
