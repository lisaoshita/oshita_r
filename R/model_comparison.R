#' Compares nested models using partial log-likelihood
#' @param crfull Cox Regression object for the full model
#' @param crreduced Cox Regression object for the reduced model
#' @return data frame with the statistic and p-value
#' @export
compare_nested <- function(crfull, crreduced) {
  loglik_f <- summary(crfull)[["loglik"]][2]
  loglik_r <- summary(crreduced)[["loglik"]][2]
  stat <- 2*(loglik_f - loglik_r)

  full <- length(crfull[["coefficients"]])
  reduced <- length(crreduced[["coefficients"]])
  df <- full - reduced
  pval <- 1 - pchisq(stat, df)
  results <- data.frame(statistic = stat, pval = pval)
  return(results)
}
