#=====================================================================

#' Set up iFixit Answers data
#'
#' Subsets iFixit Answers data to questions in English. Sets up time_until_answer (hrs) variable.
#'
#' @importFrom magrittr "%>%"
#' @return data frame
#' @export
setup <- function(){
  dir <- file.path(getwd(),"data")
  out <- read.csv(system.file("extdata/answers_data.csv", package = "oshitar"))

  x <- out %>%
    dplyr::tbl_df() %>%
    dplyr::filter(langid == "en")

  x$time_until_answer <- (x$first_answer_date - x$post_date)/3600
  empty <- which(is.na(x$time_until_answer))
  for (i in empty) {
    x$time_until_answer[i] <- (x$download_date[i] - x$post_date[i])/3600
  }
  return(x)
}

#=====================================================================

#' Compare nested models
#'
#' Performs partial likelihood ratio test for cox regression objects using evaluated log partial likelihoods from the full and nested models to determine if the inclusion of additional predictors in the model is necessary.
#'
#' @param full Cox regression object (from the function, coxph) for the full model
#' @param reduced Cox Regression object for the reduced model
#' @return data frame with the statistic and p-value
#' @export
compare_nested <- function(full, reduced) {
  l_f <- summary(full)[["loglik"]][2]
  l_r <- summary(reduced)[["loglik"]][2]
  stat <- 2*(l_f - l_r)

  df <- length(full[["coefficients"]]) - length(reduced[["coefficients"]])
  pval <- 1 - pchisq(stat, df)
  results <- data.frame(PLRT_statistic = stat, df = df, pvalue = pval)
  return(results)
}

#=====================================================================

#' Compute r-square adjusted
#'
#' Computes the r-square adjusted value for cox regression objects
#' @param cox Cox regression object to find the r-square adjusted value for
#' @param k Number of independent predictors in the model
#' @return r-square adjusted value
#' @export
get_rsq_adj <- function(cox, k) {
  r <- unname(summary(cox)$rsq[1])
  n <- summary(cox)$n
  radj <- 1 - (((1 - (r**2)) * (n - 1))/(n-k-1))
  return(radj)
}

#=====================================================================

#' Plots survival curves for survfit objects
#'
#' Takes in a survfit object as input. Uses tidy function from broom package to convert to data frame. Uses ggplot to plot Kaplan-Meier survival curves.
#' @param survfit survfit object to be used
#' @param xlim optional, specifies the x-axis limits of the returned survival plot
#' @importFrom broom tidy
#' @importFrom ggplot2 ggplot aes geom_line scale_y_continuous scale_x_continuous ggtitle
#' @importFrom directlabels geom_dl dl.trans
#' @export
get_survplot <- function(survfit, xlim = NULL) {
  df <- tidy(survfit)
  if (missing(xlim)) {
    if (!("strata" %in% names(df))) {
      ggplot(df, aes(x = time, y = estimate)) +
        geom_line() +
        scale_y_continuous("Survival Probability") +
        scale_x_continuous("Time (hours)")
      ggtitle("Survival Curve")
    }
    if ("strata" %in% names(df)) {
      ggplot(df, aes(x = time, y = estimate, color = strata)) +
        geom_line() +
        scale_y_continuous("Survival Probabilities") +
        scale_x_continuous("Time (hours)") +
        ggtitle("Survival Curves") +
        geom_dl(aes(label = strata), method = list(dl.trans(x = x + 0.2), "last.points", cex = 0.5))
    }
  } else {
    if (!("strata" %in% names(df))) {
      ggplot(df, aes(x = time, y = estimate)) +
        geom_line() +
        scale_y_continuous("Survival Probability") +
        scale_x_continuous("Time (hours)", limits = xlim)
      ggtitle("Survival Curve")
    }
    if ("strata" %in% names(df)) {
      ggplot(df, aes(x = time, y = estimate, color = strata)) +
        geom_line() +
        scale_y_continuous("Survival Probabilities") +
        scale_x_continuous("Time (hours)", limits = xlim) +
        ggtitle("Survival Curves") +
        geom_dl(aes(label = strata), method = list(dl.trans(x = x + 0.2), "last.points", cex = 0.5))
    }
  }
}

#=====================================================================
