#-------------------------------------------------------------------------------
#' Set up iFixit Answers data for exploratory analysis
#'
#' The function was used to set up all variables for exploratory analysis including variables not
#' included in the final cox regression model (e.g. num_freq_tags, text_till_punct...). Takes no parameters.
#'
#' @importFrom magrittr "%>%"
#' @importFrom stringr str_detect str_to_lower str_length str_count str_split str_replace_all str_locate
#' @importFrom rebus "%R%" START SPC QUESTION END or1 or
#' @importFrom dplyr group_by summarise n arrange filter desc
#'
#' @return Returns Answers data.
#'
#' @export

exploratory_setup <- function(){
  dir <- file.path(getwd(),"data")
  out <- read.csv(system.file("extdata/answers_data.csv", package = "oshitar"))
  #----Subset to English----------------------------------------
  x <- out %>%
    tibble::as.tibble() %>%
    filter(langid == "en")

  #----Create time_until_answer---------------------------------
  x$time_until_answer <- (x$first_answer_date - x$post_date) / 3600
  empty <- is.na(x$time_until_answer)
  x$time_until_answer[empty] <- (x$download_date[empty] - x$post_date[empty])/3600

  #----Convert factors to characters----------------------------
  x <- dplyr::mutate_if(x, is.factor, as.character)

  #----Recoding NAs---------------------------------------------
  x$category[is.na(x$category)] <- "Other"
  x$subcategory[is.na(x$subcategory)] <- "Other"

  #----Creates new_category-------------------------------------
  x$new_category <- x$category

  apple_terms <- c("apple", "ipod", "ipad") # grouping apple products
  x$apple <- str_detect(str_to_lower(x$device), pattern = START %R% or1(apple_terms) %R% SPC)
  x$new_category[x$apple == TRUE | x$subcategory == "iPhone" | x$category == "Mac"] <- "Apple Product"
  x$new_category[x$new_category == "Phone"] <- "Android/Other Phone" # renaming left-over phones

  x$new_category[x$new_category == "Appliance" | x$new_category == "Household"] <- "Home"
  x$new_category[x$new_category == "Car and Truck" | x$new_category == "Vehicle"] <- "Vehicle"

  x$new_category <- forcats::fct_lump(as.factor(x$new_category), prop = 0.02)

  #----new_user-------------------------------------------------
  x$new_user <- as.factor(x$new_user)

  #----weekday--------------------------------------------------
  x$datetime <- as.POSIXct(x$post_date,origin="1970-01-01")
  x$weekday <- factor(weekdays(x$datetime), levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

  #----ampm/hour------------------------------------------------
  x$hour <- as.numeric(format(x$datetime,"%H"))
  x$ampm <- "Night"
  x$ampm[x$hour >= 5 & x$hour < 12] <- "Morning"
  x$ampm[x$hour >= 12 & x$hour < 17] <- "Afternoon" #noon - 5pm
  x$ampm[x$hour >= 17 & x$hour < 20] <- "Evening" #5pm - 8pm

  #----text length----------------------------------------------
  x$text_length <- str_length(x$text)

  #----device_length--------------------------------------------
  x$device_length <- str_length(x$device)

  #----title_length---------------------------------------------
  x$title_length <- str_length(x$title)

  #----if the title ends with a "?"-----------------------------
  x$title_questionmark <- str_detect(x$title, pattern = QUESTION %R% END)

  #----if title begins with "wh"--------------------------------
  x$title_beginwh <- str_detect(str_to_lower(x$title), pattern = "^wh")

  #----if text is in all lower case-----------------------------
  cleaned <- str_replace_all(x$text, " ", "")
  cleaned <- str_replace_all(cleaned, "[[:punct:]]|[[:digit:]]", "")
  x$text_all_lower <- str_detect(cleaned, pattern = "^[[:lower:]]+$")

  #----length of text until first end punct.--------------------
  length <- str_locate(x$text, pattern = "[.|?|!]")[,1]
  q <- stats::quantile(length, probs = seq(0, 1, by = 0.25), na.rm = TRUE)
  x$text_till_punct <- "none"
  x$text_till_punct[length <= q[3]] <- "short"
  x$text_till_punct[length >= q[3] & length <= q[4]] <- "medium"
  x$text_till_punct[length >= q[4]] <- "long"

  #----if text contains any end punct.-------------------------
  x$text_contain_punct <- str_detect(x$text, pattern = "[.|?|!]")

  #----if user updated question--------------------------------
  x$update <- str_detect(x$text, pattern = "===")

  #----if user showed any prior effort-------------------------
  x$prior_effort <- str_detect(str_to_lower(x$text),
                               pattern = or("tried", "searched", "researched", "tested",
                                            "replaced", "used", "checked", "investigated",
                                            "considered", "measured", "attempted", "inspected", "fitted"))

  #----if user showed any gratitude----------------------------
  x$gratitude <- str_detect(str_to_lower(x$text),
                            pattern = or("please", "thank you", "thanks", "thankful",
                                         "appreciate", "appreciated", "grateful"))

  #----if user included a greeting-----------------------------
  x$greeting <- str_detect(str_to_lower(x$text), pattern = START %R% or("hey", "hello", "greetings", "hi"))

  #----newline ratio to length of text-------------------------
  x$newline_ratio <- str_count(x$text, pattern = "\n")/str_length(x$text)

  #----average tag length--------------------------------------
  taglist <- oshitar::tag_frequency(x$tags)
  split_tags <- taglist[[1]]
  tag_freq <- taglist[[2]]

  x$avg_tag_length <- 0
  for (j in which(x$n_tags != 0)) {
    x$avg_tag_length[j] <- sum(str_length(as.vector(split_tags[j,]))) / sum(as.vector(split_tags[j,]) != "")
  }

  #----freqency of tags----------------------------------------
  score1 <- oshitar::assign_tag_freq(split_tags[,1], tag_freq)
  score2 <- oshitar::assign_tag_freq(split_tags[,2], tag_freq)
  score3 <- oshitar::assign_tag_freq(split_tags[,3], tag_freq)
  score4 <- oshitar::assign_tag_freq(split_tags[,4], tag_freq)

  x$avg_tag_score <- 0
  hastag <- which(x$n_tags != 0)
  x$avg_tag_score[hastag] <- (score1[hastag] + score2[hastag] + score3[hastag] + score4[hastag]) / as.numeric(x$n_tags[hastag])

  # number of "frequent" tags a question contains
  threshold <- 0.005
  n_freq <- function(var, threshold) {
    num_freq <- rep(0, nrow(x))
    num_freq[var >= threshold] <- 1
    return(num_freq)
  }
  freq1 <- n_freq(score1, threshold); freq2 <- n_freq(score2, threshold)
  freq3 <- n_freq(score3, threshold); freq4 <- n_freq(score4, threshold)
  x$num_freq_tags <- as.factor(freq1 + freq2 + freq3 + freq4)

  #----frequent terms in answered/unanswered questions--------
  remove <- c(unique(as.character(x$category)), unique(as.character(x$subcategory)),
              unique(as.character(x$new_category)), "macbook", "imac", "ipod")

  au_terms <- oshitar::get_au_terms(x, "title", stopwords = c("can", "will", "cant", "wont",
                                                              "works", "get", "help", "need",
                                                              "fix", "doesnt", "dont"), remove = remove)
  p <- 0.01
  r <- 1
  answeredTerms <- au_terms %>%
    filter(prop_in_answered > p) %>% filter(ratio > r)

  unansweredTerms <- au_terms %>%
    filter(prop_in_unanswered > p) %>% filter(ratio < r)

  x$contain_answered <- str_detect(str_to_lower(as.character(x$title)), pattern = or1(answeredTerms$word))
  x$contain_unanswered <- str_detect(str_to_lower(as.character(x$title)), pattern = or1(unansweredTerms$word))

  return(x)
}

#-------------------------------------------------------------------------------

#' Compare nested models
#'
#' Performs partial likelihood ratio test for cox regression objects using evaluated
#' log partial likelihoods from the full and nested models to determine if the inclusion
#' of additional predictors in the model is necessary. This function only works for coxph fits.
#'
#' @param full_model Cox regression object (from the function, coxph) for the full model.
#' @param reduced_model Cox Regression object for the reduced model.
#'
#' @return data frame with the statistic, degrees of freedom and p-value.
#'
#' @export

compare_nested <- function(full_model, reduced_model) {
  full <- summary(full_model)
  reduced <- summary(reduced_model)
  stat <- 2*(full[["loglik"]][2] - reduced[["loglik"]][2])
  df <- nrow(full[["coefficients"]]) - nrow(reduced[["coefficients"]])
  pval <- 1 - pchisq(stat, df)
  results <- data.frame(PLRT = stat, df = df, pvalue = pval)
  return(results)
}

#-------------------------------------------------------------------------------

#' Plots survival curves for survfit objects
#'
#' Uses tidy function from broom package to convert survfit object to a data frame. Uses ggplot to
#' plot Kaplan-Meier survival curves.
#'
#' @param survfit survfit object to be used
#' @param xlim optional, specifies the x-axis limits of the returned survival plot
#'
#' @importFrom ggplot2 ggplot aes geom_line scale_y_continuous scale_x_continuous ggtitle
#' @importFrom directlabels geom_dl dl.trans
#'
#' @return survival curves plotted with ggplot
#'
#' @export

get_survplot <- function(survfit, xlim = NULL) {
  df <- broom::tidy(survfit) # convert to df
  if (missing(xlim)) {
    if (!("strata" %in% names(df))) {
      ggplot(df, aes(x = time, y = estimate)) +
        geom_line() +
        scale_y_continuous("Survival Probability") +
        scale_x_continuous("Time (hours)") +
        ggtitle("Survival Curve")
    }
    else if ("strata" %in% names(df)) {
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
        scale_x_continuous("Time (hours)", limits = xlim) +
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

#-------------------------------------------------------------------------------

#' Performs an iteration of cross-validation
#'
#' Fits a Cox proportional hazards model to the given training set and predicts on the input
#' test set. Performance metrics for predicted hazard ratios for training and test sets are calculated.
#'
#' @param variables a string of the variables to use in the model ("new_user + text_length + ...")
#' @param train training set to build the model on
#' @param test test set to predict on
#'
#' @importFrom rms cph rcs
#' @importFrom stats predict
#' @importFrom survival survConcordance
#'
#' @return data frame with prediction performance metrics for the training and test set
#'
#' @export

cv <- function(variables, train, test) {
  formula <- paste("Surv(time_until_answer, answered) ~ ", variables, sep = "")
  model <- cph(stats::as.formula(formula), data = train)

  train[["predictions"]] <- exp(predict(model, type = "lp"))
  metric <- cph(Surv(time_until_answer, answered) ~ predictions, data = train)
  train_metrics <- data.frame(HR = exp(metric$coefficients),
                              LR = round(metric$stats[3],2),
                              pval = round(metric$stats[5],2),
                              R2 = round(metric$stats[8], 2),
                              AIC = stats::AIC(metric, k = 2),
                              Dxy = round(metric$stats[9],2),
                              Concordance = survConcordance(Surv(time_until_answer, answered) ~ predictions,
                                                            data = train)$concordance)
  test[["predictions"]] <- exp(predict(model, newdata = test, type = "lp"))
  metric1 <- rms::cph(Surv(time_until_answer, answered) ~ predictions, data = test)
  test_metrics <- data.frame(HR = exp(metric1$coefficients),
                             LR = round(metric1$stats[3],2),
                             pval = round(metric1$stats[5],2),
                             R2 = round(metric$stats[8], 2),
                             AIC = stats::AIC(metric1, k = 2),
                             Dxy = round(metric1$stats[9],2),
                             Concordance = survConcordance(Surv(time_until_answer, answered) ~ predictions,
                                                           data = test)$concordance)
  stats <- rbind(train_metrics, test_metrics)
  rownames(stats) <- c("Training Data", "Test Data")
  return(stats)
}

#-------------------------------------------------------------------------------

#' Computes the average of performance metrics for training and test sets
#'
#' Takes output from cv function in the form of a list, and computes averages across training and test sets
#'
#' @param list list of data frames, output from cv function
#'
#' @return data frame with averages of metrics
#'
#' @export

get_avgcv <- function(list) {
  avg <- rbind(train_avg = colMeans(purrr::map_df(1:length(list), ~rbind(list[[.]][1,]))),
               test_avg = colMeans(purrr::map_df(1:length(list), ~rbind(list[[.]][2,]))))
  return(as.data.frame(avg))
}

