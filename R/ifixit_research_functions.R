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

  df <- nrow(full[["coefficients"]]) - nrow(reduced[["coefficients"]])
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
#' @return survival curves plotted with ggplot
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

#' Variable setup for test data sets
#'
#' Function to set up each of the variables included in the cox regression model.
#' Adds variables: new_category, weekday, ampm, text_length, device_length, title_questionmark, title_beginwh, capital_text, update, newline_ratio, frequent_tags, contain_answered, contain_unanswered
#' @param data test data set to be used
#' @importFrom stringr str_detect str_length str_to_lower str_count str_split
#' @importFrom rebus "%R%" or1 SPC START QUESTION
#' @importFrom magrittr "%>%"
#' @importFrom dplyr arrange filter desc
#' @return data frame
#' @export
variable_setup <- function(data) {
  data$category <- as.character(data$category)
  data$category[is.na(data$category)] <- "Other"

  data$new_category <- data$category
  apple_terms <- c("apple", "ipod", "ipad")
  data$apple <- str_detect(str_to_lower(data$device), pattern = START %R% or1(apple_terms) %R% SPC)
  data$new_category[data$apple == TRUE | data$subcategory == "iPhone" | data$category == "Mac"] <- "Apple Product"
  data$new_category[data$new_category == "Phone"] <- "Android/Other Phone"
  data$new_category[data$new_category == "Appliance" | data$new_category == "Household"] <- "Home"
  data$new_category[data$new_category == "Car and Truck" | data$new_category == "Vehicle"] <- "Vehicle"
  data$new_category[data$new_category == "Computer Hardware" | data$new_category == "Media Player" |
                      data$new_category == "Apparel"] <- "Other"
  #=============================================
  #weekday
  data$datetime <- as.POSIXct(data$post_date, origin="1970-01-01")
  data$weekday <- factor(weekdays(data$datetime), levels = c("Monday", "Tuesday", "Wednesday",
                                                             "Thursday", "Friday", "Saturday", "Sunday"))
  #=============================================
  #ampm
  data$hour <- as.numeric(format(data$datetime,"%H"))
  data$ampm <- "Night"
  data$ampm[data$hour >= 5 & data$hour < 12] <- "Morning"
  data$ampm[data$hour >= 12 & data$hour < 17] <- "Afternoon" #noon - 5pm
  data$ampm[data$hour >= 17 & data$hour < 20] <- "Evening" #5pm - 8pm
  #=============================================
  # text length
  data$text_length <- str_length(data$text)
  #=============================================
  # device name length
  data$device_length <- str_length(data$device)
  #=============================================
  # if the title ends with a question mark
  library(rebus)
  data$title_questionmark <- str_detect(data$title, pattern = QUESTION %R% END)
  #=============================================
  # if title begins with "wh"
  data$title_beginwh <- str_detect(str_to_lower(data$title), pattern = "^wh")
  #=============================================
  data$capital_text <- str_detect(as.character(data$text), pattern = "^[[:upper:]]")
  #=============================================
  data$update <- str_detect(data$text, pattern = "===")
  #=============================================
  data$newline_ratio <- str_count(data$text, pattern = "\n")/str_length(data$text)
  #=============================================
  # frequent tags
  split_tags <- str_split(data$tags, ", ", simplify = TRUE)
  tag_vector <- as.vector(split_tags)
  tag_vector <- tag_vector[which(tag_vector != "")]
  unique_tags <- unique(tag_vector)

  tag_freq <- data.frame(tag = unique_tags, percent = purrr::map_dbl(unique_tags, ~mean(rowSums(split_tags == .) > 0)))
  tag_freq <- tag_freq %>%
    arrange(desc(percent))

  #creating average frequency score variable
  data$tag1 <- split_tags[,1]
  data$tag2 <- split_tags[,2]
  data$tag3 <- split_tags[,3]
  data$tag4 <- split_tags[,4]

  assign_score <- function(data, variable) {
    score <- rep(0, nrow(data))
    notempty <- which(data[[variable]] != "")
    for (i in notempty) {
      score[i] <- tag_freq$percent[which(tag_freq$tag == data[[variable]][i])]
    }
    return(score)
  }
  data$score1 <- assign_score(data, "tag1")
  data$score2 <- assign_score(data, "tag2")
  data$score3 <- assign_score(data, "tag3")
  data$score4 <- assign_score(data, "tag4")
  data$avg_tag_score <- (data$score1 + data$score2 + data$score3 +
                           data$score4)/as.numeric(data$n_tags)
  data$avg_tag_score[is.nan(data$avg_tag_score)] <- 0
  #=============================================
  # if question contains "frequent" tag
  percentile80 <- stats::quantile(data$avg_tag_score, probs = 0.80)

  data$frequent_tag <- FALSE
  data$frequent_tag[data$avg_tag_score >= percentile80] <- TRUE
  #=============================================
  #frequent terms in unanswered/answered questions
  answered <- data %>%
    tibble::as_tibble() %>%
    filter(answered == 1)
  unanswered <- data %>%
    tibble::as_tibble() %>%
    filter(answered == 0)

  terms_a <- oshitar::get_freq_terms(answered$title, stopwords = c("can", "will", "cant", "wont", "works", "get", "help", "need", "fix"))
  terms_a$prop_in_answered <- terms_a$frequency/nrow(terms_a)
  colnames(terms_a)[2] <- "frequency_a"

  terms_u <- oshitar::get_freq_terms(unanswered$title, stopwords = c("can", "will", "cant", "wont", "works", "get", "help", "need", "fix"))
  terms_u$prop_in_unanswered <- terms_u$frequency/nrow(terms_u)
  colnames(terms_u)[2] <- "frequency_u"

  combined <- dplyr::full_join(terms_a, terms_u, by = "word")
  combined$ratio <- combined$prop_in_answered / combined$prop_in_unanswered

  p_threshold <- 0.01
  ratio_threshold <- 1

  freq_terms_u <- combined %>%
    filter(prop_in_unanswered > p_threshold) %>%
    filter(ratio < ratio_threshold)
  freq_terms_a <- combined %>%
    filter(prop_in_answered > p_threshold) %>%
    filter(ratio > ratio_threshold)

  data$contain_unanswered <- str_detect(as.character(data$title), pattern = or1(freq_terms_u$word))
  data$contain_answered <- str_detect(as.character(data$title), pattern = or1(freq_terms_a$word))

  data$new_user <- as.factor(data$new_user)

  return(data)
}

#=====================================================================
