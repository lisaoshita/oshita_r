#=====================================================================

#' Set up iFixit Answers data
#'
#' Subsets iFixit Answers data to only questions in English. Sets up time_until_answer (hrs) variable. Creates other predictor variables. (Takes no argument)
#'
#' @importFrom magrittr "%>%"
#' @importFrom stringr str_detect str_to_lower str_length str_count str_split str_replace_all str_locate
#' @importFrom rebus "%R%" START SPC QUESTION END or1 or
#' @importFrom dplyr group_by summarise n arrange filter desc
#' @return data frame
#' @export
setup <- function(){
  dir <- file.path(getwd(),"data")
  out <- read.csv(system.file("extdata/answers_data.csv", package = "oshitar"))

  #====================================
  # subsetting to questions in English, converting langid from factor to character
  x <- out %>%
    tibble::as_tibble() %>%
    filter(langid == "en")
  x <- x[,-which(names(x) == "langid")]
  #====================================
  # creating time_until_answer
  x$time_until_answer <- (x$first_answer_date - x$post_date)/3600
  empty <- which(is.na(x$time_until_answer))
  for (i in empty) {
    x$time_until_answer[i] <- (x$download_date[i] - x$post_date[i])/3600
  }
  #====================================
  # recoding factor variables with more than 10 levels as character variables (title, text, tags...)
  n_levels <- x %>%
    dplyr::select_if(is.factor) %>%
    purrr::map_dbl(~length(levels(.)))
  for (i in (which(n_levels > 10))) {
    x[[names(n_levels)[i]]] <- as.character(x[[names(n_levels)[i]]])
  }
  #====================================
  # coding NAs as "Other"
  x$category[is.na(x$category)] <- "Other"
  x$subcategory[is.na(x$subcategory)] <- "Other"
  #====================================
  # creating new_category
  x$new_category <- x$category

  apple_terms <- c("apple", "ipod", "ipad") # grouping apple products
  x$apple <- str_detect(str_to_lower(x$device), pattern = START %R% or1(apple_terms) %R% SPC)
  x$new_category[x$apple == TRUE | x$subcategory == "iPhone" | x$category == "Mac"] <- "Apple Product"
  x$new_category[x$new_category == "Phone"] <- "Android/Other Phone" # renaming left-over phones
  x$new_category[x$new_category == "Appliance" | x$new_category == "Household"] <- "Home"
  x$new_category[x$new_category == "Car and Truck" | x$new_category == "Vehicle"] <- "Vehicle"
  # grouping categories with less than 100 questions with "Other"
  counts <- x %>%
    group_by(new_category) %>%
    summarise(n = n())
  for (i in which(counts$n <= 150)) {
    x$new_category[x$new_category == counts$new_category[i]] <- "Other"
  }
  #====================================
  #new_user
  x$new_user <- as.factor(x$new_user)
  #====================================
  #weekday
  x$datetime <- as.POSIXct(x$post_date,origin="1970-01-01")
  x$weekday <- factor(weekdays(x$datetime), levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
  #=============================================
  #ampm
  x$hour <- as.numeric(format(x$datetime,"%H"))
  x$ampm <- "Night"
  x$ampm[x$hour >= 5 & x$hour < 12] <- "Morning"
  x$ampm[x$hour >= 12 & x$hour < 17] <- "Afternoon" #noon - 5pm
  x$ampm[x$hour >= 17 & x$hour < 20] <- "Evening" #5pm - 8pm
  #=============================================
  # text length
  x$text_length <- str_length(x$text)
  #=============================================
  # device name length
  x$device_length <- str_length(x$device)
  #=============================================
  # title length
  x$title_length <- str_length(x$title)
  #=============================================
  # if the title ends with a question mark
  x$title_questionmark <- str_detect(x$title, pattern = QUESTION %R% END)
  #=============================================
  # if title begins with "wh"
  x$title_beginwh <- str_detect(str_to_lower(x$title), pattern = "^wh")
  #=============================================
  # if text is in all lower case
  cleaned <- str_replace_all(x$text, " ", "")
  cleaned <- str_replace_all(cleaned, "[[:punct:]]|[[:digit:]]", "")
  x$text_all_lower <- str_detect(cleaned, pattern = "^[[:lower:]]+$")
  #=============================================
  # length of text until first end punctuation mark
  length <- str_locate(x$text, pattern = "[.|?|!]")[,1]
  q <- stats::quantile(length, probs = seq(0, 1, by = 0.25), na.rm = TRUE)
  x$text_till_punct <- "none"
  x$text_till_punct[length <= q[3]] <- "short"
  x$text_till_punct[length >= q[3] & length <= q[4]] <- "medium"
  x$text_till_punct[length >= q[4]] <- "long"
  #=============================================
  # if text contains any end punctuation
  x$text_contain_punct <- str_detect(x$text, pattern = "[.|?|!]")
  #=============================================
  # if user updated the question
  x$update <- str_detect(x$text, pattern = "===")
  #=============================================
  # prior effort?
  x$prior_effort <- str_detect(str_to_lower(x$text),
                               pattern = or("tried", "searched", "researched", "tested",
                                            "replaced", "used", "checked", "investigated",
                                            "considered", "measured", "attempted", "inspected", "fitted"))
  #=============================================
  # gratitude
  x$gratitude <- str_detect(str_to_lower(x$text), pattern = or("please", "thank you",
                                                               "thanks", "thankful",
                                                               "appreciate", "appreciated",
                                                               "grateful"))
  #=============================================
  # greeting
  x$greeting <- str_detect(str_to_lower(x$text), pattern = START %R% or("hey", "hello", "greetings", "hi"))
  #=============================================
  # newline ratio to length of text
  x$newline_ratio <- str_count(x$text, pattern = "\n")/str_length(x$text)
  #=============================================
  # avg_tag_length
  split_tags <- str_split(x$tags, ", ", simplify = TRUE)
  x$avg_tag_length <- NA
  not_na <- which(x$tags != "")
  for (i in not_na) {
    total_char <- sum(str_length(as.vector(split_tags[i,])))
    total_tags <- sum(as.vector(split_tags[i,]) != "")
    x$avg_tag_length[i] <- total_char / total_tags
  }
  x$avg_tag_length[is.na(x$avg_tag_length)] <- 0
  #=============================================
  # frequency of tags
  tag_vector <- as.vector(split_tags)
  tag_vector <- tag_vector[which(tag_vector != "")]
  unique_tags <- unique(tag_vector)

  tag_freq <- data.frame(tag = unique_tags, percent = purrr::map_dbl(unique_tags, ~mean(rowSums(split_tags == .) > 0)))
  tag_freq <- tag_freq %>%
    arrange(desc(percent))

  #creating average frequency score variable
  tag1 <- split_tags[,1]
  tag2 <- split_tags[,2]
  tag3 <- split_tags[,3]
  tag4 <- split_tags[,4]

  assign_score <- function(variable) {
    score <- rep(0, nrow(x))
    notempty <- which(variable != "")
    for (i in notempty) {
      score[i] <- tag_freq$percent[which(tag_freq$tag == variable[i])]
    }
    return(score)
  }
  score1 <- assign_score(tag1)
  score2 <- assign_score(tag2)
  score3 <- assign_score(tag3)
  score4 <- assign_score(tag4)
  x$avg_tag_score <- (score1 + score2 + score3 + score4)/as.numeric(x$n_tags)
  x$avg_tag_score[is.nan(x$avg_tag_score)] <- 0

  # number of "frequent" tags a question contains
  threshold <- 0.005
  n_freq <- function(var, threshold) {
    num_freq <- rep(0, nrow(x))
    num_freq[var >= threshold] <- 1
    return(num_freq)
  }
  freq1 <- n_freq(score1, threshold)
  freq2 <- n_freq(score2, threshold)
  freq3 <- n_freq(score3, threshold)
  freq4 <- n_freq(score4, threshold)
  x$num_freq_tags <- as.factor(freq1 + freq2 + freq3 + freq4)

  #=============================================
  #frequent terms in unanswered/answered questions
  answered <- x %>%
    tibble::as_tibble() %>%
    filter(answered == 1)
  unanswered <- x %>%
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

  # removing devices
  terms <- glue::collapse(unique(str_to_lower(c(unique(x$category), unique(x$subcategory), unique(x$new_category)))), sep = " ")
  delete <- purrr::map_dbl(combined$word, ~str_detect(terms, pattern = SPC %R% . %R% SPC))
  combined <- combined[-which(delete == 1),]

  p_threshold <- 0.01
  ratio_threshold <- 1

  freq_terms_u <- combined %>%
    filter(prop_in_unanswered > p_threshold) %>%
    filter(ratio < ratio_threshold)
  freq_terms_a <- combined %>%
    filter(prop_in_answered > p_threshold) %>%
    filter(ratio > ratio_threshold)


  x$contain_unanswered <- str_detect(as.character(x$title), pattern = or1(freq_terms_u$word))
  x$contain_answered <- str_detect(as.character(x$title), pattern = or1(freq_terms_a$word))
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
  stat <- 2*(summary(full)[["loglik"]][2] - summary(reduced)[["loglik"]][2])
  df <- nrow(summary(full)[["coefficients"]]) - nrow(summary(reduced)[["coefficients"]])
  pval <- 1 - pchisq(stat, df)
  results <- data.frame(PLRT = stat, df = df, pvalue = pval)
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
#' @importFrom stringr str_detect str_length str_to_lower str_count str_split str_replace_all
#' @importFrom rebus "%R%" or1 SPC START QUESTION
#' @importFrom magrittr "%>%"
#' @importFrom dplyr arrange filter desc
#' @return data frame
#' @export
variable_setup <- function(data) {
  data$new_category <- as.character(data$category)
  apple_terms <- c("apple", "ipod", "ipad")
  data$apple <- str_detect(str_to_lower(data$device), pattern = START %R% or1(apple_terms) %R% SPC)
  data$new_category[data$apple == TRUE | data$subcategory == "iPhone" | data$category == "Mac"] <- "Apple Product"
  data$new_category[data$new_category == "Phone"] <- "Android/Other Phone"
  data$new_category[data$new_category == "Appliance" | data$new_category == "Household"] <- "Home"
  data$new_category[data$new_category == "Car and Truck" | data$new_category == "Vehicle"] <- "Vehicle"
  data$new_category[data$new_category == "Apparel" | data$new_category == "Computer Hardware" | data$new_category == "Media Player" | data$new_category == "Skills"] <- "Other"
  #=============================================
  #new_user
  data$new_user <- as.factor(data$new_user)
  #=============================================
  #weekday
  data$datetime <- as.POSIXct(data$post_date,origin="1970-01-01")
  data$weekday <- factor(weekdays(data$datetime), levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
  #=============================================
  #ampm
  data$hour <- as.numeric(format(data$datetime,"%H"))
  data$ampm <- "Night"
  data$ampm[data$hour >= 5 & data$hour < 12] <- "Morning"
  data$ampm[data$hour >= 12 & data$hour < 17] <- "Afternoon" #noon - 5pm
  data$ampm[data$hour >= 17 & data$hour < 20] <- "Evening" #5pm - 8pm
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
  # if text is in all lower case
  data$cleaned <- str_replace_all(data$text, " ", "")
  data$cleaned <- str_replace_all(data$cleaned, "[[:punct:]]|[[:digit:]]", "")
  data$text_all_lower <- str_detect(data$cleaned, pattern = "^[[:lower:]]+$")
  #=============================================
  # if user updated the question
  data$update <- str_detect(data$text, pattern = "===")
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

  # number of "frequent" tags a question contains
  threshold <- 0.005
  num_pop <- function(var, threshold) {
    num_pop <- rep(0, nrow(data))
    num_pop[data[[var]] >= threshold] <- 1
    return(num_pop)
  }
  numpop1 <- num_pop("score1", threshold)
  numpop2 <- num_pop("score2", threshold)
  numpop3 <- num_pop("score3", threshold)
  numpop4 <- num_pop("score4", threshold)
  data$num_freq_tags <- as.factor(numpop1 + numpop2 + numpop3 + numpop4)

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


  return(data)
}

#=====================================================================

# still need to work on:
# - if factor variable only has 1 question in it's level, merge it with it's neighboring level
