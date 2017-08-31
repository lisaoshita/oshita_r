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

#' Tag Frequency
#'
#' Used in exploratory_setup() and variable_setup() to create the tag-based variables.
#' This function takes in vector of tags, and separates that vector into a matrix where each column
#' represents an individual tag. Unique tags are pulled from the matrix, and the number of times
#' each unique tags occur in the entire matrix is summed to get the frequency.
#'
#' @param tags vector of tags
#'
#' @importFrom dplyr "%>%" arrange desc
#'
#' @return Returns a list. In the first position is a matrix, where each row represents
#' a question and each column represents the question's tags. A question that has no tags has empty strings in each
#' column.  In the second position is a data frame containing each unique tag found within
#' the data, along with the percent/proportion of times it occurs in the data.
#'
#' @export

tag_frequency <- function(tags) {
  split_tags <- stringr::str_split(tags, ", ", simplify = TRUE)
  split_tags[is.na(split_tags)] <- ""
  unique_tags <- unique(as.vector(split_tags[split_tags != ""]))
  tag_freq <- data.frame(tag = unique_tags,
                         percent = purrr::map_dbl(unique_tags, ~mean(rowSums(split_tags == .) > 0))) %>%
    arrange(desc(percent))
  return(list(split_tags, tag_freq))
}

#-------------------------------------------------------------------------------

#' Assign tag frequency
#'
#' Used in exploratory_setup() and variable_setup() for creating the tag-based variables.
#' This function takes each tag in the input vector, and matches it to the corresponding frequency
#' in the input data frame.
#'
#' @param tag vector of tags
#' @param tagdf data frame of tags and tag frequencies (output from tag_frequency function)
#'
#' @return Returns a numeric vector corresponding to the frequency of each tag in the input vector. Questions with
#' no tags are assigned a frequency of 0.
#'
#' @export

assign_tag_freq <- function(tag, tagdf) {
  score <- rep(0, length(tag))
  for (j in which(tag!= "")) {
    score[j] <- tagdf$percent[which(tagdf$tag == tag[j])]
  }
  return(score)
}

#-------------------------------------------------------------------------------

#' Get most frequent terms in the titles of answered/unanswered questions
#'
#' This function is used in variable setup functions for the cox regression model. It separates the full data
#' into data frames of answered and unanswered questions. It then uses the get_freq_terms function from this
#' package to get data frames of the most commonly used words in the user-specified text variable of
#' answered and unanswered questions (the fitted model used question titles). The resulting data frames are then
#' joined by word.
#'
#' @param data The full data set
#' @param variable The variable to get the frequent terms from. In the model, question titles were used.
#' Argument should be input as a string.
#' @param stopwords Optional, add stopwords to remove. Argument should be input in the form of a string or character vector.
#' For the model, "can", "will", "cant", "wont", "works", "get", "help", "need", "fix", "doesnt", "dont" were removed.
#' @param remove Optional, add words to remove from the resulting data frame. Argument should be input in the form of
#' a string or character vector. For the model, words that matched with any of the category, subcategory, or new_category
#' levels were removed.
#'
#' @importFrom dplyr "%>%" filter
#'
#' @return Returns a data frame of words from the input text variable, along with the frequency each word
#' occurs in all of the data, as well as in answered and unanswered questions, and a ratio calculated as: frequency
#' in answered divided by frequency in unanswered. The resulting data frame is used in exploratory_set and
#' variable_setup functions for the contain_answered and contain_unanswered variables.
#'
#' @examples
#' words <- c("can", "will", "cant", "wont", "works", "get", "help", "need", "fix", "doesnt", "dont")
#' devices <- c("iphone", "macbook", "imac", "ipad")
#' get_au_terms(data = x, variable = "title", stopwords = words, remove = devices)
#'
#' @export

get_au_terms <- function(data, variable, stopwords = NULL, remove = NULL) {
  answered <- data %>% dplyr::filter(answered == 1) %>% select(variable) # subsets data into answered/unanswered questions
  unanswered <- data %>% dplyr::filter(answered == 0) %>% select(variable)

  if (is.null(stopwords)) { # gets data frame of frequent terms for answered and unanswered Qs
    a_terms <- oshitar::get_freq_terms(as.character(answered[[variable]]))
    u_terms <- oshitar::get_freq_terms(as.character(unanswered[[variable]]))
  } else {
    a_terms <- oshitar::get_freq_terms(as.character(answered[[variable]]), stopwords)
    u_terms <- oshitar::get_freq_terms(as.character(unanswered[[variable]]), stopwords)
  }

  colnames(a_terms)[2:3] <- c("frequency_a", "prop_in_answered")
  colnames(u_terms)[2:3] <- c("frequency_u", "prop_in_unanswered")

  a_terms <- dplyr::mutate_if(a_terms, is.factor, as.character)
  u_terms <- dplyr::mutate_if(u_terms, is.factor, as.character)

  combined <- dplyr::full_join(a_terms, u_terms, by = "word")
  combined$ratio <- combined$prop_in_answered / combined$prop_in_unanswered

  if (!is.null(remove)) {
    terms <- glue::collapse(stringr::str_to_lower(remove), sep = " ")
    combined$delete <- purrr::map_dbl(combined$word, ~stringr::str_detect(terms, pattern = SPC %R% . %R% SPC))
    combined <- combined %>% filter(delete == 0)
  }
  return(combined)
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



