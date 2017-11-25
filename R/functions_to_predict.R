
#' Set up iFixit Answers data for model fitting or predictions
#'
#' Used in fit_model function and to set up the data to be predicted on. This function subsets the
#' data to all questions in English and creates all necessary variables.
#'
#' @param data Answers data frame.
#' @param forpredicting Set to true if this function will be used to set up the variables in the prediction
#' data set, and will not try to set up the time_until_answer variable.
#' Default is set to false. Default of this function is used in the fit_model function for setting up the
#' data set to build the model on. The time_until_answer variable will be set up in this case.
#'
#' @importFrom magrittr "%>%"
#' @importFrom stringr str_detect str_to_lower str_length str_count str_replace_all str_locate
#' @importFrom rebus "%R%" START SPC QUESTION END or1 or
#' @importFrom dplyr filter select
#'
#' @return Returns a data frame to be used in model fitting or predicting.
#'
#' @details Variables created:
#'
#' \itemize{
#'   \item new_category: reorganizes category variable (e.g. pulled out Apple products)
#'   \item weekday: day of the week the question was posted
#'   \item ampm: (morning, noon, evening, night) time of day the question was posted
#'   \item text_length, title_length, device_length
#'   \item title_questionmark: whether or not the title ends with a "?"
#'   \item title_beginwh: whether or not the title begins with "Wh"
#'   \item text_all_lower: whether or not the text is in all lower case
#'   \item text_contain_punct: whether or not the text contains any end punctuation marks
#'   \item update: whether or not the asker updated their question
#'   \item prior_effort: whether or not the asker included words in the text that indicated that
#'   they made prior effort/did research before asking the question
#'   \item gratitude: whether or not the asker expressed gratitude in the text (e.g. "Thank you", "appreciate")
#'   \item greeting: whether or not the asker included a greeting in the text
#'   \item newline_ratio: ratio of newlines to the length of the question's text
#'   \item avg_tag_length: the average length of all of a question's tags
#'   \item avg_tag_score: the score or frequency of a tag is defined as the proportion of times that tag appears
#'   in all of the data. avg_tag_score is defined as the average score/frequency of all of a question's tags
#'   \item contain_answered: whether or not the question's title contains words considered to be frequent answered terms
#'   \item contain_unanswered: whether or not the question's title contains words considered to be frequent unanswered terms
#' }
#'
#' @note If warnings about empty documents are output, they're from the function get_au_terms. This function
#' uses the function get_freq_terms, which turns the input into a document term matrix with weighting = weightTfIdf
#'
#' @examples
#' # setting up the data to build the model on
#' dir <- file.path(getwd(),"data")
#' out <- read.csv(file.path(dir, "answers_data.csv")) # data set without any variables set up
#'
#' model <- fit_model(out) # fit_model calls variable_setup() within
#'
#' # setting up variables in the prediction data
#' newdata <- oshitar::variable_setup(newdata, forpredicting = TRUE)
#'
#' @export

variable_setup <- function(data, forpredicting = FALSE){

  x <- data %>%
    tibble::as.tibble() %>%
    filter(langid == "en")

  #----Create time_until_answer---------------------------------
  if (forpredicting == FALSE) { # only used on the data to build the model on
    x$time_until_answer <- (x$first_answer_date - x$post_date) / 3600
    empty <- is.na(x$time_until_answer)
    x$time_until_answer[empty] <- (x$download_date[empty] - x$post_date[empty]) / 3600
  }

  #----Convert factors to characters----------------------------
  x <- dplyr::mutate_if(x, is.factor, as.character)
  x$new_user <- as.character(x$new_user)

  #----Recoding NAs---------------------------------------------
  # coding NAs as "Other"
  x$category[is.na(x$category)] <- "Other"
  x$subcategory[is.na(x$subcategory)] <- "Other"

  #----Creates new_category-------------------------------------
  x$new_category <- x$category

  x$apple <- str_detect(str_to_lower(x$device),
                        pattern = START %R% or("apple","ipod","ipad") %R% SPC)
  x$new_category[x$apple == TRUE | x$subcategory == "iPhone" | x$category == "Mac"] <- "Apple Product"
  x$new_category[x$new_category == "Phone"] <- "Android/Other Phone" # renaming left-over phones
  x$new_category[x$new_category == "Appliance" | x$new_category == "Household"] <- "Home"
  x$new_category[x$new_category == "Car and Truck" | x$new_category == "Vehicle"] <- "Vehicle"
  # grouping smaller categories with other
  x$new_category <- forcats::fct_lump(as.factor(x$new_category), prop = 0.02)

  #----weekday--------------------------------------------------
  datetime <- as.POSIXct(x$post_date,origin="1970-01-01")
  x$weekday <- factor(weekdays(datetime), levels = c("Monday", "Tuesday", "Wednesday",
                                                     "Thursday", "Friday", "Saturday", "Sunday"))

  #----text length----------------------------------------------
  x$text_length <- str_length(x$text)

  #----device_length--------------------------------------------
  x$device_length <- str_length(x$device)

  #----title_length---------------------------------------------
  x$title_length <- str_length(x$title)

  #----if the title ends with a "?"-----------------------------
  x$title_questionmark <- str_detect(x$title, pattern = QUESTION %R% END)

  #----if text is in all lower case-----------------------------
  cleaned <- str_replace_all(x$text, " ", "")
  cleaned <- str_replace_all(cleaned, "[[:punct:]]|[[:digit:]]", "")
  x$text_all_lower <- str_detect(cleaned, pattern = "^[[:lower:]]+$")

  #----if text contains any end punct---------------------------
  x$text_contain_punct <- str_detect(x$text, pattern = "[.|?|!]")

  #----if user updated question---------------------------------
  x$update <- str_detect(x$text, pattern = "===")

  #----if user showed any prior effort--------------------------
  x$prior_effort <- str_detect(str_to_lower(x$text),
                               pattern = or("tried", "searched", "researched", "tested",
                                            "replaced", "used", "checked", "investigated",
                                            "considered", "measured", "attempted", "inspected", "fitted"))

  #----newline ratio to length of text--------------------------
  x$newline_ratio <- str_count(x$text, pattern = "\n")/str_length(x$text)

  #----average tag length---------------------------------------
  taglist <- oshitar::tag_frequency(x$tags)
  split_tags <- taglist[[1]]
  tag_freq <- taglist[[2]]

  x$avg_tag_length <- 0
  for (j in which(x$n_tags != 0)) {
    x$avg_tag_length[j] <- sum(str_length(as.vector(split_tags[j,]))) / sum(as.vector(split_tags[j,]) != "")
  }

  #----freqency of tags-----------------------------------------
  score1 <- oshitar::assign_tag_freq(split_tags[,1], tag_freq)
  score2 <- oshitar::assign_tag_freq(split_tags[,2], tag_freq)
  score3 <- oshitar::assign_tag_freq(split_tags[,3], tag_freq)
  score4 <- oshitar::assign_tag_freq(split_tags[,4], tag_freq)

  x$avg_tag_score <- 0
  hastag <- which(x$n_tags != 0)
  x$avg_tag_score[hastag] <- (score1[hastag] + score2[hastag] + score3[hastag] + score4[hastag]) / as.numeric(x$n_tags[hastag])

  #----frequent terms in answered/unanswered questions' titles--
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

  #----Convert factors to characters----------------------------
  x <- dplyr::mutate_if(x, is.factor, as.character)

  return(list(x, answeredTerms, unansweredTerms))
}

#-------------------------------------------------------------------------------

#' Fit cox regression model
#'
#' This function sets up variables in the input data using the variable_setup function,
#' and then fits and returns the model.
#'
#' @param data The data to use in fitting the model.
#' @param summary If true, this function will print a summary of the model (e.g. statistics, coefficients).
#' Default is set to false.
#'
#' @importFrom rms cph strat pol rcs
#'
#' @return Returns the cox regression model with 19 independent predictors. See the documentation on the
#' variable_setup function for details on what each predictor represents.
#'
#' @details
#' \itemize{
#'   \item this model is stratified on ampm
#'   \item contains a quadratic term on text_length
#'   \item restricted cubic splines on device_length (5 knots), avg_tag_length (4 knots), and newline_ratio (4 knots)
#' }
#'
#' @examples
#' dir <- file.path(getwd(),"data")
#' out <- read.csv(file.path(dir, "answers_data.csv")) # data set without any variables set up
#' model <- fit_model(out)
#' model # calling model by itself will also print out a summary
#'
#' @export

fit_model <- function(data, summary = FALSE) {
  data <- oshitar::variable_setup(data, forpredicting = FALSE)
  fit <- rms::cph(survival::Surv(time_until_answer, answered) ~ new_category + new_user + contain_unanswered +
                    contain_answered + title_questionmark + title_beginwh + text_contain_punct +
                    text_all_lower + update + greeting + gratitude + prior_effort + weekday + strat(ampm) +
                    sqrt(avg_tag_score) + pol(text_length, 2) + rcs(device_length, 5) + rcs(avg_tag_length, 4) +
                    rcs(newline_ratio, 4), data = data, x = TRUE, y = TRUE, surv = TRUE)
  if (summary == TRUE) {
    print(fit)
  }
  return(fit)
}

#-------------------------------------------------------------------------------

#' Predict failure probabilities for questions on Answers
#'
#' This function uses a fitted cox regression model to predict failure probabilities with the
#' survest function from the rms package. Failure is defined as 1 - survival probability, and indicates the
#' probability that an event does happen before a certain time. In this case, the failure probability at time t
#' for a question is the probability that a question receives an answer before time t.
#'
#' @param model The cox regression model to use for predictions (output from the fit_model function). This function
#' only works with cph fits, not coxph fits.
#' @param newdata Optional, new data from which to get predictions for. If this is omitted, this function
#' will output predictions for all subjects linear predictor and strata values at the default/user-specified times.
#' @param times Vector of times at which to predict on. If omitted, this function will return predictions at
#' 0.5, 3, 10, 24, 100, 1000 hours.
#'
#' @return Returns a data frame of predicted failure probabilities. The columns are the times predicted on, the
#' rows correspond to each question in the data.
#'
#' @examples
#' # importing data
#' dir <- file.path(getwd(),"data")
#' out <- read.csv(file.path(dir, "answers_data.csv")) # data set without any variables set up
#'
#' # fitting model
#' model <- fit_model(out)
#'
#' # setting up variables in the prediction data
#' data_for_predicting <- variable_setup(newdata, forpredicting = TRUE)
#'
#' predictions <- predict_failure(model, newdata = data_for_predicting)
#'
#'
#' @export

predict_failure <- function(model, newdata = NULL, times = c(0.5, 3, 10, 24, 100, 1000)) {
  if (!is.null(newdata)) {
    pr <- data.frame(1 - rms::survest(model, as.data.frame(newdata), times = times, conf.int = FALSE)$surv)
  } else {
    pr <- data.frame(1 - rms::survest(model, times = times, conf.int = FALSE)$surv)
  }
  colnames(pr) <- times
  return(pr)
}

#-------------------------------------------------------------------------------

#' Plot failure probability predictions for a question
#'
#' This function uses a fitted cox regression model to predict and plot failure probabilities
#' for a single question. Predictions are made at each minute, starting at 0 hours,
#' and ending at the maximum time of the data the cox model was fit on. This function then
#' plots those predictions against the corresponding time with ggplot.
#'
#' @param model The cox regression model to use in predicting failure (output from the fit_model function).
#' This function only works with cph fits.
#' @param question The question to predict failure probabilities on (one row of Answers data set).
#'
#' @importFrom dplyr "%>%" filter
#' @importFrom ggplot2 aes geom_point scale_x_continuous scale_y_continuous margin
#' expand_limits theme element_text geom_segment labs
#'
#' @return Returns a ggplot of the failure experience for the input question.
#' The plot indicates when the question is predicted to reach it's maximum
#' failure probabiity with red reference lines.
#'
#' @examples
#' # importing data
#' dir <- file.path(getwd(),"data")
#' out <- read.csv(file.path(dir, "answers_data.csv")) # data set without any variables set up
#'
#' # fitting model
#' model <- fit_model(out)
#'
#' # setting up data to predict on
#' newdata <- variable_setup(newdata)
#' question <- newdata[1,]
#'
#' plot_failure(model, question)
#'
#' @export

plot_failure <- function(model, question) {

  predict <- rms::survest(model, newdata = as.data.frame(question),
                          times = seq(0, model$maxtime, by = 1), conf.int = FALSE)

  df <- data.frame(time = predict$time, pr = 1 - predict$surv) # creating a data frame of times & predictions
  df <- df %>% filter(!is.na(pr)) # removing any missing predictions

  max <- max(df$pr) # maximum probability and time at which it occurs
  hr <- which(df$pr == max(df$pr))[1]

  ggplot(df, aes(x = time, y = pr)) +
    geom_point(size = 0.2) +
    scale_x_continuous("Hours since the question was posted", expand = c(0.01, 0),
                       breaks = seq(0, nrow(df), by = 150)) +
    scale_y_continuous("Predicted Failure Probability", expand = c(0.01, 0),
                       breaks = seq(0, 1, by = 0.1)) +
    expand_limits(y = 1, x = 0) +
    theme(axis.title.x = element_text(margin = margin(t = 10))) +
    geom_segment(aes(x = 0, xend = hr, y = max, yend = max),
                 col = "red", size = 0.25) +
    geom_segment(aes(x = hr, xend = hr, y = 0, yend = max),
                 col = "red", size = 0.25) +
    labs(title = paste("Predicted failure probabilities for question", question$id),
         subtitle = paste("The red lines indicate the highest failure probability the question reaches.",
                          "\n", "Maximum failure probability: ", round(max,2), " at ", hr, " hours", sep = ""))

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
#' @examples
#' taglist <- tag_frequency(data$tags)
#' matrix_of_tags <- taglist[[1]]
#' tag_frequency_df <- taglist[[2]]
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
#' in the input data frame. Arguments are from the output of the tag_frequency function.
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
#' in answered divided by frequency in unanswered. The resulting data frame is used in exploratory_setup and
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
