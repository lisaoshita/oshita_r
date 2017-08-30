#=====================================================================

#' Clean corpus
#'
#' Removes whitespace, removes punctuation, transforms characters to lowercase, and removes stopwords (with the option to add additional stopwords). Used in get_freq_terms.
#'
#' @param corpus corpus to be cleaned. To create the corpus use: source <- VectorSource(vec), corpus <- VCorpus(source)
#' @param stopwords optional, adds stopwords to remove. If not specified it will only remove English stopwords from the tm package.
#' @return corpus
#' @importFrom tm tm_map stripWhitespace removePunctuation content_transformer removeWords stopwords
#' @export
clean_corpus <- function(corpus, stopwords = NULL){
  if (is.null(stopwords)) {
    corpus <- tm_map(corpus, stripWhitespace)
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, content_transformer(tolower))
    corpus <- tm_map(corpus, removeWords, c(stopwords("en")))
    return(corpus)
  } else {
    corpus <- tm_map(corpus, stripWhitespace)
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, content_transformer(tolower))
    corpus <- tm_map(corpus, removeWords, c(stopwords("en"), stopwords))
    return(corpus)
  }
}

#=====================================================================

#' Get the frequency of each term in a character vector
#'
#' This function converts the input character vector into a corpus and applies the clean_corpus function from this package.
#' The resulting corpus is converted into a document term matrix, from which the number of times each term occurs
#' is summed to get the frequency.
#' @param vec character vector to get frequencies from
#' @param stopwords optional, adds stopwords to remove from the corpus. If not specified it will only remove
#' English stopwords from the tm package. Argument should be input in the form of a string or character vector.
#' @return Outputs a data frame. Each row represents a word in the character vector with its respective
#' frequency and proportion of times it occured in the vector. Ordered from most to least frequent.
#' @importFrom tm VectorSource VCorpus DocumentTermMatrix weightTfIdf
#' @examples
#' get_freq_terms(words, stopwords = c("remove", "these"))
#' @export
get_freq_terms <- function(vec, stopwords = NULL) {
  source <- VectorSource(vec)
  corpus <- VCorpus(source)

  if (is.null(stopwords)) {
    cleaned_corpus <- clean_corpus(corpus)
  } else {
    cleaned_corpus <- clean_corpus(corpus, stopwords)
  }

  dtm <- DocumentTermMatrix(cleaned_corpus, control = list(weighting = weightTfIdf))
  freq <- colSums(as.matrix(dtm))
  freq <- sort(freq, decreasing = TRUE)
  freq_df <- data.frame(word = names(freq), frequency = unname(freq))
  freq_df$proportion_occurs <- freq_df$frequency/nrow(freq_df)
  return(freq_df)
}

#=====================================================================
