#=====================================================================

#' Clean corpus
#'
#' Removes whitespace, transforms characters to lowercase, and removes stopwords (with the option to add additional stopwords to remove). This function is used in get_freq_terms.
#'
#' @param corpus corpus to be cleaned. To create the corpus use: source <- VectorSource(vec), corpus <- VCorpus(source)
#' @param stopwords optional, adds stopwords to remove. If not specified it will only remove English stopwords from the tm package.
#' @return corpus
#' @importFrom tm tm_map stripWhitespace content_transformer removeWords stopwords
#' @export
clean_corpus <- function(corpus, stopwords = NULL){
  if (is.null(stopwords)) {
    corpus <- tm_map(corpus, stripWhitespace)
    corpus <- tm_map(corpus, content_transformer(tolower))
    corpus <- tm_map(corpus, removeWords, c(stopwords("en")))
    return(corpus)
  } else {
    corpus <- tm_map(corpus, stripWhitespace)
    corpus <- tm_map(corpus, content_transformer(tolower))
    corpus <- tm_map(corpus, removeWords, c(stopwords("en"), stopwords))
    return(corpus)
  }
}

#=====================================================================

#' Find the most frequent terms and their frequencies from a character vector
#'
#' Turns the input character vector into a DocumentTermMatrix. Sums the frequencies of each words. Returns a data frame with terms and their frequencies arranged from most to least frequent.
#' @param vec character vector to be turned into corpus
#' @param n optional, specifies the number of frequent terms to return. If not specified it will return the entire data frame.
#' @param stopwords optional, adds stopwards to remove. If not specified it will only remove English stopwords from the tm package.
#' @return data frame
#' @importFrom tm VectorSource VCorpus DocumentTermMatrix weightTfIdf
#' @export
get_freq_terms <- function(vec, n = NULL, stopwords = NULL) {
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

  if (is.null(n)) {
    return(freq_df)
  }
  else {
    return(freq_df[1:n,])
  }
}

#=====================================================================
