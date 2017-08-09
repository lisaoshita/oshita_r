#' Cleans corpus
#' @param corpus corpus to be cleaned
#' @param stopwrds optional, adds stopwords to remove
#' @return corpus
#' @importFrom tm tm_map stripWhitespace content_transformer removeWords stopwords
#' @export

clean_corpus <- function(corpus, stopwrds = NULL){
  if (is.null(stopwrds)) {
    corpus <- tm_map(corpus, stripWhitespace)
    corpus <- tm_map(corpus, content_transformer(tolower))
    corpus <- tm_map(corpus, removeWords, c(stopwords("en")))
    return(corpus)
  } else {
    corpus <- tm_map(corpus, stripWhitespace)
    corpus <- tm_map(corpus, content_transformer(tolower))
    corpus <- tm_map(corpus, removeWords, c(stopwords("en"), stopwrds))
    return(corpus)
  }
}

#' Returns a data frame of terms and their frequencies
#' @param vec character vector to be turned into corpus
#' @param n optional, specifies the number of frequent terms to return
#' @param stopwrds optional, adds stopwards to remove
#' @return data frame
#' @importFrom tm VectorSource VCorpus DocumentTermMatrix
#' @export

get_freq_terms <- function(vec, n = NULL, stopwrds = NULL) {
  source <- VectorSource(vec)
  corpus <- VCorpus(source)

  if (is.null(stopwrds)) {
    cleaned_corpus <- clean_corpus(corpus)
    dtm <- DocumentTermMatrix(cleaned_corpus, control = list(weighting = weightTfIdf))
    m <- as.matrix(dtm)
    freq <- colSums(m)
    freq <- sort(freq, decreasing = TRUE)
    freq_df <- data.frame(word = names(freq), frequency = unname(freq))
  } else {
    cleaned_corpus <- clean_corpus(corpus, stopwrds)
    dtm <- DocumentTermMatrix(cleaned_corpus, control = list(weighting = weightTfIdf))
    m <- as.matrix(dtm)
    freq <- colSums(m)
    freq <- sort(freq, decreasing = TRUE)
    freq_df <- data.frame(word = names(freq), frequency = unname(freq))
  }

  if (missing(n)) {
    return(freq_df)
  }
  else {
    return(freq_df[1:n,])
  }
}
