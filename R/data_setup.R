#' Sets up iFixit Answers data
#' @return Answers data frame
#' @importFrom magrittr "%>%"
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

