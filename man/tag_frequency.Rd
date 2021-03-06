% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions_to_predict.R
\name{tag_frequency}
\alias{tag_frequency}
\title{Tag Frequency}
\usage{
tag_frequency(tags)
}
\arguments{
\item{tags}{vector of tags}
}
\value{
Returns a list. In the first position is a matrix, where each row represents
a question and each column represents the question's tags. A question that has no tags has empty strings in each
column.  In the second position is a data frame containing each unique tag found within
the data, along with the percent/proportion of times it occurs in the data.
}
\description{
Used in exploratory_setup() and variable_setup() to create the tag-based variables.
This function takes in vector of tags, and separates that vector into a matrix where each column
represents an individual tag. Unique tags are pulled from the matrix, and the number of times
each unique tags occur in the entire matrix is summed to get the frequency.
}
\examples{
taglist <- tag_frequency(data$tags)
matrix_of_tags <- taglist[[1]]
tag_frequency_df <- taglist[[2]]

}
