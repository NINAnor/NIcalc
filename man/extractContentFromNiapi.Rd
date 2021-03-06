% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extractContentFromNiapi.R
\encoding{UTF-8}
\name{extractContentFromNiapi}
\alias{extractContentFromNiapi}
\title{Extract Data from the Nature Index API}
\usage{
extractContentFromNiapi(
  APIUrl = "https://www8.nina.no/NaturindeksNiCalc",
  methodPath = NULL,
  token = NULL,
  ...
)
}
\arguments{
\item{APIUrl}{character, API address - including the server address}

\item{methodPath}{character, method address within the API}

\item{token}{character, token retrieved from the API using \code{\link{getToken}}}

\item{...}{further parameters to \code{\link[httr]{content}}}
}
\value{
A list containing the raw content extracted from the API.
}
\description{
\code{extractContentFromNiapi} is a general purpose utility function called by other functions that
retrieves data from specified tables in the Nature Index database over the
[The Nature Index Application Programming Interface](https://www8.nina.no/NaturindeksNiCalc/index.html)
and returns the raw content as a list.
}
\author{
Bård Pedersen
}
