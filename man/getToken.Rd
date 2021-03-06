% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getToken.R
\encoding{UTF-8}
\name{getToken}
\alias{getToken}
\title{Get a Connection Token from the NI Database}
\usage{
getToken(
  username = NULL,
  password = NULL,
  url = "https://www8.nina.no/NaturindeksNiCalc"
)
}
\arguments{
\item{username}{character        Your username}

\item{password}{character        Your password}

\item{url}{character        API address.}
}
\value{
The function sets the variables niToken and url, which both are used in other functions that
communicate with the database through the API.
}
\description{
This function tries to connect to the NI database API and retrieves a token that is later used to
communicate with the database.
}
\examples{
\dontrun{
getToken("test.user", "test.password")
}

}
\author{
Jens Åström
}
