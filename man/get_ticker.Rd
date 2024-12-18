% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_ticker.R
\name{get_ticker}
\alias{get_ticker}
\title{Finding a Company's Ticker}
\usage{
get_ticker(company_name)
}
\arguments{
\item{company_name}{Write in company name in quotes}
}
\value{
The stock ticker for the company if it is a public stock on NASDAQ or NYSE
}
\description{
Every company that has public stock has a ticker. This function will allow a user to plug in the company name and be able to find the ticker of it.
}
\examples{
get_ticker("Par Technology")
}
