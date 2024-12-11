#' Dataset that contains all of the tickers and their corresponding CIK numbers.
#'
#' @format A data frame with 12084 rows and 2 variables:
#' \describe{
#'   \item{\code{Ticker}}{chr stock ticker}
#'   \item{\code{cyl}}{num cik number}
#' }
#' @source CIK number and stock data created by merging symbols_valid_meta data csv (https://www.kaggle.com/datasets/jacksoncrow/stock-market-dataset) with edgar data from tidyedgar package
"cikticker"
