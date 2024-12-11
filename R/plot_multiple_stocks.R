#'@title Plot Multiple Stocks on One Plot
#'@description Allows the user to see multiple stocks on one plot.
#'@param tickers A vector of stock tickers to be plotted.
#'@param from_date The starting date of the plot, formatted as MM/DD/YYYY.
#'@param to_date The ending date of the plot, formatted as MM/DD/YYYY.
#'@return A plot showing stock prices and a table with percentage changes over the specified time period.
#'@import tidyquant
#'@import ggplot2
#'@import plotly
#'@import lubridate
#'@import dplyr
#'@import knitr
#'@import scales
#'@export
#'@examples
#'plot_multiple_stocks(c("PAR", "MSFT", "BA", "AAPL"), from_date = "01/05/2023", to_date = "01/01/2024")

plot_multiple_stocks <- function(tickers, from_date = NULL, to_date = NULL) {

  # Helper function: Check if a date is a valid trading day
  is_trade_day <- function(date) {
    holidays <- as.Date(c("2023-01-01", "2023-12-25"))  # Extend this list as needed
    !weekdays(date) %in% c("Saturday", "Sunday") && !(date %in% holidays)
  }

  # Helper function: Get the last valid market day before a given date
  get_last_valid_trading_day <- function(to_date) {
    while (weekdays(to_date) %in% c("Saturday", "Sunday") || !is_trade_day(to_date)) {
      to_date <- to_date - 1
    }
    return(to_date)
  }

  # Process from_date and to_date
  if (!is.null(from_date)) {
    from_date <- if (is.numeric(from_date)) {
      as.Date(as.character(from_date), format = "%m/%d/%Y")
    } else {
      mdy(from_date)
    }
  } else {
    from_date <- Sys.Date() - 365  # Default to one year ago
  }

  if (!is.null(to_date)) {
    to_date <- if (is.numeric(to_date)) {
      as.Date(as.character(to_date), format = "%m/%d/%Y")
    } else {
      mdy(to_date)
    }
  } else {
    to_date <- Sys.Date()  # Default to today
  }

  # Ensure to_date is a valid trading day
  to_date <- get_last_valid_trading_day(to_date)

  # Fetch stock data
  stock_data <- purrr::map_dfr(tickers, function(ticker) {
    suppressWarnings(
      tryCatch({
        data <- tq_get(ticker, from = from_date, to = to_date, get = "stock.prices")
        if (nrow(data) > 0) {
          mutate(data, symbol = ticker)
        } else {
          stop("No data returned.")
        }
      }, error = function(e) {
        message(paste0("Cannot gather \"", ticker, "\" data. Please try again later."))
        NULL
      })
    )
  })

  if (nrow(stock_data) == 0) {
    stop("No valid stock data available. Please check the ticker symbols or try again later.")
  }

  # Calculate percentage changes
  percentage_changes <- stock_data %>%
    group_by(symbol) %>%
    summarize(
      start_date = first(date),
      end_date = last(date),
      start_price = first(adjusted),
      end_price = last(adjusted),
      pct_change = (last(adjusted) - first(adjusted)) / first(adjusted) * 100
    ) %>%
    mutate(
      pct_change = scales::percent(pct_change / 100, accuracy = 0.01)
    ) %>%
    arrange(desc(pct_change))

  # Print percentage changes
  cat("\nPercentage Change for each stock:\n")
  print(kable(percentage_changes %>%
                select(symbol, start_date, end_date, pct_change),
              format = "pipe",
              digits = 2))

  # Plot stock data
  p <- ggplot(stock_data, aes(x = date, y = adjusted, color = symbol)) +
    geom_line() +
    labs(title = "Stock Price Comparison", y = "Adjusted Close", x = "Date") +
    theme_minimal()

  # Return an interactive plot
  ggplotly(p)
}


