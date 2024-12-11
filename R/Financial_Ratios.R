#'@title Quick Look at a Company's Financials
#'@description This function allows a user to view several financial ratios for a company.
#'@param tik A character string consisting of the stock ticker for the company.
#'@return A dataframe containing financial ratios for the company if it is a public stock on NASDAQ or NYSE.
#'@import tidyedgar
#'@import dplyr
#'@import readxl
#'@import DT
#'@export
#'@examples
#' Example of `filter_company_data` in use is in the stock package in action section

filter_company_data <- function(tik) {
  # Define the dataframe inside the function
  df <- tidyedgar::yearly_data(years = 2015:2023)

  # Merge df with cikticker by the cik number
  merged_df <- merge(df, cikticker, by = "data.cik", all = FALSE)

  # Add the ratios to the dataframe using mutate
  merged_df <- merged_df %>%
    mutate(
      gross_profit_margin = (GrossProfit / revenue) * 100,
      return_on_sales = (net_income / GrossProfit) * 100,
      gross_profit_to_net_income_ratio = (GrossProfit / net_income) * 100,
      Ticker = toupper(Ticker)
    )

  # Round all numeric columns to the nearest hundredth
  merged_df <- merged_df %>%
    mutate(across(where(is.numeric), ~ round(.x, 2)))

  # Check if df has been populated correctly
  if (nrow(merged_df) == 0) {
    stop("No data found for the specified years.")
  }

  # Filter data by Ticker symbol
  result <- merged_df %>% filter(Ticker == tik)

  # If no rows are returned after filtering
  if (nrow(result) == 0) {
    stop("No data found for the specified Ticker.")
  }

  # Order the data by the year column from smallest to largest
  result <- result %>% arrange(year)

  # Return the result as an interactive DT datatable
  return(
    DT::datatable(
      result,
      options = list(
        scrollX = TRUE, # Enables horizontal scrolling
        pageLength = 10 # Default number of rows displayed
      )
    )
  )
}

