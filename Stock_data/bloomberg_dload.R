# Librarys
library(lubridate)
library(readxl)
library(writexl)

# Load overview of tickers and date sequence (only trading days)
ticker_date <- read_excel("Stock_data/ticker_date.xlsx")

# Create a dataframe that repeats the date sequence 1 time for each company.
# A total of 257 companies
Security <- rep(ticker_date$input[1:257], times=1, each=length(ticker_date$date))

ticker_date2 <- as.data.frame(Security)

ticker_date2$date <- rep(ticker_date$date, times = length(ticker_date$input)-sum(is.na(ticker_date$input)))

# Creating variables to download data
ticker_date2$PX_LAST <- NA         # closing stock price
ticker_date2$PX_OPEN <- NA         # open stock price
ticker_date2$PX_VOLUME <- NA       # trading volume
ticker_date2$VOLATILITY_30D <- NA  # trading volume


# Write an Excel file that can be used
write_xlsx(ticker_date2, path = "bloomberg_test.xlsx", col_names = T)

