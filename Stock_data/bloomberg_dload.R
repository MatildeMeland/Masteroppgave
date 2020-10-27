# Librarys
library(lubridate)
library(readxl)
library(writexl)

# Load overview of tickers and date sequence (only trading days)
ticker_date <- read_excel("Stock_data/ticker_date.xlsx")

# Create a dataframe that repeats the date sequence 1 time for each company.
# A total of 257 companies
Security <- rep(ticker_date$input[1:255], times=1, each=length(ticker_date$date))

ticker_date2 <- as.data.frame(Security)

ticker_date2$date <- rep(ticker_date$date, times = length(ticker_date$input)-sum(is.na(ticker_date$input)))

# Creating variables to download data
ticker_date2$PX_LAST <- NA         # closing stock price
ticker_date2$PX_OPEN <- NA         # open stock price
ticker_date2$PX_VOLUME <- NA       # trading volume
ticker_date2$VOLATILITY_30D <- NA  # 30 day volatility
ticker_date2$EQY_SH_OUT <- NA      # number of shares
ticker_date2$CUR_MKT_CAP <- NA     # CUR_MKT_CAP

# Accounting variables
ticker_date2$MARKET_CAPITALIZATION_TO_BV <- NA         # Market cap to book value
ticker_date2$BOOK_VAL_PER_SH <- NA                     # Book value per share
ticker_date2$PX_TO_BOOK_RATIO <- NA                    # Price to book ratio
ticker_date2$RETURN_ON_INV_CAPITAL <- NA               # Return on invested capital...
ticker_date2$TOTAL_ANALYST_REC <- NA                   # Total number of analyst recommendations 
ticker_date2$PE_RATIO <- NA                            # Price per earning ratio
ticker_date2$EQY_INST_PCT_SH_OUT <- NA                 # Institutional ownership percentage of outstanding shares




# Write an Excel file that can be used
write_xlsx(ticker_date2, path = "bloomberg_test.xlsx", col_names = T)

