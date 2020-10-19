# Neccecary packages
library(httr)
library(jsonlite)
library(tidyverse)

# #_____________________________________________________________________#
# #Uncomment with Ctrl + Shift + C

# #Download earnings announcements from Oslo Børs
# res <- POST("https://newsweb.oslobors.no/obsvc/news.obsvc?category=1002&fromDate=2020-01-01&issuer=&toDate=2020-03-01", verbose())
# x <- content(res, as = "text")
# input <- fromJSON(x)
# 
# # Create the first dataframe from the JSON-file
# data1 <- data.frame(name = input$data$messages$issuerName,
#                    ticker = input$data$messages$issuerSign,
#                    date = input$data$messages$publishedTime)
# 
# res <- POST("https://newsweb.oslobors.no/obsvc/news.obsvc?category=1002&fromDate=2020-03-02&issuer=&toDate=2020-06-01", verbose())
# x <- content(res, as = "text")
# input <- fromJSON(x)
# 
# data2 <- data.frame(name = input$data$messages$issuerName,
#                     ticker = input$data$messages$issuerSign,
#                     date = input$data$messages$publishedTime)
# 
# res <- POST("https://newsweb.oslobors.no/obsvc/news.obsvc?category=1002&fromDate=2020-06-02&issuer=&toDate=2020-09-25", verbose())
# x <- content(res, as = "text")
# input <- fromJSON(x)
# 
# data3 <- data.frame(name = input$data$messages$issuerName,
#                     ticker = input$data$messages$issuerSign,
#                     date = input$data$messages$publishedTime)
# 
# data <- rbind(data1, data2, data3)
# 
# 
# 
# # create back-up file
# # write.csv(data, file = "earning_data.csv")

# _______________________________________________________________________#
## Data cleaning
data <- read_csv("Peer_companies/earning_data.csv") %>% 
  select(ticker, name, date)

# Reformat the date
data_dup <- data
data_dup$date <- gsub("T.+","",data_dup$date)   # remove minutes from date

newdate <- strptime(as.character(data_dup$date), "%Y-%m-%d") # change format
data_dup$date <- format(newdate, "%d.%m.%Y")

# Remove duplicats

data_dup <- unique(data_dup)

# List of tickers visible on Oslo børs webpage
library(readxl)
Ticker_list <- read_excel("Peer_companies/Ticker-list.xlsx")


# Need to remove all observations that have an earning announcement, but are not listed on the stock exchange

test <- data_dup[data_dup$ticker %in% Ticker_list$ticker,]

test2 <- merge(test, Ticker_list, by = c("ticker")) %>% 
  select(ticker, name.x, date, industry, industri)

# date
test2$date <- test2$date %>% as.Date(., format = "%d.%m.%Y")
