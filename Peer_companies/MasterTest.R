# Neccecary packages
library(httr)
library(jsonlite)
library(tidyverse)

# Download earnings announcements from Oslo Børs
res <- POST("https://newsweb.oslobors.no/obsvc/news.obsvc?category=1002&fromDate=2020-01-01&issuer=&toDate=2020-03-01", verbose())
x <- content(res, as = "text")
input <- fromJSON(x)

# Create the first dataframe from the JSON-file
data1 <- data.frame(name = input$data$messages$issuerName, 
                   ticker = input$data$messages$issuerSign,
                   date = input$data$messages$publishedTime)

res <- POST("https://newsweb.oslobors.no/obsvc/news.obsvc?category=1002&fromDate=2020-03-02&issuer=&toDate=2020-06-01", verbose())
x <- content(res, as = "text")
input <- fromJSON(x)

data2 <- data.frame(name = input$data$messages$issuerName, 
                    ticker = input$data$messages$issuerSign,
                    date = input$data$messages$publishedTime)

res <- POST("https://newsweb.oslobors.no/obsvc/news.obsvc?category=1002&fromDate=2020-06-02&issuer=&toDate=2020-09-25", verbose())
x <- content(res, as = "text")
input <- fromJSON(x)

data3 <- data.frame(name = input$data$messages$issuerName, 
                    ticker = input$data$messages$issuerSign,
                    date = input$data$messages$publishedTime)

data <- rbind(data1, data2, data3)

## Data cleaning

# Reformat the date
data_dup <- data
data_dup$date <- gsub("T.+","",data_dup$date)   # remove minutes from date

newdate <- strptime(as.character(data_dup$date), "%Y-%m-%d") # change format
data_dup$date <- format(newdate, "%d.%m.%Y")

# Remove duplicats
data_dup <- unique(data_dup)

length(unique(data_dup$name)) 
    # why are there 441 observations when Oslo børs states that it only has 264 instruments registered.


# List of tickers visible on Oslo børs webpage
library(readxl)
Ticker_list <- read_excel("Peer_companies/Ticker-list.xlsx")

length(unique(Ticker_list$TICKER))

setdiff(data_dup$ticker, Ticker_list$TICKER) # show tickers that are only in earnings reports



