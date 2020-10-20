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
data_dup <- read_csv("Peer_companies/earning_data.csv") %>% 
  select(ticker, name, date)

# Reformat the date
data_dup$date <- gsub("T.+","",data_dup$date)   # remove minutes from date


data_dup$date <- format(strptime(as.character(data_dup$date), "%Y-%m-%d"), "%d.%m.%Y") # change format

# Remove duplicats

data_dup <- unique(data_dup)

# List of tickers visible on Oslo børs webpage
library(readxl)
Ticker_list <- read_excel("Peer_companies/Ticker-list.xlsx")


# Need to remove all observations that have an earning announcement, but are not listed on the stock exchange

earnings <- data_dup[data_dup$ticker %in% Ticker_list$ticker,]

earnings <- merge(earnings, Ticker_list, by = c("ticker")) %>% 
  select(ticker, name.x, date, industry, industri)

# date
earnings$date <- earnings$date %>% as.Date(., format = "%d.%m.%Y")

# How many peer comanies do we have?

# 1. Make data set with 2 columns: date and name ++ some empty columns for peers
peer_companies <- earnings[,2:3]
peer_companies[,3:100] <- NA

# 2. Check industry dataset for dates that match
for (i in 1:length(earnings$ticker)) {
  x <- 3
  print(i)
  for (j in 1:length(earnings$ticker)) {
    if ((earnings$industry[i] == earnings$industry[j])&(earnings$date[j] %in% seq(as.Date(earnings$date[i]), by = "day", length.out = 10))) {
      peer_companies[i,x] <- earnings$name.x[j] # 3. Write name of matches in the empty columns
      x <- x + 1
    }
  }
}

# Total number of non na values in the peer columns
sum(colSums(!is.na(peer_companies[4:100]))) 



x <- c(2,5,3,9,8,11,6)
count <- 0

for (i in earnings) {
  if(val %% 2 == 0)  count = count+1
}
print(count)

test <- earnings[earnings$date == "2020-02-12",]

match("2020-02-12", earnings$date)


earnings %>% filter(., industry == "Finance" &  date  %in% seq(as.Date("2020-02-12"), by = "day", length.out = 10))



