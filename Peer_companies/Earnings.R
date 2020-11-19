# Neccecary packages
library(httr)
library(jsonlite)
library(tidyverse)

# #_____________________________________________________________________#
# #Uncomment with Ctrl + Shift + C


# Download earnings announcements from Oslo Børs
# Quarterly reports
res <- POST("https://newsweb.oslobors.no/obsvc/news.obsvc?category=1002&fromDate=2020-01-01&issuer=&toDate=2020-02-01", verbose())
x <- content(res, as = "text")
input <- fromJSON(x)

# Create the first dataframe from the JSON-file
data1 <- data.frame(name = input$data$messages$issuerName,
                   ticker = input$data$messages$issuerSign,
                   date = input$data$messages$publishedTime, 
                   title = input$data$messages$title, 
                   nr_attachments = input$data$messages$numbAttachments)

res <- POST("https://newsweb.oslobors.no/obsvc/news.obsvc?category=1002&fromDate=2020-02-02&issuer=&toDate=2020-03-01", verbose())
x <- content(res, as = "text")
input <- fromJSON(x)

data2 <- data.frame(name = input$data$messages$issuerName,
                    ticker = input$data$messages$issuerSign,
                    date = input$data$messages$publishedTime, 
                    title = input$data$messages$title, 
                    nr_attachments = input$data$messages$numbAttachments)

res <- POST("https://newsweb.oslobors.no/obsvc/news.obsvc?category=1002&fromDate=2020-03-02&issuer=&toDate=2020-04-01", verbose())
x <- content(res, as = "text")
input <- fromJSON(x)

data3 <- data.frame(name = input$data$messages$issuerName,
                    ticker = input$data$messages$issuerSign,
                    date = input$data$messages$publishedTime, 
                    title = input$data$messages$title, 
                    nr_attachments = input$data$messages$numbAttachments)

res <- POST("https://newsweb.oslobors.no/obsvc/news.obsvc?category=1002&fromDate=2020-04-02&issuer=&toDate=2020-05-01", verbose())
x <- content(res, as = "text")
input <- fromJSON(x)

data4 <- data.frame(name = input$data$messages$issuerName,
                    ticker = input$data$messages$issuerSign,
                    date = input$data$messages$publishedTime, 
                    title = input$data$messages$title, 
                    nr_attachments = input$data$messages$numbAttachments)

res <- POST("https://newsweb.oslobors.no/obsvc/news.obsvc?category=1002&fromDate=2020-05-02&issuer=&toDate=2020-06-01", verbose())
x <- content(res, as = "text")
input <- fromJSON(x)

data5 <- data.frame(name = input$data$messages$issuerName,
                    ticker = input$data$messages$issuerSign,
                    date = input$data$messages$publishedTime, 
                    title = input$data$messages$title, 
                    nr_attachments = input$data$messages$numbAttachments)

res <- POST("https://newsweb.oslobors.no/obsvc/news.obsvc?category=1002&fromDate=2020-06-02&issuer=&toDate=2020-07-01", verbose())
x <- content(res, as = "text")
input <- fromJSON(x)

data6 <- data.frame(name = input$data$messages$issuerName,
                    ticker = input$data$messages$issuerSign,
                    date = input$data$messages$publishedTime, 
                    title = input$data$messages$title, 
                    nr_attachments = input$data$messages$numbAttachments)

res <- POST("https://newsweb.oslobors.no/obsvc/news.obsvc?category=1002&fromDate=2020-07-02&issuer=&toDate=2020-08-01", verbose())
x <- content(res, as = "text")
input <- fromJSON(x)

data7 <- data.frame(name = input$data$messages$issuerName,
                    ticker = input$data$messages$issuerSign,
                    date = input$data$messages$publishedTime, 
                    title = input$data$messages$title, 
                    nr_attachments = input$data$messages$numbAttachments)

res <- POST("https://newsweb.oslobors.no/obsvc/news.obsvc?category=1002&fromDate=2020-08-02&issuer=&toDate=2020-09-01", verbose())
x <- content(res, as = "text")
input <- fromJSON(x)

data8 <- data.frame(name = input$data$messages$issuerName,
                    ticker = input$data$messages$issuerSign,
                    date = input$data$messages$publishedTime, 
                    title = input$data$messages$title, 
                    nr_attachments = input$data$messages$numbAttachments)

res <- POST("https://newsweb.oslobors.no/obsvc/news.obsvc?category=1002&fromDate=2020-09-02&issuer=&toDate=2020-09-30", verbose())
x <- content(res, as = "text")
input <- fromJSON(x)

data9 <- data.frame(name = input$data$messages$issuerName,
                    ticker = input$data$messages$issuerSign,
                    date = input$data$messages$publishedTime, 
                    title = input$data$messages$title, 
                    nr_attachments = input$data$messages$numbAttachments)

data_q <- rbind(data1, data2, data3, data4, data5, data6, data7, data8, data9) %>% 
 mutate(type = "quarterly")
  
rm(data1, data2, data3, data4, data5, data6, data7, data8, data9)

# Anual reports
res <- POST("https://newsweb.oslobors.no/obsvc/news.obsvc?category=1001&fromDate=2020-01-01&issuer=&toDate=2020-03-01", verbose())
x <- content(res, as = "text")
input <- fromJSON(x)

data1 <- data.frame(name = input$data$messages$issuerName,
                    ticker = input$data$messages$issuerSign,
                    date = input$data$messages$publishedTime, 
                    title = input$data$messages$title, 
                    nr_attachments = input$data$messages$numbAttachments)

res <- POST("https://newsweb.oslobors.no/obsvc/news.obsvc?category=1001&fromDate=2020-03-02&issuer=&toDate=2020-05-01", verbose())
x <- content(res, as = "text")
input <- fromJSON(x)

data2 <- data.frame(name = input$data$messages$issuerName,
                    ticker = input$data$messages$issuerSign,
                    date = input$data$messages$publishedTime, 
                    title = input$data$messages$title, 
                    nr_attachments = input$data$messages$numbAttachments)

res <- POST("https://newsweb.oslobors.no/obsvc/news.obsvc?category=1001&fromDate=2020-05-02&issuer=&toDate=2020-09-30", verbose())
x <- content(res, as = "text")
input <- fromJSON(x)

data3 <- data.frame(name = input$data$messages$issuerName,
                    ticker = input$data$messages$issuerSign,
                    date = input$data$messages$publishedTime, 
                    title = input$data$messages$title, 
                    nr_attachments = input$data$messages$numbAttachments)

# Combine anual data
data_a <- rbind(data1, data2, data3) %>% 
  mutate(type = "annual")

rm(data1, data2, data3)

# Annen info (noen har kvartalsrapporter her)
res <- POST("https://newsweb.oslobors.no/obsvc/news.obsvc?category=1010&fromDate=2020-01-01&issuer=&toDate=2020-02-01", verbose())
x <- content(res, as = "text")
input <- fromJSON(x)

# Create the first dataframe from the JSON-file
data1 <- data.frame(name = input$data$messages$issuerName,
                    ticker = input$data$messages$issuerSign,
                    date = input$data$messages$publishedTime, 
                    title = input$data$messages$title, 
                    nr_attachments = input$data$messages$numbAttachments)

res <- POST("https://newsweb.oslobors.no/obsvc/news.obsvc?category=1010&fromDate=2020-02-02&issuer=&toDate=2020-02-15", verbose())
x <- content(res, as = "text")
input <- fromJSON(x)

data21 <- data.frame(name = input$data$messages$issuerName,
                    ticker = input$data$messages$issuerSign,
                    date = input$data$messages$publishedTime, 
                    title = input$data$messages$title, 
                    nr_attachments = input$data$messages$numbAttachments)

res <- POST("https://newsweb.oslobors.no/obsvc/news.obsvc?category=1010&fromDate=2020-02-16&issuer=&toDate=2020-03-01", verbose())
x <- content(res, as = "text")
input <- fromJSON(x)

data22 <- data.frame(name = input$data$messages$issuerName,
                    ticker = input$data$messages$issuerSign,
                    date = input$data$messages$publishedTime, 
                    title = input$data$messages$title, 
                    nr_attachments = input$data$messages$numbAttachments)

res <- POST("https://newsweb.oslobors.no/obsvc/news.obsvc?category=1010&fromDate=2020-03-02&issuer=&toDate=2020-03-15", verbose())
x <- content(res, as = "text")
input <- fromJSON(x)

data31 <- data.frame(name = input$data$messages$issuerName,
                    ticker = input$data$messages$issuerSign,
                    date = input$data$messages$publishedTime, 
                    title = input$data$messages$title, 
                    nr_attachments = input$data$messages$numbAttachments)

res <- POST("https://newsweb.oslobors.no/obsvc/news.obsvc?category=1010&fromDate=2020-03-16&issuer=&toDate=2020-04-01", verbose())
x <- content(res, as = "text")
input <- fromJSON(x)

data32 <- data.frame(name = input$data$messages$issuerName,
                    ticker = input$data$messages$issuerSign,
                    date = input$data$messages$publishedTime, 
                    title = input$data$messages$title, 
                    nr_attachments = input$data$messages$numbAttachments)

res <- POST("https://newsweb.oslobors.no/obsvc/news.obsvc?category=1010&fromDate=2020-04-02&issuer=&toDate=2020-04-15", verbose())
x <- content(res, as = "text")
input <- fromJSON(x)

data41 <- data.frame(name = input$data$messages$issuerName,
                    ticker = input$data$messages$issuerSign,
                    date = input$data$messages$publishedTime, 
                    title = input$data$messages$title, 
                    nr_attachments = input$data$messages$numbAttachments)

res <- POST("https://newsweb.oslobors.no/obsvc/news.obsvc?category=1010&fromDate=2020-04-16&issuer=&toDate=2020-05-01", verbose())
x <- content(res, as = "text")
input <- fromJSON(x)

data42 <- data.frame(name = input$data$messages$issuerName,
                    ticker = input$data$messages$issuerSign,
                    date = input$data$messages$publishedTime, 
                    title = input$data$messages$title, 
                    nr_attachments = input$data$messages$numbAttachments)

res <- POST("https://newsweb.oslobors.no/obsvc/news.obsvc?category=1010&fromDate=2020-05-02&issuer=&toDate=2020-05-15", verbose())
x <- content(res, as = "text")
input <- fromJSON(x)

data51 <- data.frame(name = input$data$messages$issuerName,
                    ticker = input$data$messages$issuerSign,
                    date = input$data$messages$publishedTime, 
                    title = input$data$messages$title, 
                    nr_attachments = input$data$messages$numbAttachments)

res <- POST("https://newsweb.oslobors.no/obsvc/news.obsvc?category=1010&fromDate=2020-05-16&issuer=&toDate=2020-06-01", verbose())
x <- content(res, as = "text")
input <- fromJSON(x)

data52 <- data.frame(name = input$data$messages$issuerName,
                    ticker = input$data$messages$issuerSign,
                    date = input$data$messages$publishedTime, 
                    title = input$data$messages$title, 
                    nr_attachments = input$data$messages$numbAttachments)

res <- POST("https://newsweb.oslobors.no/obsvc/news.obsvc?category=1010&fromDate=2020-06-02&issuer=&toDate=2020-06-15", verbose())
x <- content(res, as = "text")
input <- fromJSON(x)

data61 <- data.frame(name = input$data$messages$issuerName,
                    ticker = input$data$messages$issuerSign,
                    date = input$data$messages$publishedTime, 
                    title = input$data$messages$title, 
                    nr_attachments = input$data$messages$numbAttachments)

res <- POST("https://newsweb.oslobors.no/obsvc/news.obsvc?category=1010&fromDate=2020-06-16&issuer=&toDate=2020-07-01", verbose())
x <- content(res, as = "text")
input <- fromJSON(x)

data62 <- data.frame(name = input$data$messages$issuerName,
                    ticker = input$data$messages$issuerSign,
                    date = input$data$messages$publishedTime, 
                    title = input$data$messages$title, 
                    nr_attachments = input$data$messages$numbAttachments)

res <- POST("https://newsweb.oslobors.no/obsvc/news.obsvc?category=1010&fromDate=2020-07-02&issuer=&toDate=2020-08-01", verbose())
x <- content(res, as = "text")
input <- fromJSON(x)

data7 <- data.frame(name = input$data$messages$issuerName,
                    ticker = input$data$messages$issuerSign,
                    date = input$data$messages$publishedTime, 
                    title = input$data$messages$title, 
                    nr_attachments = input$data$messages$numbAttachments)

res <- POST("https://newsweb.oslobors.no/obsvc/news.obsvc?category=1010&fromDate=2020-08-02&issuer=&toDate=2020-09-01", verbose())
x <- content(res, as = "text")
input <- fromJSON(x)

data8 <- data.frame(name = input$data$messages$issuerName,
                    ticker = input$data$messages$issuerSign,
                    date = input$data$messages$publishedTime, 
                    title = input$data$messages$title, 
                    nr_attachments = input$data$messages$numbAttachments)

res <- POST("https://newsweb.oslobors.no/obsvc/news.obsvc?category=1010&fromDate=2020-09-02&issuer=&toDate=2020-09-30", verbose())
x <- content(res, as = "text")
input <- fromJSON(x)

data9 <- data.frame(name = input$data$messages$issuerName,
                    ticker = input$data$messages$issuerSign,
                    date = input$data$messages$publishedTime, 
                    title = input$data$messages$title, 
                    nr_attachments = input$data$messages$numbAttachments)


data_o <- rbind(data1, data21, data22, data31, data32, data41, data42, data51, data52, data61, data62, data7, data8, data9) %>% 
  mutate(type = "other")

rm(data1, data21, data22, data31, data32, data41, data42, data51, data52, data61, data62, data7, data8, data9)



# Combine all data
data <- rbind(data_q, data_a, data_o)


# Tried to make a loop -- look at this later
temp <- data.frame(category = c("1002", "1001", "1010"),
                   type = c("quaterly", "anual", "other"))
day <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31")
month <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10")
for (i in 1:length(temp$category)) {
  for (j in month) {
    for (k in day) {
      link <- paste0("https://newsweb.oslobors.no/obsvc/news.obsvc?category=", temp$category[i], "fromDate=2020-", j, "-", k, "&issuer=&toDate=2020-", j + 1, k + 1)
      res <- POST(link, verbose())
      x <- content(res, as = "text")
      input <- fromJSON(x)
      
      data_temp <- data.frame(name = input$data$messages$issuerName,
                              ticker = input$data$messages$issuerSign,
                              date = input$data$messages$publishedTime,
                              title = input$data$messages$title,
                              nr_attachments = input$data$messages$numbAttachments,
                              type = temp$type[i])
      
      test <- rbind(data, data_temp)
    }
  }
}


# create back-up file
write.csv(data, file = "Peer_companies/earning_dataT.csv")

# _______________________________________________________________________#

## Data cleaning
data_dup <- read_csv("Peer_companies/earning_dataT.csv", locale = readr::locale(encoding = "latin1"))[,-1]

# Reformat the date
data_dup$date <- gsub("T.+","",data_dup$date)   # remove minutes from date
data_dup$date <- format(strptime(as.character(data_dup$date), "%Y-%m-%d"), "%d.%m.%Y") # change format

data_dup$ticker <- gsub("-", "", data_dup$ticker)


# Remove duplicats
# data_dup <- unique(data_dup)

data_dup$ticker[data_dup$ticker == "PPG PREF"] <- "PPGPREF"

# List of tickers visible on Oslo børs webpage
library(readxl)
Ticker_list <- read_excel("Peer_companies/Ticker-list.xlsx")


# Need to remove all observations that have an earning announcement, but are not listed on the stock exchange

earnings <- data_dup[data_dup$ticker %in% Ticker_list$ticker,]

earnings <- merge(earnings, Ticker_list, by = c("ticker")) %>% 
  select(ticker, name.x, date, industry, industri, title, nr_attachments, type)

rm(data_dup, Ticker_list) # Don't need this anymore

# date
earnings$date <- earnings$date %>% as.Date(., format = "%d.%m.%Y")

# Remove all without attachments
earnings <- earnings[earnings$nr_attachments > 0,]

temp <- earnings[earnings$type == "other",]

# Words we want
temp1 <- temp[grepl("kvartal|quarter|første|andre|tredje|fjerde|first|second|third|fourth|annual|interim|tertial|halvårs|halvårs|halvtårs|inviterer|half|helår|delårs", temp$title, ignore.case = T), ]
temp2 <- temp[grepl("Q|H1|1H", temp$title, ignore.case = F), ]
temp12 <- rbind(temp1, temp2)

# Words we don't want - maybe move this to the whole dataset?
temp3 <- temp12[!grepl("invitasjon|invitation|announcment|notice|innkalling|meeting|update|general|godkjenning|presents|presenterer|presentation|korreksjon|correction|preliminary", temp12$title, ignore.case = T), ]

# Fjerne BGBIO
temp3 <- temp3[!temp3$ticker == "BGBIO",]

data <- rbind(temp3, earnings[earnings$type == "quarterly",], earnings[earnings$type == "annual",])
data <- data[!grepl("preliminary|invitation", data$title, ignore.case = T), ]

rm(temp, temp1, temp12, temp2, temp3)


# Save to csv
write.csv(data, file = "Stock_data/earning_data2.csv")




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

earnings %>% filter(., industry == "Finance" &  date  %in% seq(as.Date("2020-02-12"), by = "day", length.out = 10))

earnings %>% filter(., industry == "Finance")

for (i in 1:12){
  earnings %>% filter(., industry == unique(earnings$industry)[i]) %>% 
    
    
    for (j in 1:117){
      filter(.,date  %in% seq(unique(earnings$date)[j], by = "day", length.out = 10))
    }
}


