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
write.csv(data, file = "Peer_companies/earning_data2.csv")

# _______________________________________________________________________#

## Data cleaning
data_dup <- read_csv("Peer_companies/earning_data2.csv", locale = readr::locale(encoding = "latin1"))[,-1]

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



earning_data <- data %>% 
  select(-c(industri, industry, nr_attachments))

# Change column names
colnames(earning_data) <- c("Security", "Name", "date", "title", "type")

earning_data$Q <- ifelse(grepl("kvartal|quarter|første|andre|tredje|fjerde|first|second|third|fourth|interim|tertial|halvårs|halvår|halvtårs|half|delårs", earning_data$title, ignore.case = T) | grepl("Q1|Q2|Q3|Q4|H1|1H|H2|2H", earning_data$title, ignore.case = F), 1, 0)
earning_data$A <- ifelse(grepl("annual|helårs|helår|årsrapport|årsregnskap|årsmelding|year end|full year|year-end|års-", earning_data$title, ignore.case = T) & !grepl("kvartal|quarter|interim|tertial|halvårs|halvårs|halvtårs|half|delårs|semi-|Q1|Q2|Q3|Q4|H1|1H", earning_data$title, ignore.case = T), 1, 0)

Y2019_Q3 <- interval(ymd("2019-10-01"), ymd("2019-12-30"))
Y2019_Q4 <- interval(ymd("2020-01-01"), ymd("2020-03-31"))
Y2020_Q1 <- interval(ymd("2020-04-01"), ymd("2020-06-30"))
Y2020_Q2 <- interval(ymd("2020-07-01"), ymd("2020-09-30"))

earning_data$quarter[earning_data$date %within% Y2019_Q3] <- "Q3"
earning_data$quarter[earning_data$date %within% Y2019_Q4] <- "Q4"
earning_data$quarter[earning_data$date %within% Y2020_Q1] <- "Q1"
earning_data$quarter[earning_data$date %within% Y2020_Q2] <- "Q2"

rm(Y2019_Q3, Y2019_Q4, Y2020_Q1, Y2020_Q2)

earning_data$quarter[earning_data$A == 1] <- "A"

# Manually reclassify
earning_data$quarter[earning_data$Security == "GEOSME" & earning_data$type == "annual"] <- "A"

earning_data$quarter[earning_data$Security == "WILS" & earning_data$date == "2020-02-19"] <- "Q4"
earning_data$quarter[earning_data$Security == "SRBANK" & earning_data$date == "2020-02-05"] <- "Q4"
earning_data$quarter[earning_data$Security == "MORG" & earning_data$date == "2020-01-30"] <- "Q4"

# Identify the annual reports that are released after a Q4 report
for (i in unique(earning_data$Security)) {
  sub <- earning_data[earning_data$Security == i,]
  Q4 <- sub[sub$quarter == "Q4",]
  A <- sub[sub$quarter == "A",]
  if (nrow(Q4) > 0 & nrow(A) > 0) {
    if (Q4$date < A$date) {
      earning_data$remove[earning_data$Security == i & earning_data$quarter == "A"] <- 1
    }
  }
}

rm(A, Q4, sub, i)

# Remove Sustainability report, independent audit and pilar 3 report
earning_data$remove[earning_data$Security == "ASA" & earning_data$date == "2020-06-12"] <- 1
earning_data$remove[earning_data$Security == "LIFEME" & earning_data$date == "2020-06-18"] <- 1

earning_data$remove[earning_data$Security == "VISTIN" & earning_data$date == "2020-05-29"] <- 1

earning_data$remove[earning_data$Security == "SVEG" & earning_data$date == "2020-04-28"] <- 1


# Remove the annual reports that are released after a Q4 ++
earning_data <- earning_data[is.na(earning_data$remove),] %>% select(-c(remove, type, Q, A))

earning_data$quarter[earning_data$date %within% interval(ymd("2020-01-01"), ymd("2020-03-31"))] <- "Q4"

# Remove duplicated dates with about 40 days in between
earning_data <- earning_data %>% group_by(Security) %>% arrange(date, .by_group = TRUE) %>%
  mutate(DUP = ifelse(abs(difftime(date, lead(date))) == 0,1,0)) %>%
  subset(DUP %in%  0| DUP %in%  NA) %>% select(-DUP) %>% arrange(date, .by_group = TRUE) %>%
  mutate(diff = difftime(date, lag(date), units = c("days")),
         quarter = ifelse(quarter == "A", "Q4",quarter),
         DUP = ifelse(quarter == lag(quarter) ,1,0))

# Manually mark the observations we want to keep
earning_data$DUP[earning_data$Security == "HELG" & earning_data$date == "2020-01-29"] <- 1
earning_data$DUP[earning_data$Security == "HELG" & earning_data$date == "2020-02-05"] <- 0

earning_data$DUP[earning_data$Security == "KCC" & earning_data$date == "2020-04-07"] <- 1
earning_data$DUP[earning_data$Security == "KCC" & earning_data$date == "2020-05-27"] <- 0

earning_data$DUP[earning_data$Security == "KID" & earning_data$date == "2020-01-15"] <- 1
earning_data$DUP[earning_data$Security == "KID" & earning_data$date == "2020-02-26"] <- 0

earning_data$DUP[earning_data$Security == "KID" & earning_data$date == "2020-07-07"] <- 1
earning_data$DUP[earning_data$Security == "KID" & earning_data$date == "2020-08-20"] <- 0

earning_data$DUP[earning_data$Security == "NATTO" & earning_data$date == "2020-04-06"] <- 1
earning_data$DUP[earning_data$Security == "NATTO" & earning_data$date == "2020-05-06"] <- 0

earning_data$DUP[earning_data$Security == "NONG" & earning_data$date == "2020-01-24"] <- 1
earning_data$DUP[earning_data$Security == "NONG" & earning_data$date == "2020-02-13"] <- 0

earning_data$DUP[earning_data$Security == "NONG" & earning_data$date == "2020-07-09"] <- 1
earning_data$DUP[earning_data$Security == "NONG" & earning_data$date == "2020-08-12"] <- 0

earning_data$DUP[earning_data$Security == "OET" & earning_data$date == "2020-01-22"] <- 1
earning_data$DUP[earning_data$Security == "OET" & earning_data$date == "2020-02-28"] <- 0

earning_data$DUP[earning_data$Security == "SBVG" & earning_data$date == "2020-01-28"] <- 1
earning_data$DUP[earning_data$Security == "SBVG" & earning_data$date == "2020-02-07"] <- 0

earning_data <- earning_data %>% 
  subset(DUP %in%  0| DUP %in%  NA) %>% 
  select(-c(DUP, diff)) %>% 
  arrange(date, .by_group = TRUE)



# Save to csv
write.csv(earning_data, file = "Stock_data/earning_data2.csv")




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


