library(httr)
library(jsonlite)
library(tidyverse)

res <- POST("https://newsweb.oslobors.no/obsvc/news.obsvc?category=1002&fromDate=2020-01-01&issuer=&toDate=2020-03-01", verbose())
x <- content(res, as = "text")
input <- fromJSON(x)

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
data$date <- 
  gsub("T.+","",input$data$messages$publishedTime) 

newdate <- strptime(as.character(data$date), "%Y-%m-%d")
data$date <- format(newdate, "%d.%m.%Y")

# Remove duplicats
data <- unique(data)

length(unique(data$name))


