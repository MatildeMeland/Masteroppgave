library(readxl)

news_data <- read_xlsx("NRK/Artikler NRK.no oktober 2019-september 2020.xlsx")

# Remove all articles that doesn't have subject
news_data_na <- subset(news_data, !is.na(news_data$Subjekt))

# Creates a new subset of all articles with only subject is corona
corona_only_news <- subset(news_data, Subjekt == "Nytt koronavirus (Covid-19)")

# Creates a new subset of all corona articles 
corona_news <- news_data[grep("Covid-19", news_data$Subjekt), ]

