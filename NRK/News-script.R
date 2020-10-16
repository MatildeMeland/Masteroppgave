library(readxl)
library(tidyverse)

news_data <- read_xlsx("NRK/Artikler NRK.no oktober 2019-september 2020.xlsx")

news_data <-
  news_data %>% 
  rename(
    date = `Dato publisert`,
    title = `Artikkel Tittel`,
    pageviews = Pageviews,
    pageviews_mobile = `Pageviews Mobile`,
    xl = `Content Type Annen Tabell`,
    read_time = Lesetid,
    read_time_total = `Tot tid brukt`,
    sentences_count = `Sentence Count`,
    subject = Subjekt,
    url = Url,
    word_count = Wordcount
  )

news_data <-
  news_data %>% 
  select(date, title, pageviews, pageviews_mobile, xl, read_time, read_time_total,
         sentences_count, subject, url, word_count)

# FIX - lage dummy variabel med 0/1 for xl artikler

# Remove all articles that doesn't have subject
news_data_na <- subset(news_data, !is.na(news_data$subject))

# Creates a new subset of all articles with only subject is corona
corona_only_news <- subset(news_data, subject == "Nytt koronavirus (Covid-19)")

# Creates a new subset of all corona articles 
corona_news <- news_data[grep("Covid-19", news_data$subject, ignore.case = T), ]

# Making a plot of corona articles over time 
plot(table(corona_news$date))


# More advanced plot
table_corona <- as.data.frame(table(corona_news$date))
table_corona$Var1 <- as.Date(table_corona$Var1)

ggplot(table_corona, aes(x = Var1, y = Freq)) + 
  geom_point()

ggplot(data = table_corona, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(title = "Amount of Corona Articles by NRK",
       subtitle = "October 2019 - September 2020",
       x = "Date", y = "Number of Articles") +
  scale_x_date(date_labels = "%d %b %Y") +
  theme_bw() +
  theme(axis.text.x=element_text(angle=60, hjust=1)) 

# Making a plot of reading time of corona articles over time
## FIX - gjøre om total tid til timer
## Fix - lage tabel over dato og sammenlagt tid for alle artikler på en dag
ggplot(corona_news, aes(x = date, y = read_time_total)) + 
  geom_point()


# Analzing the different subjects
sub <- sort(table(corona_news$Subjekt))

subjects <- news_data_na$Subjekt

subjects_split <- strsplit(news_data_na$Subjekt, split = ";")

subjects_all <- unlist(subjects_split)


# All unique subjects
subjects_unique <- unique(subjects_all)

# Most frequent subjects
table(subjects_all) %>% 
  sort(decreasing = T) %>% 
  head(100)

table_sub <- as.data.frame(table(subjects_all))

sum(corona_news$pageviews)
