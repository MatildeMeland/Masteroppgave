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
    word_count = Wordcount)

news_data <-
  news_data %>% 
  select(date, title, pageviews, pageviews_mobile, xl, read_time, read_time_total,
         sentences_count, subject, url, word_count)

# Dummy varible for XL articles
news_data$xl <- ifelse(news_data$xl == 'XL', 1, 0)

# Remove all articles that doesn't have subject
news_data <- subset(news_data, !is.na(news_data$subject))

# Creates a new subset of all articles with only subject is corona
corona_only_news <- subset(news_data, subject == "Nytt koronavirus (Covid-19)")

# Creates a new subset of all corona articles 
corona_news <- news_data[grep("Covid-19", news_data$subject, ignore.case = T), ]
sum(corona_news$pageviews) # Total veiws on Corona articles (VG.no had 515M while Dagbladet.no had 220M)

# Making a plot of corona articles over time
plot(table(corona_news$date))

# More advanced plot
table_corona_clicks <- as.data.frame(table(corona_news$date))
table_corona_clicks$Var1 <- as.Date(table_corona$Var1)

ggplot(table_corona_clicks, aes(x = Var1, y = Freq)) + 
  geom_point()

ggplot(data = table_corona_clicks, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(title = "Amount of Corona Articles by NRK",
       subtitle = "October 2019 - September 2020",
       x = "Date", y = "Number of Articles") +
  scale_x_date(date_labels = "%d %b %Y") + # FIX date (only 3 months show up suddenly?)
  theme_bw()

# Making a plot of reading time of corona articles over time
# Changing the total time from seconds to hours
corona_news$read_time_total <- corona_news$read_time_total/360

## Fix - lage tabel over dato og sammenlagt tid for alle artikler på en dag
table_corona_time <- corona_news %>% 
  select(date, read_time_total) %>% 
  group_by(date) %>% 
  summarise(time.sum = sum(read_time_total))

ggplot(corona_news, aes(x = date, y = read_time_total)) + 
  geom_point()




# Analzing the different subjects
sub <- sort(table(corona_news$subject))

subjects <- news_data_na$subject

subjects_split <- strsplit(news_data_na$subject, split = ";")

subjects_all <- unlist(subjects_split)


# All unique subjects
subjects_unique <- unique(subjects_all)

# Most frequent subjects
table(subjects_all) %>% 
  sort(decreasing = T) %>% 
  head(100)

table_sub <- as.data.frame(table(subjects_all))

# FIXX se på temaer som er relatert til finans, lage tidslinje
