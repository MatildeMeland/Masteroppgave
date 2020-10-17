library(readxl)
library(tidyverse)

# Loading and preparing data ----------------------------------------------

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


# Format Corona data ------------------------------------------------------
# Creates a new subset of all articles with only subject is corona
corona_only_news <- subset(news_data, subject == "Nytt koronavirus (Covid-19)")

# Creates a new subset of all corona articles 
news_corona <- news_data[grep("Covid-19", news_data$subject, ignore.case = T), ]
sum(news_corona$pageviews) # Total veiws on Corona articles (VG.no had 515M while Dagbladet.no had 220M)


# Amount of articles ------------------------------------------------------
# Making a plot of corona articles over time
table_corona_articles <- as.data.frame(table(news_corona$date))
table_corona_articles$Var1 <- as.Date(table_corona_articles$Var1)

ggplot(table_corona_articles, aes(x = Var1, y = Freq)) + 
  geom_point()

# Histogram
ggplot(data = table_corona_articles, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(title = "Amount of Corona Articles by NRK",
       subtitle = "October 2019 - September 2020",
       x = "Date", y = "Number of Articles") +
  scale_x_date(date_labels = "%d %b %Y",date_breaks  ="1 month") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

# Line plot
ggplot(data = table_corona_articles, aes(x = Var1, y = Freq)) +
  geom_line(color = "darkblue") +
  labs(title = "Amount of Corona Articles by NRK",
       subtitle = "October 2019 - September 2020",
       x = "Date", y = "Number of Articles") +
  scale_x_date(date_labels = "%d %b %Y",date_breaks  ="1 month") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

# Clicks of corona articles -----------------------------------------------
# Making a plot of clicks of corona articles over time

ggplot(news_corona, aes(x = date, y = pageviews)) + 
  geom_point() # Remove outliers?

# Top read time articles
news_corona %>% 
  select(title, pageviews) 
# %>% sort(pageviews, decreasing = T) Får ikke denne til å funke plutselig

## Plotting total hours per day of all articles
news_corona %>% 
  select(date, pageviews) %>% 
  group_by(date) %>% 
  summarise(clicks.sum = sum(pageviews)) %>%
  
  ggplot(., aes(x = as.Date(date), y = clicks.sum/(10^6))) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(title = "Total pageviews of Corona Articles by NRK (MILL)",
       subtitle = "October 2019 - September 2020",
       x = "Date", y = "Million pageviews") +
  scale_x_date(date_labels = "%d %b %Y",date_breaks  ="1 month") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))


# Readtime of corona articles ---------------------------------------------
# Making a plot of reading time of corona articles over time
# Changing the total time from seconds to hours
news_corona$read_time_total <- news_corona$read_time_total/360

ggplot(news_corona, aes(x = date, y = read_time_total)) + 
  geom_point() # Plot of all articles by date (remove outliers?)

# Top read time articles
news_corona %>% 
  select(title, read_time_total) %>% 
  sort(read_time_total, decreasing = T)

## Formating the data for plotting total hours per day of all articles
table_corona_time <- news_corona %>% 
  select(date, read_time_total) %>% 
  group_by(date) %>% 
  summarise(time.sum = sum(read_time_total))

table_corona_time$date <- as.Date(table_corona_time$date)

ggplot(data = table_corona_time, aes(x = date, y = time.sum)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(title = "Total readtime of Corona Articles by NRK (hours)",
       subtitle = "October 2019 - September 2020",
       x = "Date", y = "Number of hours read") +
  scale_x_date(date_labels = "%d %b %Y",date_breaks  ="1 month") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

# Doing the same without XL articles (29 articles of 3660 - don't expect a lot)
table_corona_time_noxl <- news_corona[news_corona$xl != 1, ]

table_corona_time_noxl <- news_corona %>% 
  select(date, read_time_total) %>% 
  group_by(date) %>% 
  summarise(time.sum = sum(read_time_total))

table_corona_time_noxl$date <- as.Date(table_corona_time_noxl$date)

ggplot(data = table_corona_time_noxl, aes(x = date, y = time.sum)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(title = "Total readtime of Corona Articles by NRK (hours), no XL articles",
       subtitle = "October 2019 - September 2020",
       x = "Date", y = "Number of hours read") +
  scale_x_date(date_labels = "%d %b %Y",date_breaks  ="1 month") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) # Pretty much no difference - don't think we need to do this


# Analzing the different subjects -----------------------------------------
sub <- sort(table(news_corona$subject))

subjects <- news_data$subject

subjects_split <- strsplit(news_data$subject, split = ";")

subjects_all <- unlist(subjects_split)

subjects_unique <- unique(subjects_all) # All unique subjects

# Most frequent subjects
table(subjects_all) %>% 
  sort(decreasing = T) %>% 
  head(100)

table_sub <- as.data.frame(table(subjects_all))


# Formating finance news --------------------------------------------------
# Piciking finance subjects
test_finance <- news_data[grep("Reiseliv", news_data$subject, ignore.case = T), ] # Change subject to investegate what articles are in it
test_finance[sample(nrow(test_finance), 20), 1:2]

# Mest relevat er økonomi, Equinor, olje og gass, teknologi og data, 
# Litt mer usikker på temaene politikk, Donald Trump, USA osv.
# Subsetting with all chosen finance subjects
news_finance <- news_data[grep("økonomi|politikk|trump|teknologi|energi|reiseliv|olje|equinor", news_data$subject, ignore.case = T), ]

#news_finance <- news_finance[grep("", news_data$full_text, ignore.case = T), ] # When we add full text we can add words that have to be included
#news_finance <- news_finance[-grep("", news_data$full_text, ignore.case = T), ] # Words we want to exclue - maybe we want to remove articles which mentions corona 5 or more times or something?

# Random sample of articles form this sample (to check that they match)
news_finance[sample(nrow(news_finance), 20), 1:2]

# Making a plot of corona articles over time
table_finance <- as.data.frame(table(news_finance$date))
table_finance$Var1 <- as.Date(table_finance$Var1)

# Plotting finance articles -----------------------------------------------
# Number of finance articles by day
ggplot(data = table_finance, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(title = "Amount of Articles by NRK on Finance Related Topics",
       subtitle = "October 2019 - September 2020",
       x = "Date", y = "Number of Articles") +
  scale_x_date(date_labels = "%d %b %Y",date_breaks  ="1 month") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Clicks on finance articles
ggplot(news_finance, aes(x = date, y = pageviews)) + 
  geom_point() # Remove outliers?
# "Spørsmål og svar om koronautbruddet" - This article has insane hits, think we should remove

# Total number of pageviews by day of finance news
news_finance %>% 
  select(date, pageviews) %>% 
  group_by(date) %>% 
  summarise(clicks.sum = sum(pageviews)) %>%
  
  ggplot(., aes(x = as.Date(date), y = clicks.sum/(10^6))) +
  geom_bar(stat = "identity", fill = "lightblue") +
  geom_line() +
  geom_smooth(method = "lm", color = "darkgray") +
  labs(title = "Total Pageviews of Finance Articles by NRK (MILL)",
       subtitle = "October 2019 - September 2020",
       x = "Date", y = "Million Pageviews") +
  scale_x_date(date_labels = "%d %b %Y",date_breaks  ="1 month") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

# Total number of pageviews by day of finance news
news_finance %>% 
  select(date, read_time) %>% 
  group_by(date) %>% 
  summarise(readtime.sum = sum(read_time)) %>%
  
  ggplot(., aes(x = as.Date(date), y = readtime.sum/(360))) +
  geom_bar(stat = "identity", fill = "lightblue") +
  geom_line() +
  geom_smooth(method = "lm", color = "darkgray") +
  labs(title = "Total Read Time of Finance Articles by NRK (hours)",
       subtitle = "October 2019 - September 2020",
       x = "Date", y = "Hours of Read Time") +
  scale_x_date(date_labels = "%d %b %Y",date_breaks  ="1 month") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))


