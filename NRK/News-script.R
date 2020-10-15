library(readxl)

news_data <- read_xlsx("NRK/Artikler NRK.no oktober 2019-september 2020.xlsx")

# Remove all articles that doesn't have subject
news_data_na <- subset(news_data, !is.na(news_data$Subjekt))

# Creates a new subset of all articles with only subject is corona
corona_only_news <- subset(news_data, Subjekt == "Nytt koronavirus (Covid-19)")

# Creates a new subset of all corona articles 
corona_news <- news_data[grep("Covid-19", news_data$Subjekt), ]

# Making a plot of corona articles over time 
plot(table(corona_news$`Dato publisert`))

# More advanced plot


# Analzing the different subjects
sub <- sort(table(corona_news$Subjekt))

subjects <- news_data_na$Subjekt

subjects_split <- strsplit(news_data_na$Subjekt, split = ";")

subjects_all <- unlist(subjects_split)

# All unique subjects
subjects_unique <- unique(subjects_all)

# Most frequent subjects
table_sub <- as.table(table(subjects_all)) %>% 
  sort(decreasing = T) %>% 
  head(100)


