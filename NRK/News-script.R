library(readxl)
library(tidyverse)
library(lubridate)
library(stringr)
library(lmtest)
library(sandwich)

# Loading and preparing data ----------------------------------------------
news_data <- read_xlsx("NRK/Artikler NRK.no oktober 2019-september 2020.xlsx") %>% 
  rename( date = `Dato publisert`, # Rename columns
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
  
news_data <- news_data %>% 
  select(date, title, pageviews, pageviews_mobile, xl, read_time, read_time_total,
         sentences_count, subject, url, word_count) %>% # Select columns we want to work with
  mutate(xl = ifelse(xl == 'XL', 1, 0)) %>% # Dummy varible for XL articles
  subset(!is.na(news_data$subject)) # Remove all articles that doesn't have subject


# Creates a new subset of all corona articles 
news_corona <- news_data[grep("Covid-19|korona", news_data$subject, ignore.case = T), ] %>%  # Corona articles
  mutate(type = "corona")

# Remove economic subjects
#news_data <- news_data[!grepl("økonomi|næringsliv", news_data$subject, ignore.case = T), ]

sum(news_corona$pageviews) # Total veiws on Corona articles (VG.no had 515M while Dagbladet.no had 220M)

news_other <- news_data[!grepl("Covid-19|korona", news_data$subject, ignore.case = T), ] %>% # Other atricles
  mutate(type = "other")

#write.csv(news_data, file = "Stock_data/news_data.csv")
#write.csv(news_corona, file = "Stock_data/news_corona.csv")
#write.csv(news_not_corona, file = "Stock_data/news_not_corona.csv")


# Plotting ---------------------------------------------------
# Number of articles
news_articles <- function(data) {
  plot <- data
  
  table <- as.data.frame(table(data$date))
  table$Var1 <- as.Date(table$Var1)
  
  ggplot(data = table, aes(x = Var1, y = Freq)) +
    geom_bar(stat = "identity", fill = "lightblue") + # Uncomment to show
    # geom_line(color = "darkblue") + # Uncomment to show
    labs(title = "Amount of Corona Articles by NRK",
         subtitle = "October 2019 - September 2020",
         x = "Date", y = "Number of Articles") +
    scale_x_date(date_labels = "%d %b %Y",date_breaks  ="1 month") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))
}

news_articles(news_data)
news_articles(news_corona) # Amount of Corona articles over time
news_articles(news_other)



# Other variables
news_variables <- function(data, variable){
  
  plot <- data
  plot$plot <- variable
  
  # Millions with pageviews? ++ Add this
  # Also hours with total read time
  
  plot %>% 
    select(date, plot) %>% 
    group_by(date) %>% 
    summarise(clicks.sum = sum(plot)) %>%
    
    ggplot(., aes(x = as.Date(date), y = clicks.sum)) +
    geom_line() +
    geom_smooth(method = "lm", color = "darkgray") +
    labs(title = paste0("Total ", str_to_sentence(gsub("^.+\\$", "", deparse(substitute(variable)))), " of All ", str_to_sentence(data$type[1]), " Articles by NRK"),
         # subtitle = "October 2019 - September 2020",
         x = "Date", y = str_to_sentence(gsub("^.+\\$", "", deparse(substitute(variable))))) +
    scale_x_date(date_labels = "%d %b %Y",date_breaks  ="1 month") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))
}

# All articles
news_variables(news_data, news_data$pageviews)
news_variables(news_data, news_data$read_time)
news_variables(news_data, news_data$read_time_total)

# Corona articles
news_variables(news_corona, news_corona$pageviews)
news_variables(news_corona, news_corona$read_time)
news_variables(news_corona, news_corona$read_time_total)

# Not corona articles
news_variables(news_other, news_other$pageviews)
news_variables(news_other, news_other$read_time)
news_variables(news_other, news_other$read_time_total)




# Comparing two types of data -------------------------------
news_compare <- function(data1, data2, variable, normalized = "No", interval = "daily"){
 
  plot1 <- data1
  plot2 <- data2
  plot1$plot <- data1[, variable]
  plot2$plot <- data2[, variable]
  
  #This is not done
  plot1$time <- ifelse(interval == "weekly", strftime(plot1$date, format = "%V"), plot1$date) 
  plot2$time <- ifelse(interval == "weekly", strftime(plot2$date, format = "%V"), plot2$date)
  
  temp1 <- plot1 %>% 
    select(date, plot) %>% 
    mutate(nr_articles = n_distinct(date)) %>% 
    group_by(date) %>% 
    summarise(sum = sum(plot), articles_sum = n()) %>% 
    mutate(pr_article = sum/articles_sum) %>% 
    mutate(type = plot1$type[1])
  
  temp2 <- plot2 %>% 
    select(date, plot) %>% 
    mutate(nr_articles = n_distinct(date)) %>% 
    group_by(date) %>% 
    summarise(sum = sum(plot), articles_sum = n()) %>% 
    mutate(pr_article = sum/articles_sum) %>%
    mutate(type = plot2$type[1])
  
  temp <- rbind(temp1, temp2)
  temp$plotting <- unlist(temp[, ifelse(normalized == "Yes", "pr_article", "sum")])
    
  temp %>% 
    ggplot(., aes(x = as.Date(date), y = plotting, color = type)) +
    geom_line() +
    labs(title = "Total Pageviews of Corona Articles (pink) and All Other Articles (blue) by NRK (MILL)",
         subtitle = "October 2019 - September 2020",
         x = "Date", y = "Million pageviews") +
    scale_x_date(date_labels = "%d %b %Y", date_breaks  ="1 month") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))
}


## Not normalized, daily
# Compare Corona to all other articles
news_compare(news_corona, news_other, "pageviews")
news_compare(news_corona, news_other, "read_time")
news_compare(news_corona, news_other, "read_time_total")

# Compare Corona to all articles
news_data$type <- "all"
news_compare(news_corona, news_data, "pageviews")
news_compare(news_corona, news_data, "read_time")
news_compare(news_corona, news_data, "read_time_total")


## Normalized, daily
# Compare Corona to all other articles
news_compare(news_corona, news_other, "pageviews", "Yes")
news_compare(news_corona, news_other, "read_time", "Yes")
news_compare(news_corona, news_other, "read_time_total", "Yes")

# Compare Corona to all articles
news_compare(news_corona, news_data, "pageviews", "Yes")
news_compare(news_corona, news_data, "read_time", "Yes")
news_compare(news_corona, news_data, "read_time_total", "Yes")

news_compare(news_corona, news_other, "pageviews", "No", "Weekly") # Not working yet


# PERCENT of articles that were related to corona on a day 
## THIS IS NOT DONE

ggplot(data = news_plot_table, aes(x = as.Date(date), fill = type)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) 

# PERCENT of pageviews that were related to corona on a day

## FIX FLIP THE CHATEGORIES
news_plot_table$type <- factor(news_plot_table$type, levels = c("not corona", "corona"))

ggplot(news_plot_table, mapping = aes(x = as.Date(date), y = clicks_sum, fill = type)) +
  geom_bar(position = "fill", stat = "identity") +
  scale_fill_manual(values = c("black", "blue")) +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "Percentage of Total Pageviews of All Articles Contributed by Corona Articles",
       subtitle = "October 2019 - September 2020",
       x = "Date", y = "Percentage") +
    scale_x_date(date_labels = "%d %b %Y",date_breaks  ="1 month") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))




# Weekly data
news_corona$week <- week(as.Date(news_corona$date, format = "%Y-%m-%d"))
news_corona$year <- year(as.Date(news_corona$date, format = "%Y-%m-%d"))

news_other$week <- week(as.Date(news_other$date, format = "%Y-%m-%d"))
news_other$year <- year(as.Date(news_other$date, format = "%Y-%m-%d"))


temp1 <- news_corona %>% 
  select(date, year, week, pageviews, type) %>% 
  group_by(year, week) %>% 
  mutate(sum_page = sum(pageviews), articles_sum = n()) %>% 
  mutate(pr_article = sum_page/articles_sum)
  
temp2 <- news_other %>% 
  select(date, year, week, pageviews, type) %>% 
  group_by(year, week) %>% 
  mutate(sum_page = sum(pageviews), articles_sum = n()) %>% 
  mutate(pr_article = sum_page/articles_sum)
  
temp <- rbind(temp1, temp2) %>% group_by(week) %>% filter(!duplicated(pr_article))

rm(temp1, temp2)

temp %>% 
  ggplot(., aes(x = as.Date(date), y = sum_page, color = type)) +
  geom_line() +
    labs(title = "Total Pageviews of Corona Articles (pink) and All Other Articles (blue) by NRK (MILL)",
         subtitle = "October 2019 - September 2020",
         x = "Date", y = "Million pageviews") +
    scale_x_date(date_labels = "%d %b %Y", date_breaks = "1 month") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))








# Formating finance news --------------------------------------------------
# Analzing the different subjects
subjects <- strsplit(news_data$subject, split = ";") %>% 
  unlist() %>% tolower() %>% unique()  # All unique subjects

# Most frequent subjects
table(subjects_all) %>% 
  sort(decreasing = T) %>% 
  head(100)

table_sub <- as.data.frame(table(subjects_all))
# Piciking finance subjects
test_finance <- news_data[grep("politikk", news_data$subject, ignore.case = T), ] # Change subject to investegate what articles are in it
test_finance[sample(nrow(test_finance), 20), 1:2]

# Mest relevat er økonomi, Equinor, olje og gass, teknologi og data, 
# Litt mer usikker på temaene politikk, Donald Trump, USA osv.
# Subsetting with all chosen finance subjects
news_finance <- news_data[grep("økonomi|trump|teknologi|energi|reiseliv|olje|equinor|fiskeri|næringsliv", news_data$subject, ignore.case = T), ] %>% 
  mutate(type = "finance")

#news_finance <- news_finance[grep("", news_data$full_text, ignore.case = T), ] # When we add full text we can add words that have to be included
#news_finance <- news_finance[-grep("", news_data$full_text, ignore.case = T), ] # Words we want to exclue - maybe we want to remove articles which mentions corona 5 or more times or something?

# Random sample of articles form this sample (to check that they match)
news_finance[sample(nrow(news_finance), 20), 1:2]


# Plotting finance articles -----------------------------------------------
# Number of finance articles by day
news_articles(news_finance)

news_variables(news_finance, news_finance$pageviews) # Clicks on finance articles
news_variables(news_finance, news_finance$read_time) # Average read time on finance articles
news_variables(news_finance, news_finance$read_time_total) # Total read time on finance articles



# Ecnonometric analysis
# Create dataset
news_data <- read.csv("Stock_data/news_data.csv") %>%  select(-X)
news_corona <- read.csv("Stock_data/news_corona.csv")[,-1] %>% 
  mutate(type = "corona",
         day = wday(date, label = T), 
         month = month(date, label = T),
         year = year(date)) %>% 
  filter(!day == "Sat" & !day == "Sun" & !month == "Jan")

news_other <- read.csv("Stock_data/news_not_corona.csv")[,-1] %>% 
  mutate(type = "other",
         day = wday(date, label = T), 
         month = month(date, label = T),
         year = year(date)) %>% 
  filter(!day == "Sat" & !day == "Sun" & !month == "Jan" & year == "2020")


# Formatting newsdata for regression 
news_data_formatted1 <- news_other %>% 
  select(date, pageviews, read_time_total) %>% 
  group_by(date) %>% 
  summarise(clicks_sum = sum(pageviews), 
            read_sum = sum(read_time_total),
            articles_sum = n()) %>%
  mutate(clicks_article = clicks_sum/articles_sum,
         read_article = read_sum/articles_sum)

news_data_formatted2 <- news_corona %>% 
  select(date, pageviews, read_time_total) %>% 
  group_by(date) %>% 
  summarise(clicks_sum = sum(pageviews), 
            read_sum = sum(read_time_total),
            articles_sum = n()) %>%
  mutate(clicks_article = clicks_sum/articles_sum,
         read_article = read_sum/articles_sum)


# Merge corona and not corona data
news_data_formatted <- merge(news_data_formatted1, news_data_formatted2, by=1, all.x = T)
rm(news_data_formatted1, news_data_formatted2)

news_data_formatted$date <- as.Date(news_data_formatted$date) # Make sure it is formated as a date

# Make variable for gov announcments
government_tv <- read_csv("Stock_data/government_tv.csv")[,-1] # Load data 

news_data_formatted$gov <- ifelse(news_data_formatted$date %in% government_tv$date, 1, 0)

# Drop some columns
news_data_formatted <- news_data_formatted %>% 
  select(-clicks_sum.x, -articles_sum.x, -clicks_sum.y, -articles_sum.y, -read_sum.x, -read_sum.y)

news_data_formatted$clicks_article.y[is.na(news_data_formatted$clicks_article.y)] <- 0
news_data_formatted$read_article.y[is.na(news_data_formatted$read_article.y)] <- 0


#change of column names
colnames(news_data_formatted)[2:5] <- c("other_ca", "other_ra", "corona_ca", "corona_ra")

# Control variables
news_data_formatted$month <- format(news_data_formatted$date, "%B")
news_data_formatted$day <- format(news_data_formatted$date, "%A")


# Linear model and summary statistics

# Pageviews
summary(mod1 <- lm(log(1 + other_ca) ~ log(1 + corona_ca), data = news_data_formatted))
summary(mod1 <- lm(log(1 + corona_ca) ~ log(1 + other_ca), data = news_data_formatted))

# Readtime
summary(mod1 <- lm(log(1 + other_ra) ~ log(1 + corona_ra) + month + day, data = news_data_formatted))
summary(mod1 <- lm(log(1 + corona_ra) ~ log(1 + other_ra), data = news_data_formatted))

summary(lm(log(1 + corona_ca) ~ gov + month + day, data = news_data_formatted))


dwtest(mod1) # There is autocorrelation
bptest(mod1) # There is heteroskedasticity


# robust standard errors
(se_2 <- coeftest(mod1, vcov=vcovHC(mod1, type="HC0")))


# tables showing:
# - average readership/article for corona and others each month and standard deviation
# - number of articles 
# - average percentage readership in different weekdays
# - per day

news_data_formatted %>% group_by(month) %>% summarise(sum_other = sum(other_ca),
                                                      sum_corona = sum(corona_ca),
                                                      sd_other = sd(other_ca),
                                                      sd_corona = sd(corona_ca))

summary(news_data_formatted)






# Weekly data
news_data_formatted <- merge(temp[temp$type == "corona",], temp[temp$type == "other",], by = "week") 

news_data_formatted$month <- format(news_data_formatted$date.x,"%B")

news_data_formatted <- news_data_formatted %>% 
  select(-c(pageviews.x, articles_sum.x, pageviews.y, sum_page.x, sum_page.y, articles_sum.y, date.x, date.y, year.x, year.y, type.x, type.y))

colnames(news_data_formatted)[2:3] <- c("corona_ca", "other_ca")

# Weekly
summary(mod1 <- lm(log(1 + other_ca) ~ log(1 + corona_ca) + month + as.factor(week), data=news_data_formatted))

summary(lm(log(1 + corona_ca) ~ month, data=news_data_formatted))



# robust standard errors
(se_2 <- coeftest(mod1, vcov=vcovHC(mod1, type="HC0")))


# why is this here?
# Leftover code removed from stock_analysis3 ------------------------------

# Variable for amount of corona news - Not done
# news_corona <- read_csv("Stock_data/news_corona.csv")
# 
# news_data_formatted <- news_corona[,-1] %>% 
#   select(date, pageviews) %>% 
#   mutate(nr_articles = n_distinct(date)) %>% 
#   group_by(date) %>% 
#   summarise(clicks.sum = sum(pageviews), articles.sum = n()) %>% 
#   mutate(clicks_article = clicks.sum/articles.sum)
# 
# news_data_formatted$news <- cut(news_data_formatted$clicks_article,
#                                 quantile(news_data_formatted$clicks_article, c(0, .25, .50, .75, 1)),
#                                 labels = c("very low", "low", "high", "very high"),
#                                 include.lowest = TRUE)
# 
# # Create the variable in the main dataframe
# stock_data$news_corona <- news_data_formatted$news[match(stock_data$date, as.Date(news_data_formatted$date))]



