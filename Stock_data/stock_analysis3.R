# Libraries
library(readxl)
library(tidyverse)
library(zoo)
library(lubridate)

# Load and format stock data ----------------------------------------------------
stock_data <- read_excel("Stock_data/stock_final.xlsx") %>% as.data.frame()

set.seed(123) # For reproducible results 

# Fix problem with mismatching dates
stock_data <- 
  merge(stock_data[,-2], stock_data[,1:2], by=1:2, all.y = T) %>% 
  select(-c(EQY_SH_OUT,VOLATILITY_30D))

colnames(stock_data)[2] <- "date"

stock_data$date <- as.Date(stock_data$date, format="%d.%m.%Y")

# Change structure of variables from character to numeric
stock_data[,3:6] <- sapply(stock_data[,3:6],as.numeric)

# Change NA to 0 in volume
stock_data$PX_VOLUME[is.na(stock_data$PX_VOLUME)] <- 0

# Change NA's in PX_LAST to last value of PX_LAST
for (i in unique(stock_data$Security)) {
  stock_data$PX_LAST[stock_data$Security == i] <- na.locf(stock_data$PX_LAST[stock_data$Security == i], na.rm = FALSE)
}


# Same fix for PX_OPEN
for (i in unique(stock_data$Security)) {
  x <- min(as.numeric(row.names(stock_data[stock_data$Security == i,]))) + 1
  z <- max(as.numeric(row.names(stock_data[stock_data$Security == i,])))
  
  for (j in x:z) {
    if (is.na(stock_data$PX_OPEN[j])) {
      stock_data$PX_OPEN[j] <- stock_data$PX_LAST[j-1]
    }
  }
}


# Create a column with industry specifications
Ticker_list <- read_excel("Peer_companies/Ticker-list.xlsx") # list of companies with industry specification

colnames(Ticker_list)[5] <- "Security" 

stock_data <- merge(stock_data, Ticker_list, by = "Security") %>%  # merge with stock data
  select(-c(ticker, name, industri))

# Remove unnecessary variables
rm(Ticker_list, i, j, x, z)


# Cumulative abnormal volume and return ----------------------------------------------
stock_data <- stock_data %>% 
  group_by(industry, date) %>%
  mutate(total_mkt_cap = sum(CUR_MKT_CAP, na.rm = T)-CUR_MKT_CAP, # total market cap for each day in each industy minus current company
         val = PX_VOLUME*PX_LAST,                                 # value traded each day for each company
         total_val = sum(val, na.rm = T)-val) %>%                 # total value traded each day in each industry minus current company
  ungroup %>%
  arrange(., Security) %>% 
  group_by(Security) %>% 
  mutate(AV_DV = log(val+1) - (rollsumr(log(lag(val+1, n = 11)), k = 30, fill = NA)/30),   # Abnormal volume for each company
         PX_5 = lag(PX_LAST, n=5),                                                         # Price five days ago, needed later
         MR = c(NA,diff(total_mkt_cap))/lag(total_mkt_cap, 1),                             # Market return, diff takes the difference between last observation and current
         daily_return = (PX_LAST-PX_OPEN)/PX_OPEN,                                         # Daily return
         
         CAR1 = c(rep(NA,times = 1), as.numeric(rollapply(1 + daily_return, 2, prod,partial = FALSE, align = "left"))) # Cumulative abnormal return (CAR) for each company in each industry first 2 days
         -c(rep(NA,times = 1), as.numeric(rollapply(1 + MR, 2, prod,partial = FALSE, align = "left"))),
         
         CAR40 = c(rep(NA,times = 39), as.numeric(rollapply(1 + daily_return, 40, prod,partial = FALSE, align = "left"))) # Cumulative abnormal return (CAR) for each company in each industry first 40 days
         -c(rep(NA,times = 39), as.numeric(rollapply(1 + MR, 40, prod,partial = FALSE, align = "left")))) %>% 
  group_by(industry,date) %>%
  mutate(AV_industy = ((sum(AV_DV, na.rm = T, fill = NA)-AV_DV)/(n()-ifelse(n() == sum(is.na(AV_DV)),1,0)-sum(is.na(AV_DV))))) %>% # Mean abnormal volume for industry
  group_by(Security) %>% 
  mutate(AV_alt = AV_DV - AV_industy) %>% # Abnormal volume minus mean industy abnormal volume
  select(-c(AV_DV,daily_return, MR, val , total_val, total_mkt_cap, PX_OPEN, AV_industy)) # Remove variables used for calculations


# Creating control variables -------------------------------------------

# Price before corona
# Some companies listed in 2020 will not have this value. Add an ifelse statement and use PX-5?
for (i in unique(stock_data$Security)) {
  stock_data$PX_normal[stock_data$Security == i] <- stock_data[stock_data$date == as.Date("2020-01-31") & stock_data$Security == i,]$PX_LAST
}

stock_data <- stock_data %>% group_by(Security) %>% mutate(PX_normal = ifelse(is.na(PX_normal),PX_5,PX_normal)) %>% 
  select(-c(PX_5))

# Varaible for month & day
stock_data$month <- format(stock_data$date,"%B")
stock_data$day <- format(stock_data$date,"%A")

# Dummy variable for government announcments
government_tv <- read_csv("Stock_data/government_tv.csv")[,-1] # Load data 

stock_data$gov <- ifelse(stock_data$date %in% government_tv$date, 1, 0)

rm(government_tv) # remove government_tv dataset

# Variable for amount of corona news
news_data <- read_csv("Stock_data/news_corona.csv")[,-1] # Load data 

news_data_formatted <- news_data %>% 
  select(date, pageviews) %>% 
  mutate(nr_articles = n_distinct(date)) %>% 
  group_by(date) %>% 
  summarise(clicks.sum = sum(pageviews), articles.sum = n()) %>% 
  mutate(clicks_article = clicks.sum/articles.sum)

stock_data$news <- news_data_formatted$clicks_article[match(stock_data$date, as.Date(news_data_formatted$date))]

# Create the variable in the main dataframe
stock_data$news[is.na(stock_data$news)] <- 0

stock_data <- stock_data[year(stock_data$date) == 2020,] # Remove days before 2020
#stock_data <- stock_data[!month(stock_data$date) == 1,]
#stock_data <- stock_data[!month(stock_data$date) == 2,]

stock_data <- stock_data %>% ungroup() %>% group_by(month)%>% mutate(ranks=rank(news, ties.method = "first")) %>% 
  mutate(news_Q2 = cut(ranks,
                       breaks = quantile(ranks, seq(0, 1, l = 6), type = 8),
                       labels = c("N1","N2","N3","N4","N5"),
                       include.lowest = TRUE))

ranks <- rank(stock_data$news, ties.method = "first")
stock_data$news_q <- cut(ranks, 
                   breaks = quantile(ranks, seq(0, 1, l = 6), type = 8),
                   labels = c("N1","N2","N3","N4","N5"),
                   include.lowest = TRUE)


#summary(stock_data$news_q) # Look at quantiles
#nrow(stock_data[stock_data$news == 0,]) # Count number of rows with 0 news

rm(news_data, news_data_formatted, ranks)


# Dummy variable for earnings announcments and industy
# Loading in data
# Earnings data
earning_data <- read_csv("Stock_data/earning_data2.csv") %>% 
  select(-c(X1,title))

# Change column names
colnames(earning_data) <- c("Security", "Name", "date")



# EPS data
EPS <- read_excel("Stock_data/bloomber_EPS2.xlsx", sheet = 3) %>% 
  select(c(Security, date, EPS_correct, quarter)) %>% 
  mutate(ES_4 = NA)

EPS$Security <- gsub(" .*$", "", EPS$Security, ignore.case = T)


colnames(EPS)[3] <- "eps"

# EPS last year
for (i in 1:length(EPS$Security)){
  x <- EPS$eps[EPS$Security == EPS$Security[i] & year(EPS$date) == year(EPS$date[i]) - 1 & EPS$quarter == EPS$quarter[i]]
  if (length(x) > 0) {
    EPS$ES_4[i] <- x
  }
}


# Calculate estimated EPS using same quarter last year and a drift tem eaqual to the average change between quarters
EPS <- EPS %>% group_by(Security) %>% 
  mutate(ES_4d = ES_4 + c(NA, cumsum(na.omit(c(NA,diff(eps)))))/(1:n()-1)) %>% 
  filter(date > as.Date("2019-12-30"))

# Combine earnings data with EPS data
earning_data <- merge(earning_data, EPS[,-2] , by = c("Security", "quarter"))

rm(EPS)

# Create dataframe for accounting variables:
# remove some unneccesary variables as well as PX_TO_BOOK as Book to market had more obs.
acc_vars <- read_excel("Stock_data/accounting_vars.xlsx") %>% as.data.frame() %>% 
  select(-c(date1, time, PX_TO_BOOK_RATIO, date2, comparable, period))

acc_vars$Security <- gsub(" .*$", "", acc_vars$Security, ignore.case = T)
acc_vars$date <- as.Date(acc_vars$date)
acc_vars$date5 <- as.Date(as.numeric(acc_vars$date5), origin = "1899-12-30") 


# Combine earnings data with consensus analyst forcast of EPS
temp1 <- acc_vars%>% 
  select(Security, date6, estimated, actual) %>% # add book to market somewhere else
  filter(estimated != "#N/A N/A" & actual != "#N/A N/A" & year(date6) > 2019)

colnames(temp1)[2] <- "date"

earning_data <- right_join(temp1, earning_data[-c(3)], by = c("Security","date") , all = F)

rm(temp1)

# Combine with stock data
stock_data$Security <- gsub(" .*$", "", stock_data$Security, ignore.case = T)

stock_data <- merge(stock_data, earning_data, by = c("Security", "date")) %>% 
  mutate_at(c("actual","estimated"),as.numeric) # need to make values numeric

# Remove SOFF as this company had extreme values
stock_data <- stock_data %>% filter(Security != "SOFF")

# Incorporating accounting variables

# Dates, month, year for each security
temp1 <- acc_vars %>% select(Security, date) %>% 
  mutate(month = month(date),
         year = year(date) )

# Mean number of analysts
temp2 <- acc_vars %>% select(Security, date, date5, TOT_ANALYST_REC) %>% 
  mutate(month = month(date5),year = year(date5) ) %>% 
  group_by(Security, year, month) %>% 
  summarize(mean_analyst = mean(TOT_ANALYST_REC, na.rm = T))

# Mean institutional ownership
temp3 <- acc_vars %>% select(Security, date, date3, EQY_INST_PCT_SH_OUT) %>% 
  mutate(month = month(date3), year = year(date3) ) %>% 
  group_by(Security, year, month) %>% 
  summarize(mean_IO_share = mean(EQY_INST_PCT_SH_OUT))

# Mean market to book and shares outstanding
temp4 <- acc_vars %>% select(Security, date, MARKET_CAPITALIZATION_TO_BV, EQY_SH_OUT) %>% 
  mutate(month = month(date), year = year(date) ) %>% 
  group_by(Security, year, month) %>% 
  summarize(mean_MtoB = mean(MARKET_CAPITALIZATION_TO_BV),
            mean_share = mean(EQY_SH_OUT))

# Merge all temporary dataframes togheter using reduce
temp5 <- Reduce(function(x,y) merge(x = x, y = y, all.x = T), 
                list(temp1, temp2, temp3, temp4)) %>% select(-c(month, year))

# Combine with stock data
stock_data <- merge(temp5, stock_data, by = c("Security", "date"))

rm(temp1, temp2, temp3, temp4, temp5, acc_vars)

stock_data$mean_analyst[is.na(stock_data$mean_analyst)] <- 0

# Share turnover
stock_data$EQY_SH_OUT <- stock_data$mean_share * 10^6 # It was formatted in mill
stock_data$share_turnover <- stock_data$PX_VOLUME / stock_data$mean_share

assign("last.warning", NULL, envir = baseenv()) # removes warning messages


#stock_data$news_q <- stock_data$news_Q2

# Earnings surprise
earnings_calc <- function(EPSactual, EPSestimated, variable) {
  stock_data$ES <<- (EPSactual - EPSestimated)/ifelse(is.na(stock_data$PX_normal),stock_data$PX_LAST,stock_data$PX_normal)
  
  stock_data$ES_quantile <<- cut(stock_data$ES,
                                 breaks = quantile(stock_data$ES, seq(0, 1, l = 6), na.rm = T, type = 8),
                                 labels = c("Q1","Q2","Q3","Q4","Q5"),
                                 include.lowest = TRUE)
  
  stock_data$plot <- variable
  
  # plot
  stock_data %>% select(news_q, ES_quantile, plot) %>%
    filter(news_q == "N1" | news_q == "N5") %>%
    group_by(news_q,ES_quantile) %>%
    summarise(mean = mean(plot, na.rm = T)) %>% ungroup() %>%
    na.omit(ES) %>% 
    
    ggplot(., aes(x = ES_quantile, y = mean, group = news_q)) +
    geom_line(aes(colour = news_q)) + 
    geom_point() +
    labs(title = paste0("Average ", gsub("^.+\\$", "", deparse(substitute(variable))), " for Each Earnings Surprise Quantile"),
         subtitle = paste0("EPS Estimate Model = ", gsub("^.+\\$", "", deparse(substitute(EPSestimated)))),
         x = "ES Quantile", y = gsub("^.+\\$", "", deparse(substitute(variable)))) + # Give right variable name inside plot
    theme_bw()
}


# Analyst Consensus
earnings_calc(stock_data$actual, stock_data$estimated, stock_data$CAR1)
earnings_calc(stock_data$actual, stock_data$estimated, stock_data$CAR40)

earnings_calc(stock_data$actual, stock_data$estimated, stock_data$AV_alt) 

earnings_calc(stock_data$actual, stock_data$estimated, stock_data$VOLATILITY_30D) # need to update later

stock_data$ES[!is.na(stock_data$ES)] %>% length() # Number of observations


# Foster model 1
earnings_calc(stock_data$eps, stock_data$ES_4, stock_data$CAR1)
earnings_calc(stock_data$eps, stock_data$ES_4, stock_data$CAR40)

earnings_calc(stock_data$eps, stock_data$ES_4, stock_data$AV_alt)

earnings_calc(stock_data$eps, stock_data$ES_4, stock_data$VOLATILITY_30D)

stock_data$ES[!is.na(stock_data$ES)] %>% length() # Number of observations


# Foster model 2
earnings_calc(stock_data$eps, stock_data$ES_4d, stock_data$CAR1)
earnings_calc(stock_data$eps, stock_data$ES_4d, stock_data$CAR40)

earnings_calc(stock_data$eps, stock_data$ES_4d, stock_data$AV_alt)

summary(stock_data)

earnings_calc(stock_data$eps, stock_data$ES_4d, stock_data$VOLATILITY_30D)

stock_data$ES[!is.na(stock_data$ES)] %>% length() # Number of observations





# Regression analysis -----------------------------------------------------


df <- stock_data %>% 
  select(news_q, ES, CUR_MKT_CAP, MARKET_CAPITALIZATION_TO_BV, mean_analyst, mean_IO_share, share_turnover) %>% 
  group_by(news_q) %>% 
  summarize(ES = mean(ES),
            mkt_cap = mean(CUR_MKT_CAP, na.rm = T)/1000,
            M_B = mean(MARKET_CAPITALIZATION_TO_BV, na.rm = T),
            analyst = mean(mean_analyst),
            IO_share = mean(mean_IO_share),
            share_turnover = mean(share_turnover, na.rm = T)) 

# for some reason the security SOFF has an earnings surprise of 514. There has to be some kind of dataerror here.
# the real value of EPS from their report is -3,5 no -1001. 

# need a value of share turnover in a given period leading up to the earnings announcement

# Run linear regression on only the top category
# Logit model for only top news group, need dummy variable for this 
stock_data$TOP_N <- ifelse(stock_data$news_q == "N5", 1, 0)

glm.fits=glm(TOP_N ~ ES+ CUR_MKT_CAP + MARKET_CAPITALIZATION_TO_BV + mean_analyst + mean_IO_share,
             data=stock_data,family=binomial)

summary(glm.fits)
# No variables are correlated with wheter they publish on a high news day or not, which makes sense


# Multinominal Logistic regression since dependent variable is a categorical variable.
lm.fit=lm(news_q ~ ES+ CUR_MKT_CAP + MARKET_CAPITALIZATION_TO_BV + mean_analyst + mean_IO_share, data=stock_data)
summary(lm.fit)


df <- stock_data %>% 
  select(news_q, ES, ES_quantile, CAR1, CAR40) %>% 
  group_by(news_q) %>% 
  summarize(ES = mean(ES)*100,
            car1 = mean(CAR1)*100,
            car40 = mean(CAR40, na.rm = T)*100)

# CAR = FE10 +NRANK10 + FE10*NRANK10

lm.fit <- stock_data %>% 
  select(news_q, ES, ES_quantile, CAR1, CAR40) %>% 
  mutate(N5 = ifelse(stock_data$news_q == "N5", 1, 0),
         Q5 = ifelse(stock_data$ES_quantile == "Q5", 1, 0)) %>% 
  lm(CAR1 ~ Q5 + N5 + Q5*N5, data=.)

summary(lm.fit)

# Robust standard errors
library(lmtest)
library(sandwich)

coeftest(lm.fit, df = Inf, vcov = vcovHC(lm.fit, type = "HC0"))



# Maybe it is easier to only focus on the top news group, and use similar method to DV
# Make quantile based on wheter they are positive or negative?
# - top 3 is positive, bottom 3 negative etc..




# Simple regression 
# Load news data 
news_data <- read_csv("Stock_data/news_data.csv")[,-1]
news_data_corona <- read_csv("Stock_data/news_data.csv")[,-1]
news_data$corona <- ifelse(grepl("Covid-19|korona", news_data$subject, ignore.case = T) == T, 1, 0)

news_corona <- news_data[grepl("Covid-19|korona", news_data$subject, ignore.case = T), ]


grepl("Covid-19|korona", news_data$subject[43], ignore.case = T) == T
sum()

# multipple regression using news and gov as explanatory variables. Alsp includes control variables
lm.fit=lm(AV_ODEEN~gov+ news + day + month + mean_share + mean_analyst + CUR_MKT_CAP + industry ,data=stock_data)

summary(lm.fit)
#   - Not any intresting results, need to adjust for heteroskedasticity.
#   - Stadardize all variables by substracting the mean and dividing by the standard deviation for all variables?
#   - Univariate vs multivariate?
#   - Compare CAR40 and CAR1 (post earnings announcement drift) to the most extreme and least extreme.



summary(lm(news~day+month+gov, data=stock_data)) 
#   - news is a dummy-variable with more than two levels.
#   - this is similar to that of hirshleifer that tests if rank (10 levels) is affected by control variables.
#   - maybe convert readership to a continius variable instead?
#   - look further into this later.


library(lmtest)

waldtest(lm.fit) # can test for heteroskedacity?

# comparing two groups




# Various plots -------------------------------------------------------------------

# Graph of trading volume over time
# Differentiating between company size.

grouped_df <- stock_data %>% 
  group_by(Security) %>%
  summarize(avg_mkt_cap = mean(CUR_MKT_CAP, na.rm = T)) %>%
  remove_missing() %>% 
  mutate(Size_Level = cut(avg_mkt_cap,
                          quantile(avg_mkt_cap, c(0, .50, .75, 1)),
                          labels = c('Small', 'Medium', 'Big'),
                          include.lowest = TRUE)) %>% 
  merge(.,stock_data, by = "Security")  # idk if this is the most efficient way..

# Plot by each category.
# Intrestingly we see that it is the medium sized companies that is deriving most of the volume
grouped_df %>% 
  group_by(date, Size_Level) %>% 
  summarize(total_volume = sum(PX_VOLUME)) %>%
  ggplot(., aes(x=date, y=total_volume/10^6, fill=Size_Level)) +
  geom_area()+
  geom_area(alpha=0.6 , size=.2, colour="black") +
  ggtitle("Total trading volume for different market caps") +
  labs(y = "Million trades") +
  theme_bw()




stock_data %>% 
  select(date, AV_ODEEN) %>% 
  group_by(date) %>% 
  summarise(av.mean = mean(AV_ODEEN, na.rm = T)) %>%
  na.omit %>% 
  
  ggplot(., aes(x = as.Date(date), y = av.mean)) +
  geom_line(color = "blue") +
  labs(title = "Average Abnormal Volum",
       subtitle = "October 2019 - September 2020",
       x = "Date", y = ".") +
  scale_x_date(date_labels = "%d %b %Y",date_breaks  ="1 month") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

# Total abnormal volume by day
stock_data %>% 
  select(date, AV_ODEEN) %>% 
  group_by(date) %>% 
  summarise(av.sum = sum(AV_ODEEN, na.rm = T)) %>%
  na.omit %>% 
  
  ggplot(., aes(x = as.Date(date), y = av.sum)) +
  geom_line(color = "blue") +
  labs(title = "Sum Abnormal Volum",
       subtitle = "October 2019 - September 2020",
       x = "Date", y = ".") +
  scale_x_date(date_labels = "%d %b %Y",date_breaks  ="1 month") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))



# Old Earnings
# Remove duplicated dates with about 40 days in between
earning_data <- earning_data %>% group_by(Security) %>% arrange(date, .by_group = TRUE) %>%  
  mutate(DUP = ifelse(abs(difftime(date, lead(date))) <= 40,1,0)) %>% 
  subset(DUP %in%  0| DUP %in%  NA) %>% select(-DUP)

# remove shitty observations
earning_data <-earning_data[!(earning_data$Security == "LSG" & earning_data$date == as.Date("2020-01-08")),]
earning_data <-earning_data[!(earning_data$Security == "SBVG" & earning_data$date == as.Date("2020-03-24")),]


# Make a column for quarters
Y2019_Q3 <- interval(ymd("2019-10-01"), ymd("2019-12-30"))
Y2019_Q4 <- interval(ymd("2020-01-01"), ymd("2020-03-31"))
Y2020_Q1 <- interval(ymd("2020-04-01"), ymd("2020-06-30"))
Y2020_Q2 <- interval(ymd("2020-07-01"), ymd("2020-09-30"))

earning_data$quarter[earning_data$date %within% Y2019_Q3] <- "Q3"
earning_data$quarter[earning_data$date %within% Y2019_Q4] <- "Q4"
earning_data$quarter[earning_data$date %within% Y2020_Q1] <- "Q1"
earning_data$quarter[earning_data$date %within% Y2020_Q2] <- "Q2"

rm(Y2019_Q3, Y2019_Q4, Y2020_Q1, Y2020_Q2)

