# Libraries
library(readxl)
library(tidyverse)
library(zoo)
library(lubridate)
library(DescTools)
library(sandwich)
library(lmtest)
library(robustHD)
library(stargazer)

# Load and format stock data ----------------------------------------------------
stock_data <- read_excel("Stock_data/stock_final2.xlsx") %>% as.data.frame()

# Fix problem with mismatching dates
stock_data <- 
  merge(stock_data[,-2], stock_data[,1:2], by=1:2, all.y = T) %>% 
  select(-c(VOLATILITY_30D))

colnames(stock_data)[2] <- "date"

stock_data$date <- as.Date(stock_data$date, format="%d.%m.%Y")

# Change structure of variables from character to numeric
stock_data[,3:8] <- sapply(stock_data[,3:8],as.numeric)

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

# Fix instances with missing high and low. Make them equal to px_last since these are days with
stock_data <- stock_data %>% mutate(PX_LOW = ifelse(is.na(PX_LOW), PX_LAST, PX_LOW),
                                    PX_HIGH = ifelse(is.na(PX_HIGH), PX_LAST, PX_HIGH))

# Create a column with industry specifications
Ticker_list <- read_excel("Peer_companies/Ticker-list.xlsx") # list of companies with industry specification

colnames(Ticker_list)[5] <- "Security" 

stock_data <- merge(stock_data, Ticker_list, by = "Security") %>%  # merge with stock data
  select(-c(ticker, name, industri))

# Remove unnecessary variables
rm(Ticker_list, i, j, x, z)

# Mean trading volume each month
stock_data <- stock_data %>% 
  mutate(month = month(date), year = year(date)) %>% 
  group_by(Security, year, month) %>%
  mutate(mean_volume = mean(PX_VOLUME))

# Mean volatility in january
stock_data <- left_join(stock_data, (stock_data %>% filter(month == 1) %>% ungroup %>% 
                                       mutate(daily_return = log(PX_LAST/PX_OPEN)) %>% group_by(Security) %>% 
                                       mutate(Vol_jan = mean(daily_return^2))))

stock_data <- stock_data %>% group_by(Security) %>% # not working?
  mutate(Vol_jan = last(na.omit(Vol_jan)))

# Get shares outstanding
acc_vars <- read_excel("Stock_data/accounting_vars.xlsx") %>% as.data.frame() %>% 
  select(-c(date1, time, PX_TO_BOOK_RATIO, date2, comparable, period))

temp4 <- acc_vars %>% select(Security, date4, EQY_SH_OUT) %>% 
  rename(date = date4) %>% 
  mutate(month = month(date), year = year(date)) %>% 
  group_by(Security, year, month) %>% 
  summarize(mean_share = mean(EQY_SH_OUT)) %>% 
  drop_na()

stock_data <- merge(stock_data, temp4, by = c("Security", "year", "month"), all.x = T) %>% 
  arrange(.,Security, date)
rm(acc_vars, temp4)

stock_data$mean_share[is.na(stock_data$mean_share)] <- round(stock_data$CUR_MKT_CAP[is.na(stock_data$mean_share)] / stock_data$PX_LAST[is.na(stock_data$mean_share)], 3)

for (i in unique(stock_data$Security)) { # Fill in missing values
  stock_data$mean_share[stock_data$Security == i] <- na.locf(stock_data$mean_share[stock_data$Security == i], na.rm = FALSE)}

stock_data$CUR_MKT_CAP[is.na(stock_data$CUR_MKT_CAP)] <- round(stock_data$PX_LAST[is.na(stock_data$CUR_MKT_CAP)] * stock_data$mean_share[is.na(stock_data$CUR_MKT_CAP)], 3)



# Cumulative abnormal volume and return ----------------------------------------------
stock_data <- stock_data %>% 
  group_by(industry, date) %>%
  mutate(T_MC = sum(CUR_MKT_CAP, na.rm = T) - CUR_MKT_CAP, # total market cap for each day in each industy minus current company
         
         capH = sum(PX_HIGH*mean_share, na.rm = T)-PX_HIGH*mean_share,
         capL = sum(PX_LOW*mean_share, na.rm = T)-PX_LOW*mean_share,
         capO = sum(PX_OPEN*mean_share, na.rm = T)-PX_OPEN*mean_share,
         capC = sum(PX_LAST*mean_share, na.rm = T)-PX_LAST*mean_share,
         
         val = PX_VOLUME*PX_LAST,                                 # value traded each day for each company
         total_val = sum(val, na.rm = T)-val) %>%                 # total value traded each day in each industry minus current company
  ungroup %>%
  arrange(., Security) %>% 
  group_by(Security) %>% 
  mutate(AV_DV30 = (log(val+1) - (rollsumr(log(lag(val+1, n = 11)), k = 30, fill = NA)/30)),   # Abnormal volume for each company
         AV_DV20 = (log(val+1) - (rollsumr(log(lag(val+1, n = 11)), k = 20, fill = NA)/20)),   # Abnormal volume for each company
         AV20_m = c(as.numeric(rollapply(log(lead(val, 2) + 1), 50, mean, partial = FALSE, align = "right")), rep(NA, times = 49)), # Average of AV day 2 to 21
         
         AV20 = AV20_m - log((mean(val[month(date) == 1], na.rm = T)) + 1), # Normalized Volume day 2 to 21
         AV1_DV = log(val + 1) - log(val[date == "2020-01-31"] + 1),
         
         RN = Winsorize((log(PX_LAST/lag(PX_LAST))), minval = NULL, maxval = NULL, probs = c(0.05, 0.95), na.rm = T, type = 7),
         
         #vol1 = RN^2, # Winsorized 
         vol1 = log(PX_LAST/lag(PX_LAST))^2, # Not winsorized 
         vol2 = ((log(PX_HIGH)-log(PX_LOW))^2)/(4*log(2)),
         vol3 = 0.5*log(PX_HIGH/PX_LOW)^2 - (2*log(2)-1)*log(PX_LAST/PX_OPEN)^2,
         
         vol1A = vol1 - (rollsumr((lag(vol1, n = 11)), k = 20, fill = NA)/20),
         vol2A = vol2 - (rollsumr((lag(vol2, n = 11)), k = 20, fill = NA)/20),
         vol3A = vol3 - (rollsumr((lag(vol3, n = 11)), k = 20, fill = NA)/20),
         
         vol1MR = vol1 - log(capC/lag(capC))^2,
         vol2MR = vol2 - ((log(capH)-log(capL))^2)/(4*log(2)),
         vol3MR = vol3 - 0.5*log(capH/capL)^2 - (2*log(2)-1)*log(capC/capO)^2,
         
         
         PX_5 = lag(PX_LAST, n=5),                                                         # Price five days ago, needed later
         MR = c(NA,diff(T_MC))/lag(T_MC, 1),                                               # Market return, diff takes the difference between last observation and current
         
         #daily_return = log(PX_LAST/PX_OPEN),                                              # Daily return, might change to log returns instead
         daily_return = Winsorize((log(PX_LAST/lag(PX_LAST))), minval = NULL, maxval = NULL, probs = c(0.05, 0.95),
                                  na.rm = T, type = 7),
         
         CAR1 = c(as.numeric(rollapply(1 + daily_return, 2, prod, partial = FALSE, align = "right")), rep(NA, times = 1)) # Cumulative abnormal return (CAR) for each company in each industry first 2 days
         -c(as.numeric(rollapply(1 + MR, 2, prod, partial = FALSE, align = "right")), rep(NA, times = 1)),
         
         CAR2 = c(as.numeric(rollapply(1 + daily_return, 3, prod, partial = FALSE, align = "right")), rep(NA, times = 2)) # Cumulative abnormal return (CAR) for each company in each industry first 2 days
         -c(as.numeric(rollapply(1 + MR, 3, prod, partial = FALSE, align = "right")), rep(NA, times = 2)),
         
         CAR40 = c(as.numeric(rollapply(1 + lead(daily_return, 3), 40, prod, partial = FALSE, align = "right")), rep(NA, times = 39)) # Cumulative abnormal return (CAR) for each company in each industry first 40 days
         -c(as.numeric(rollapply(1 + lead(MR, 3), 40, prod, partial = FALSE, align = "right")), rep(NA, times = 39))) %>% 
  
  
  group_by(industry,date) %>%
  mutate(AV_industy30 = ((sum(AV_DV30, na.rm = T, fill = NA)-AV_DV30)/(n()-ifelse(n() == sum(is.na(AV_DV30)),1,0)-sum(is.na(AV_DV30)))), # Mean abnormal volume for industry
         AV_industy20 = ((sum(AV_DV20, na.rm = T, fill = NA)-AV_DV20)/(n()-ifelse(n() == sum(is.na(AV_DV20)),1,0)-sum(is.na(AV_DV20)))),  # Mean abnormal volume for industry 
         AV20_industy = ((sum(AV20, na.rm = T, fill = NA) - AV20)/(n() - ifelse(n() == sum(is.na(AV20)), 1, 0) - sum(is.na(AV20)))),  # Average Normalized Volume in each industry day 2 to 21
         
         AVol_industy1 = ((sum(vol1A, na.rm = T, fill = NA)-vol1A)/(n()-ifelse(n() == sum(is.na(vol1A)),1,0)-sum(is.na(vol1A)))),
         AVol_industy2 = ((sum(vol2A, na.rm = T, fill = NA)-vol2A)/(n()-ifelse(n() == sum(is.na(vol2A)),1,0)-sum(is.na(vol2A)))),
         AVol_industy3 = ((sum(vol3A, na.rm = T, fill = NA)-vol3A)/(n()-ifelse(n() == sum(is.na(vol3A)),1,0)-sum(is.na(vol3A))))) %>%
  
  group_by(Security) %>% 
  mutate(AV_alt30 = AV_DV30 - AV_industy30,                     # Abnormal volume minus mean industy abnormal volume
         AV_alt30lag = (AV_alt30 + lead(AV_alt30))/2,           # Avolume for regression
         AV_alt20 = AV_DV20 - AV_industy20, 
         AV_alt20lag = (AV_alt20 + lead(AV_alt20))/2,
         AV1_alt = AV1_DV - AV_industy20,
         AV20_reg = AV20 - AV20_industy,
         
         AVol_alt1 = vol1A - AVol_industy1,
         AVol_alt2 = vol2A - AVol_industy2,
         AVol_alt3 = vol3A - AVol_industy3,
         AVol_reg1 = (AVol_alt1 + lead(AVol_alt1))/2,           # Avolatility for regression
         AVol_reg2 = (AVol_alt2 + lead(AVol_alt2))/2,
         AVol_reg3 = (AVol_alt3 + lead(AVol_alt3))/2) %>% 
  select(-c(T_MC, val, total_val, AV_industy30,AV_industy20, vol1A, vol2A, vol3A, AVol_industy1, AVol_industy2, AVol_industy3, capH, capL, capO, capC))

#library(moments)
#skewness(stock_data$abn_volatility1, na.rm = T) # abn_volatility1 is super skewed, as it should be


# Creating control variables -------------------------------------------

# Price before corona
# Some companies listed in 2020 will not have this value. Add an ifelse statement and use PX-5
for (i in unique(stock_data$Security)) {
  stock_data$PX_normal[stock_data$Security == i] <- stock_data[stock_data$date == as.Date("2020-01-31") & stock_data$Security == i,]$PX_LAST
}

stock_data <- stock_data %>% group_by(Security) %>% mutate(PX_normal = ifelse(is.na(PX_normal),PX_5,PX_normal)) %>% 
  select(-c(PX_5))

# Varaible for month & day
stock_data$month <- format(stock_data$date,"%B")
stock_data$day <- format(stock_data$date,"%A")


# Variable for amount of corona news
news_data <- read_csv("Stock_data/news_corona.csv", locale = readr::locale(encoding = "latin1"))[,-1] # Load data 

news_data_formatted <- news_data %>% 
  select(date, pageviews) %>% 
  mutate(nr_articles = n_distinct(date)) %>% 
  group_by(date) %>% 
  summarise(clicks.sum = sum(pageviews), articles.sum = n()) %>% 
  mutate(clicks_article = clicks.sum/articles.sum)

stock_data$news <- news_data_formatted$clicks_article[match(stock_data$date, as.Date(news_data_formatted$date))]
stock_data$lognews <- log(stock_data$news)

# Create the variable in the main dataframe
stock_data$news[is.na(stock_data$news)] <- 0
stock_data$lognews[is.na(stock_data$lognews)] <- 0

stock_data <- stock_data[year(stock_data$date) == 2020,] # Remove days before 2020
stock_data <- stock_data[!month(stock_data$date) == 1,] # Remove days in january
stock_data <- stock_data[!is.na(stock_data$PX_LAST),] # Remove all observations with no stock price (some companies release reports before they are public)



# Divide into groups grouped by month
stock_data <- stock_data %>% ungroup() %>% group_by(month)%>% mutate(ranks=rank(news, ties.method = "first")) %>% 
  mutate(news_month = cut(ranks,
                          breaks = quantile(ranks, seq(0, 1, l = 6), type = 7),
                          labels = c("N1","N2","N3","N4","N5"),
                          include.lowest = TRUE))

stock_data <- stock_data %>% ungroup() %>% group_by(month)%>% mutate(ranks=rank(news, ties.method = "first")) %>% 
  mutate(news_month4 = cut(ranks,
                           breaks = quantile(ranks, seq(0, 1, l = 5), type = 7),
                           labels = c("N1","N2","N3", "N4"),
                           include.lowest = TRUE))

stock_data <- stock_data %>% ungroup() %>% group_by(month)%>% mutate(ranks=rank(news, ties.method = "first")) %>% 
  mutate(news_month3 = cut(ranks,
                           breaks = quantile(ranks, seq(0, 1, l = 4), type = 7),
                           labels = c("N1","N2","N3"),
                           include.lowest = TRUE))

#summary(stock_data$news_q) # Look at quantiles
#nrow(stock_data[stock_data$news == 0,]) # Count number of rows with 0 news

rm(news_data, news_data_formatted, ranks,i)


# Dummy variable for earnings announcments and industry

# Earnings data
earning_data <- read_csv("Stock_data/earning_data2.csv") %>% 
  select(-c(X1,title))

# Change column names
colnames(earning_data) <- c("Security", "Name", "date", "quarter")

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

## TEST ##
#test <- stock_data %>% filter(PX_VOLUME != 0)

#_VOLATILITY / ABNORMAL VOLUME - PLOT _______________________________________________________________________________________

plot_data <- stock_data %>% select(c(Security, date, AV_alt30, AV_alt30lag, AV_alt20, AV_alt20lag, AV1_alt, AVol_alt1, AVol_alt2, AVol_alt3, news_month, news_month3, news_month4, daily_return, MR, news)) %>% 
  drop_na()

plot_data$Security <- gsub(" .*$", "", plot_data$Security, ignore.case = T)

plot_data$news_q <- plot_data$news_month4

plot_data <- left_join(plot_data, earning_data) %>% select(-c("Name")) %>% ungroup() %>% 
  mutate(time = NA)

# Calculate ES quantiles
plot_data$ES_q[!is.na(plot_data$ES_4d)] <- cut(plot_data$ES_4d[!is.na(plot_data$ES_4d)],
                                               breaks = quantile(plot_data$ES_4d[!is.na(plot_data$ES_4d)], seq(0, 1, l = 6), na.rm = T, type = 7),
                                               labels = c("Q1","Q2","Q3","Q4","Q5"),
                                               include.lowest = TRUE)

for (i in 1:nrow(plot_data)) {
  if (is.na(plot_data$quarter[i]) == F) {
    for (j in -2:5) {
      plot_data$time[i + j] <- j
      plot_data$news_q[i + j] <- plot_data$news_q[i]
      plot_data$ES_q[i + j] <- plot_data$ES_q[i]
    }
  }
}

rm(i, j)

plot_data <- plot_data %>% drop_na(time)

# Plot of abnormal volume from -2 to +10 surrounding earnings announcement
plot_data %>% filter(news_q %in% c("N1", "N4")) %>% 
  mutate(labels = ifelse(news_q == "N1", "Corona Bottom", "Corona Top")) %>% 
  group_by(labels, time) %>% 
  summarize(m_AV = mean(AV_alt20)) %>%  
  ggplot(.,aes(fill = labels, x = time, y = m_AV * 100, linetype = labels)) +
  geom_line(aes(colour = labels)) +
  scale_color_manual("News", values = c("Corona Bottom" = "#4EBCD5", "Corona Top" = "#1C237E")) +
  scale_linetype_manual(name = "News", values = c("solid", "longdash")) +
  labs(title = "Mean Abnormal Volume Around Earnings Announcement",
       x = "Days from Announcement", y = "Mean Abnormal Volume (%)") +
  scale_x_continuous(n.breaks = 11) +
  theme_bw() +
  theme(text = element_text(family = "serif"),
        #legend.position="top",
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())

# Volatility
plot_data %>% filter(news_q %in% c("N1", "N4")) %>% 
  mutate(labels = ifelse(news_q == "N1", "Corona Bottom", "Corona Top")) %>% 
  group_by(labels, time) %>% 
  summarize(m_AVol = mean(AVol_alt1)) %>% 
  ggplot(.,aes(x = time, y = m_AVol * 100, linetype = labels, fill = labels,  shape = labels)) +
  geom_line(aes(colour = labels)) +
  scale_color_manual("News", values = c("Corona Bottom" = "#4EBCD5", "Corona Top" = "#1C237E")) +
  scale_linetype_manual(name = "News", values = c("solid", "longdash")) +
  labs(title = "Mean Abnormal Volatility Around Earnings Announcement",
       x = "Days from Announcement", y = "Mean abnormal volatility (%)") +
  scale_x_continuous(n.breaks = 11) +
  theme_bw() +
  theme(text = element_text(family = "serif"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())

mean(plot_data$news[plot_data$news_month4 == "N1"])

# CAR over time
plot_data$CAR_start <- NA

for (i in 1:nrow(plot_data)) {
  if (plot_data$time[i] == min(plot_data$time)) {
    for (j in 1:(max(plot_data$time - min(plot_data$time) + 1))) {
      plot_data$CAR_start[i+j-1] <- prod(1 + plot_data$daily_return[i:(i+j-1)]) - prod(1 + plot_data$MR[i:(i+j-1)])
    }
  }
}


plot_data %>% group_by(news_q, time) %>% filter(news_q %in% c("N1", "N4")) %>% 
  #filter(!month == "3") %>% # Without March
  #filter(ES_q == "1") %>% # Only a spesific ES
  summarize(m_CAR = mean(CAR_start)) %>% 
  ggplot(.,aes(color = news_q, x = time, y = m_CAR, linetype = news_q, shape = news_q)) +
  geom_line() +
  geom_point() +
  scale_color_manual("News Q", values = c("N1" = "#4EBCD5", "N4" = "#1C237E")) +
  scale_linetype_manual(name = "News Q", values = c("solid", "longdash")) +
  scale_shape_manual(name = "News Q", values = c(15, 17)) +
  geom_vline(xintercept = 0, linetype = 4, colour = "black") + 
  labs(title = "Figure 5: Mean Cumulative Abnormal Return Around Earnings Announcement",
       x = "Days from Announcement", y = "Mean Cumulative Abnormal Return") +
  scale_x_continuous(n.breaks = 15) +
  theme_bw() +
  theme(text = element_text(family = "serif"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())


#________________________________________________________________________________________________________

# Remove observations with too little avalible data
# Just got listed
earning_data <- earning_data[!(earning_data$Security == "PEXIP" & earning_data$date == "2020-05-27"),]
earning_data <- earning_data[!(earning_data$Security == "NOL" & earning_data$date == "2020-02-28"),]

# No info of inst. ownership
earning_data <- earning_data[!(earning_data$Security == "EPICME" & earning_data$date == "2020-04-16"),]
earning_data <- earning_data[!(earning_data$Security == "EPICME" & earning_data$date == "2020-08-13"),]
earning_data <- earning_data[!(earning_data$Security == "RIVERME" & earning_data$date == "2020-04-30"),]
earning_data <- earning_data[!(earning_data$Security == "RIVERME" & earning_data$date == "2020-08-31"),]

# Remove SOFF as this company had extreme values
earning_data <- earning_data %>% filter(Security != "SOFF")



# Create dataframe for accounting variables:
# remove some unneccesary variables as well as PX_TO_BOOK as Book to market had more obs.
acc_vars <- read_excel("Stock_data/accounting_vars.xlsx") %>% as.data.frame() %>% 
  select(-c(time, PX_TO_BOOK_RATIO, date2, comparable, period))

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




# Incorporating accounting variables

# Dates, month, year for each security
temp1 <- acc_vars %>% select(Security, date) %>% 
  mutate(month = month(date),
         year = year(date) )

# Mean number of analysts
temp2 <- acc_vars %>% select(Security, date, date5, TOT_ANALYST_REC) %>% 
  mutate(month = month(date5),year = year(date5) ) %>% 
  group_by(Security, year, month) %>% 
  summarize(mean_analyst = mean(log(1 + TOT_ANALYST_REC), na.rm = T))

# Mean institutional ownership
temp3 <- acc_vars %>% select(Security, date, date3, EQY_INST_PCT_SH_OUT) %>% 
  mutate(month = month(date3), year = year(date3) ) %>% 
  group_by(Security, year, month) %>% 
  summarize(mean_IO_share = mean(EQY_INST_PCT_SH_OUT))

# Mean market to book and shares outstanding
temp4 <- acc_vars %>% select(Security, date, MARKET_CAPITALIZATION_TO_BV) %>% 
  mutate(month = month(date), year = year(date) ) %>% 
  group_by(Security, year, month) %>% 
  summarize(mean_MtoB = mean(MARKET_CAPITALIZATION_TO_BV, na.rm = T))

for (i in unique(temp4$Security)) { # Fill in missing values
  temp4$mean_MtoB[temp4$Security == i] <- na.locf(temp4$mean_MtoB[temp4$Security == i], na.rm = FALSE)
}

# Merge all temporary dataframes togheter using reduce
temp5 <- Reduce(function(x,y) merge(x = x, y = y, all.x = T), 
                list(temp1, temp2, temp3, temp4)) %>% select(-c(month, year))

# Combine with stock data
stock_data <- merge(temp5, stock_data, by = c("Security", "date"))

rm(temp1, temp2, temp3, temp4, temp5, acc_vars)

stock_data$mean_analyst[is.na(stock_data$mean_analyst)] <- 0

# Make B/M istead of M/B and MarketCap. Take log of both
# Fix IO share to make sure no values is greater than 0.
stock_data <- stock_data %>% mutate(
  logMktCap = log(CUR_MKT_CAP),
  BtoM = (1/mean_MtoB),
  logBtoM = log(BtoM),
  logBtoM2 = log(BtoM - min(BtoM, na.rm = T)+1),
  mean_IO_share = ifelse(mean_IO_share>100,100,mean_IO_share))


# Share turnover
stock_data$share_turnover1 <- stock_data$PX_VOLUME / (stock_data$mean_share * 10^6)
stock_data$share_turnover30 <- stock_data$mean_volume / (stock_data$mean_share * 10^6)

assign("last.warning", NULL, envir = baseenv()) # removes warning messages


# Earnings surprise
earnings_calc <- function(EPSactual, EPSestimated, variable, news) {
  stock_data$ES <<- (EPSactual - EPSestimated)/ifelse(is.na(stock_data$PX_normal),stock_data$PX_LAST,stock_data$PX_normal)
  
  stock_data$ES_quantile <<- cut(stock_data$ES,
                                 breaks = quantile(stock_data$ES, seq(0, 1, l = 6), na.rm = T, type = 7),
                                 labels = c("Q1","Q2","Q3","Q4","Q5"),
                                 include.lowest = TRUE)
  
  stock_data$plot <- variable
  stock_data$news_q <- news
  
  # plot
  stock_data %>% select(news_q, ES_quantile, plot) %>%
    filter(news_q == "N1" | news_q == "N4") %>%
    group_by(news_q, ES_quantile) %>%
    summarise(mean = mean(plot, na.rm = T)) %>% ungroup() %>%
    na.omit(ES) %>% 
    
    ggplot(., aes(x = ES_quantile, y = mean, group = news_q, linetype = news_q, colour = news_q, shape = news_q)) +
    geom_line() + 
    geom_point() +
    scale_color_manual(name = "News Q", values = c("#4EBCD5", "#1C237E")) +
    scale_linetype_manual(name = "News Q", values = c("solid", "longdash")) +
    scale_shape_manual(name = "News Q", values = c(15, 17)) +
    labs(title = paste0("Average ", gsub("^.+\\$", "", deparse(substitute(variable))), " for Each Earnings Surprise Quantile"),
         subtitle = paste0("EPS Estimate Model = ", gsub("^.+\\$", "", deparse(substitute(EPSestimated)))),
         x = "Earnings Surprise Quantile", y = gsub("^.+\\$", "", deparse(substitute(variable)))) + # Give right variable name inside plot
    theme_bw() +
    theme(text = element_text(family = "serif"),
          panel.grid.minor.y = element_blank())
  
}


# Analyst Consensus
earnings_calc(stock_data$actual, stock_data$estimated, stock_data$CAR1, stock_data$news_month)
stock_data$ES[!is.na(stock_data$ES)] %>% length() # Number of observations
# Lack of observations

# Foster model 1
earnings_calc(stock_data$eps, stock_data$ES_4, stock_data$CAR1, stock_data$news_month)
stock_data$ES[!is.na(stock_data$ES)] %>% length() # Number of observations
# Simpler model than next one

# Foster model 2
earnings_calc(stock_data$eps, stock_data$ES_4d, stock_data$CAR1, stock_data$news_month4)
earnings_calc(stock_data$eps, stock_data$ES_4d, stock_data$CAR2, stock_data$news_month)
earnings_calc(stock_data$eps, stock_data$ES_4d, stock_data$CAR40, stock_data$news_month)

stock_data$ES[!is.na(stock_data$ES)] %>% length() # Number of observations




# Regression analysis -----------------------------------------------------


# CAR[0,1] ----------------------------------------------------------------
stock_data$news2 <- stock_data$news/1000
stock_data$CAR2_2 <- stock_data$CAR1*100
stock_data$CAR1_2 <- stock_data$CAR1*100

# See if results differ using numeric variables
stock_data$ES_quantile <- as.numeric(stock_data$ES_quantile)

# Reg1 : CAR2 = news + ES_quantiles
lm.fit <- lm(CAR1_2 ~ news2*ES_quantile, data = stock_data)
coeftest(lm.fit, vcov = vcovHC(lm.fit, type = "HC1"))

# Reg2 : CAR2 = news*ES_quantiles + control variables*ES_quantile
lm.fit <- lm(CAR1_2 ~ news2*ES_quantile + month + ES_quantile*(logMktCap + logBtoM + mean_IO_share + mean_analyst + share_turnover30), data = stock_data)
coeftest(lm.fit, vcov = vcovHC(lm.fit, type = "HC1"))

# Reg3: CAR2 = news + ES_TOP + control variables
lm.fit <- stock_data %>% filter(ES_quantile == 1 | ES_quantile == 5) %>% 
  lm(CAR1_2 ~ I(ES_quantile)*news2, data = .)
coeftest(lm.fit, df = Inf, vcov = vcovHC(lm.fit, type = "HC1"))

# Reg4: CAR2 = news + ES_TOP + control variables
stock_data %>% filter(ES_quantile == 1 | ES_quantile == 5) %>% 
  lm(CAR1_2 ~ I(ES_quantile)*news2 + month + I(ES_quantile)*(logBtoM + mean_IO_share + mean_analyst + share_turnover30), data = .) -> lm.fit
coeftest(lm.fit, df = Inf, vcov = vcovHC(lm.fit, type = "HC1"))


# CAR[2,40] ---------------------------------------------------------------
stock_data$news2 <- stock_data$news/1000
stock_data$CAR40_2 <- stock_data$CAR40*100

# Reg 1:
lm.fit <- lm(CAR40_2 ~ news2*ES_quantile, data = stock_data)
summary(lm.fit)
coeftest(lm.fit, df = Inf, vcov = vcovHC(lm.fit, type = "HC1"))

# Reg 2:
lm.fit <- lm(CAR40_2 ~ news2*ES_quantile + month + ES_quantile*(logMktCap + logBtoM + mean_IO_share + mean_analyst + share_turnover30), data = stock_data)
summary(lm.fit)
coeftest(lm.fit, df = Inf, vcov = vcovHC(lm.fit, type = "HC1"))

# Reg 3:
lm.fit <- stock_data %>% filter(ES_quantile == 1 | ES_quantile == 5) %>% 
  lm(CAR40_2 ~ news2*I(ES_quantile), data = .)
summary(lm.fit)
coeftest(lm.fit, df = Inf, vcov = vcovHC(lm.fit, type = "HC1"))

# Reg 4
lm.fit <- stock_data %>% filter(ES_quantile == 1 | ES_quantile == 5) %>% 
  lm(CAR40_2 ~ news2*I(ES_quantile) + month + I(ES_quantile)*(logBtoM + mean_IO_share + mean_analyst + share_turnover30), data = .)
coeftest(lm.fit, df = Inf, vcov = vcovHC(lm.fit, type = "HC1"))


# Abnormal volume ----------------------------------------------------------------
stock_data$ES_abs = abs(stock_data$ES)
stock_data$ES_quantile2 <- cut(stock_data$ES_abs,
                               breaks = quantile(stock_data$ES_abs, seq(0, 1, l = 6), na.rm = T, type = 8),
                               labels = c("Q1","Q2","Q3","Q4","Q5"),
                               include.lowest = TRUE)
stock_data$ES_quantile2 <- as.numeric(stock_data$ES_quantile2)
stock_data$news_q <- stock_data$news_month4

stock_data  %>%
  filter(news_q == "N1" | news_q == "N4") %>%
  group_by(news_q,ES_quantile2) %>%
  summarise(mean = mean(AV_alt20, na.rm = T)) %>% ungroup() %>%
  na.omit(ES) %>% 
  ggplot(., aes(x = ES_quantile2, y = mean, group = news_q)) +
  geom_line(aes(colour = news_q)) + 
  labs(title = "Mean Abnormal Value",
       x = "Absolute ES Quantile", y = "Mean AV") + 
  geom_point() +
  theme_bw()

# Abnormal volume[0,1], not sure of we have over two days currently

# AV20 = news + ES_quantile2
mod1 <- stock_data %>%
  lm(AV20_reg ~ news, data = .)
coeftest(mod1, df = Inf, vcov = vcovHC(mod1, type = "HC1"))

# AV20 = news + ES_quantile2 + controls
mod2 <- stock_data %>%
  lm(AV_alt20lag ~ news + ES_quantile2 + 
       month + (logMktCap + logBtoM + mean_IO_share + mean_analyst), data = .)
coeftest(mod2, df = Inf, vcov = vcovHC(mod2, type = "HC1"))

# AV20 = news + ES_quantile2, only top and bottom
mod3 <- stock_data %>% filter(ES_quantile2 == 1 | ES_quantile2 == 5) %>% mutate(ES_quantile = ifelse(ES_quantile2 == 1, 0, 1)) %>% 
  lm(AV_alt20lag ~ news + I(ES_quantile2), data = .)
coeftest(mod3, df = Inf, vcov = vcovHC(mod3, type = "HC1"))

# AV20 = news + ES_quantile2 + controls, only top and bottom
mod4 <- stock_data %>% filter(ES_quantile2 == 1 | ES_quantile2 == 5) %>% mutate(ES_quantile = ifelse(ES_quantile2 == 1, 0, 1)) %>% 
  lm(AV_alt20lag ~ news + I(ES_quantile2) + 
       month + (logMktCap + logBtoM + mean_IO_share + mean_analyst), data = .)
coeftest(mod4, df = Inf, vcov = vcovHC(mod4, type = "HC1"))

estimatr::lm_robust(AV_alt20lag ~ news + ES_quantile2, data = stock_data, se_type = "stata", clusters = date)
estimatr::lm_robust(AV_alt20lag ~ news + ES_quantile2, data = stock_data, se_type = "stata", clusters = day)

estimatr::lm_robust(AVol_reg1 ~ news + ES_quantile2, data = stock_data, se_type = "stata", clusters = date)[1]

# Volatility --------------------------------------------------------------

stock_data  %>%
  filter(news_q == "N1" | news_q == "N5") %>%
  group_by(news_q,ES_quantile2) %>%
  summarise(mean = mean(AVol_alt3, na.rm = T)) %>% ungroup() %>%
  na.omit(ES) %>% 
  ggplot(., aes(x = ES_quantile2, y = mean, group = news_q)) +
  geom_line(aes(colour = news_q)) + 
  labs(title = "Mean Abnormal Value",
       x = "Absolute ES Quantile", y = "Mean AV") + 
  geom_point() +
  theme_bw()

# Reg 1: Avol1
lm.fit=lm(AVol_reg1 ~ news + ES_quantile2 + month + logMktCap + logBtoM + mean_IO_share + mean_analyst, data=stock_data)
coeftest(lm.fit, df = Inf, vcov = vcovHC(lm.fit, type = "HC1"))
summary(lm.fit)

# Reg 2: Avol2
lm.fit=lm(AVol_reg2 ~ news + ES_quantile2 + month + logMktCap + logBtoM + mean_IO_share + mean_analyst, data=stock_data)
coeftest(lm.fit, df = Inf, vcov = vcovHC(lm.fit, type = "HC1"))
summary(lm.fit)

# Reg 3: Avol3
lm.fit=lm(AVol_reg3 ~ news + ES_quantile2 + month + logMktCap + logBtoM + mean_IO_share + mean_analyst, data=stock_data)
coeftest(lm.fit, df = Inf, vcov = vcovHC(lm.fit, type = "HC1"))
summary(lm.fit)

coeftest(lm.fit, df = Inf, vcov = vcovCL(lm.fit, cluster = ~ date, type = "HC1")) # denne funker, nesten lik



# Plots and tables for paper ----------------------------------------------



# Table 1a:
#   Distribution of earnings during the year
#   Use earnings data from stock_analysis3
stargazer(summary(as.factor(month(earning_data$date,label = T, abbr = T))), #Create variable for month
          type = "html",
          title = "Distribution of earnings announcment through the sample period")
# add: width="600" ,after: style="text-align:center"

# As a plot
plot1A <- summary(as.factor(month(earning_data$date,label = T, abbr = T))) %>% as.data.frame() %>% 
  rename(earnings = ".")
plot1A$month <- factor(row.names(plot1A),levels = c("jan","feb","mar","apr","mai","jun","jul","aug","sep","okt","nov", "des"))

plot1 <-plot1A %>%  
  ggplot(.,aes(x = month, y = earnings)) +
  geom_bar(stat = "identity",fill = "#2549b9") +
  #scale_fill_manual("News Q", values = c("N1" = "#4EBCD5", "N5" = "#1C237E")) +
  labs(title = "Announcements per Month",
       x = "Month", y = "Number of Announcements") +
  theme_bw() +
  theme(text = element_text(family = "serif"),
        #legend.position="top",
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())

# Days of week
stargazer(summary(as.factor(wday(stock_data$date,label = T, abbr = F))), #Create variable for month
          type = "html",
          title = "Distribution of Earnings Announcment through the sample period")

# As a plot
plot1B <-summary(as.factor(wday(stock_data$date,label = T, abbr = F))) %>% as.data.frame() %>% 
  rename(earnings = ".")
plot1B$month <- factor(row.names(plot1B), levels = c("mandag", "tirsdag", "onsdag", "torsdag", "fredag", "lørdag", "søndag"))

plot2 <- plot1B %>%  
  ggplot(.,aes(x = month, y = earnings)) +
  geom_bar(stat = "identity",fill = "#2549b9") +
  #scale_fill_manual("News Q", values = c("N1" = "#4EBCD5", "N5" = "#1C237E")) +
  labs(title = "Announcements per Day",
       x = "Day of Week", y = "Number of Announcements") +
  theme_bw() +
  theme(text = element_text(family = "serif"),
        #legend.position="top",
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())

library(gridExtra)
grid.arrange(plot1, plot2, ncol=2)

rm(plot1B, plot1A)

# Table 1b.1:
#   Mean, median, . see Ungehauer
df <- stock_data %>% select(news, ES,CAR2,CAR40,AV_alt30,AVol_alt1,CUR_MKT_CAP ,mean_analyst, mean_IO_share, 
                            BtoM, share_turnover30) %>% 
  mutate(news = news/1000,
         mean_analyst = exp((mean_analyst))-1,
         CUR_MKT_CAP = (CUR_MKT_CAP)/1000)

stargazer(df, type = "html",
          title = "Table 2: Summary statistics news and company data",
          covariate.labels=c("Pageviews Corona-News (1000)","Earnings Surprise",
                             "CAR[0,2]","CAR[3,40]","AV[0,1]","AVOLA[0,1]",
                             "Market cap (BNOK)", "Nr. of Analysts", "Inst. Ownership (%)",
                             "Book-to-Market (%)","Share Turnover"),
          notes.append = FALSE,
          notes.label = "",
          notes.align = "l",
          #table.layout = "n-=ldc-tas-",
          table.layout = "n-=d",
          notes = "JAAAA",
          median = T)

# Did not find an easy way to edit the number of observations in the summary table.
# Add readtime as well?
# width="800"

# Table 1b.2 - Difference between top news and bottom news __________________________________________________
stock_data$news_q <- stock_data$news_month4

# T-test
# X needs to be values of N1 and Y needs to be values of N5
df <- stock_data %>%
  select(news_q, news, ES, CAR2, CAR40, AV_alt20lag, AVol_alt, CUR_MKT_CAP, BtoM, mean_analyst, mean_IO_share, share_turnover30) %>%
  filter(news_q == "N1" | news_q == "N4") %>%
  group_by(news_q) %>%
  summarize(cnews = mean(news, na.rm = T)/1000,
            ES = mean(ES, na.rm = T),
            CAR1 = mean(CAR2, na.rm = T),
            CAR40 = mean(CAR40, na.rm = T),
            AV = mean(AV_alt20lag, na.rm = T),
            AVol = mean(AVol_alt, na.rm = T),
            mkt_cap = mean(CUR_MKT_CAP, na.rm = T)/1000,
            B_M = mean(BtoM, na.rm = T),
            analyst = mean(mean_analyst, na.rm = T),
            IO_share = mean(mean_IO_share, na.rm = T),
            share_turnover = mean(share_turnover30, na.rm = T)) %>%
  t() %>% as.data.frame() %>% rename(N1 = V1, N4 = V2) %>%
  subset(N1 != "N1") %>% rownames_to_column() %>%
  mutate_at(c("N1","N4"), as.numeric) %>%
  mutate(diff = N4-N1)

df2 <- stock_data %>% 
  select(news_q, news, ES, CAR2, CAR40, AV_alt20lag, AVol_alt, CUR_MKT_CAP, BtoM, mean_analyst, mean_IO_share, share_turnover30) %>% 
  filter(news_q == "N1" | news_q == "N4")


# loop calculating p-values and standard errors
for (i in 2:(ncol(df2)-1)) {
  test <- t.test(df2[df2$news_q == "N1", i], df2[df2$news_q == "N4", i])
  df$p[i-1] <- test$p.value
  df$stderr[i-1] <- test$statistic
}


df$rowname <- c("Corona- Pageviews (1000)", "Earnings Surprise", "CAR[0,2]", "CAR[3,42]", "AV[0,1]", "AVOLA[0,1]", "Market Cap (BNOK)", "Book-to-Market (%)", "Nr. of Analysts", "Inst. Ownership (%)", "Share Turnover")
stargazer(df, 
          digits = 3,
          header = F,
          type = "html",
          summary = F,
          covariate.labels=c("", "",  "N1", "N4", "Difference", "p-value", "t-value"),
          title = "Summary News and Company Data (2)")

rm(df,df2,test,i)

# Table 1c_________________________________________________________________________________________
# Average surprise within each quantile
df2 <- stock_data %>% select(ES, ES_quantile)%>% group_by(ES_quantile) %>% drop_na() %>% 
  summarize(ES=mean(ES), N=n()) %>% mutate(ES=format(round(ES, 3), nsmall = 3)) %>% 
  t() %>% as.data.frame() 

names(df2) <- as.matrix(df2[1, ])
df2 <- df2[-1, ]
df2[] <- lapply(df2, function(x) type.convert(as.character(x)))

stargazer(df2, type= "html", summary = F,
          covariate.labels = c("Quantile"),
          title = "Table 4: Mean earning surprise in each quantile")
# width = "600"
rm(df2)

# Table 3: Abnormal Volume______________________________________________________________________________
# Creating a nice table for output statistics:
# No interaction
stock_data$news2 <- stock_data$news/1000
mod1 <- stock_data %>%
  lm(AV_alt20lag ~ news2 + ES_quantile2, data = .)
coeftest(mod1, df = Inf, vcov = vcovHC(mod1, type = "HC1"))

mod2 <- stock_data %>%
  lm(AV_alt20lag ~ news2 + ES_quantile2 + 
       month + (logMktCap + logBtoM + mean_IO_share + mean_analyst), data = .)
coeftest(mod2, df = Inf, vcov = vcovHC(mod2, type = "HC1"))

mod3 <- stock_data %>% filter(ES_quantile2 == 1 | ES_quantile2 == 5) %>% mutate(ES_quantile = ifelse(ES_quantile2 == 1, 0, 1)) %>% 
  lm(AV_alt20lag ~ news2 + I(ES_quantile2), data = .)
coeftest(mod3, df = Inf, vcov = vcovHC(mod3, type = "HC1"))

mod4 <- stock_data %>% filter(ES_quantile2 == 1 | ES_quantile2 == 5) %>% mutate(ES_quantile = ifelse(ES_quantile2 == 1, 0, 1)) %>% 
  lm(AV_alt20lag ~ news2 + I(ES_quantile2) + 
       month + (logMktCap + logBtoM + mean_IO_share + mean_analyst), data = .)
coeftest(mod4, df = Inf, vcov = vcovHC(mod4, type = "HC1"))

# Values in table
rob_se <- list(sqrt(diag(vcovHC(mod1, type = "HC1"))),
               sqrt(diag(vcovHC(mod2, type = "HC1"))),
               sqrt(diag(vcovHC(mod3, type = "HC1"))),
               sqrt(diag(vcovHC(mod4, type = "HC1"))))

t_vals <- list(coef(mod1)/sqrt(diag(vcovHC(mod1, type = "HC1"))),
               coef(mod2)/sqrt(diag(vcovHC(mod2, type = "HC1"))),
               coef(mod3)/sqrt(diag(vcovHC(mod3, type = "HC1"))),
               coef(mod4)/sqrt(diag(vcovHC(mod4, type = "HC1"))))

p_vals <- list(coeftest(mod1, df = Inf, vcov = vcovHC(mod1, type = "HC1"))[,4], 
               coeftest(mod2, df = Inf, vcov = vcovHC(mod2, type = "HC1"))[,4],
               coeftest(mod3, df = Inf, vcov = vcovHC(mod3, type = "HC1"))[,4],
               coeftest(mod4, df = Inf, vcov = vcovHC(mod4, type = "HC1"))[,4])


stargazer(mod1, mod2, mod3, mod4, 
          digits = 3,
          header = FALSE,
          type = "html",
          se = t_vals,
          p = p_vals,
          dep.var.labels = c("AV[0,1]"),
          omit.stat = c("adj.rsq","f","ser"),
          omit = c("month", "logMktCap", "logBtoM", "mean_IO_share", "mean_analyst", "Constant"),
          add.lines = list(c("Controls", "", "X","","X")),
          table.layout = "n-=ldc-tas-",
          covariate.labels = c("Corona News", "Absolut Earnings <br> Surprise Quantile", "Top and Bottom Absolut <br> Earnings Surprise Quantiles"),
          title = "Table 3: <br> Linear Regression Models of how Corona-News effect Abnormal Volume",
          notes = "Table 3 shows the results of the multivariate regression on how the amount of Corona articles published by NRK effects abnormal trading volume of stocks publishing an earnings announcement on the same day. In regression (3) and (4) only the observations from the top two Cov-quantiles are used. Regression (2) and (4) adjust for control the variables Month, Market Capitalization Deciles, Market-to-Book ratio Deciles, IO share and Number of Analysts. All control variables are averaged for each month. Standard errors are adjusted for heteroskedasticity and t-values are reported in the parentheses. *, **, and *** represent significance at the 10%, 5% and 1% level, respectively.",
          notes.append = FALSE,
          notes.label = "",
          notes.align = "l",
          model.numbers = FALSE,
          star.cutoffs = c(0.1, 0.05, 0.01),
          report = "vc*s",
          column.labels = c("(1)", "(2)", "(3)", "(4)"))





# Table 4: Abnormal Volatility________________________________________________________________________________
stock_data$news2 <- stock_data$news/1000
stock_data$news_q <- stock_data$news_month4
stock_data$month <- factor(stock_data$month,
                                    levels = c("February", "March", "April", "May", "June", "July", "August", "September"))

stock_data$day <- factor(stock_data$day,
                                  levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))


mod1 <- lm(AVol_reg1 ~ news2, data = stock_data)
mod2 <- lm(AVol_reg1 ~ news2 + ES_quantile2 + month + logMktCap + logBtoM + mean_IO_share + mean_analyst, data = stock_data)

mod3 <- stock_data %>%  filter(ES_quantile2 == 1 | ES_quantile2 == 5) %>% 
  lm(AVol_reg1 ~ news2, data = .)

mod4 <- stock_data %>% filter(ES_quantile2 == 1 | ES_quantile2 == 5) %>% 
  lm(AVol_reg1 ~ news2 + ES_quantile2 + month + logMktCap + logBtoM + mean_IO_share + mean_analyst, data = .)

mod5 <- stock_data %>% filter(news_q == "N1" | news_q == "N4") %>% 
  lm(AVol_reg1 ~ news2, data = .)

mod6 <- stock_data %>% filter(news_q == "N1" | news_q == "N4") %>% 
  lm(AVol_reg1 ~ news2 + ES_quantile2 + month + logMktCap + logBtoM + mean_IO_share + mean_analyst, data = .)


rob_se <- list(sqrt(diag(vcovHC(mod1, type = "HC1"))),
               sqrt(diag(vcovHC(mod2, type = "HC1"))),
               sqrt(diag(vcovHC(mod3, type = "HC1"))),
               sqrt(diag(vcovHC(mod4, type = "HC1"))),
               sqrt(diag(vcovHC(mod5, type = "HC1"))),
               sqrt(diag(vcovHC(mod6, type = "HC1"))))

t_vals <- list(coef(mod1)/sqrt(diag(vcovHC(mod1, type = "HC1"))),
               coef(mod2)/sqrt(diag(vcovHC(mod2, type = "HC1"))),
               coef(mod3)/sqrt(diag(vcovHC(mod3, type = "HC1"))),
               coef(mod4)/sqrt(diag(vcovHC(mod4, type = "HC1"))),
               coef(mod5)/sqrt(diag(vcovHC(mod5, type = "HC1"))),
               coef(mod6)/sqrt(diag(vcovHC(mod6, type = "HC1"))))

p_vals <- list(coeftest(mod1, df = Inf, vcov = vcovHC(mod1, type = "HC1"))[,4], 
               coeftest(mod2, df = Inf, vcov = vcovHC(mod2, type = "HC1"))[,4],
               coeftest(mod3, df = Inf, vcov = vcovHC(mod3, type = "HC1"))[,4],
               coeftest(mod4, df = Inf, vcov = vcovHC(mod4, type = "HC1"))[,4], 
               coeftest(mod5, df = Inf, vcov = vcovHC(mod5, type = "HC1"))[,4],
               coeftest(mod6, df = Inf, vcov = vcovHC(mod6, type = "HC1"))[,4])

stargazer(mod1, mod2, mod3, mod4, mod5, mod6,
          digits = 3,
          header = FALSE,
          type = "html",
          #se = rob_se,
          se = t_vals,
          p = p_vals,
          dep.var.labels = c("AVol[0,1]"),
          omit.stat = c("adj.rsq","f","ser"),
          omit = c("Constant"),
          add.lines = list(c("Controls", "X","X","X")),
          table.layout = "n-=ldc-tas-",
          #covariate.labels = c("Corona News", "Absolut Earnings <br> Surprise Quantile"),
          title = "Table 4: <br> Linear Regression Models of how Corona-News effect Abnormal Volatility",
          notes = "Table 4 shows the results of the multivariate regression on how the amount of clicks on Corona articles published by NRK effects abnormal volatility of stocks around earnings announcements. Abnormal volatility for each stock is the average abnormal volatility of day 0 and 1. Since volatility is expected to react to both positive and negative earnings surprises the absolute earnings surprise is used and divided into 5 quantiles. Control variables include indicators for each month, market capitalization deciles, market-to-book ratio deciles, share of institutional ownership, and Number of Analysts. All control variables are averaged for each month. Standard errors are adjusted for heteroskedasticity and t-values are reported in the parentheses. *, **, and *** represent significance at the 10%, 5% and 1% level, respectively.",
          notes.append = FALSE,
          notes.label = "",
          notes.align = "l",
          model.numbers = FALSE,
          report = "vc*s",
          column.labels = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)"))



# Table 5: CAR _________________________________________________________________________________________
stock_data$news2 <- stock_data$news/1000
stock_data$CAR1_2 <- stock_data$CAR1*100
stock_data$ES_quantile <- as.numeric(stock_data$ES_quantile)


mod1 <- lm(CAR1_2 ~ news2*ES_quantile, data = stock_data)
mod2 <- lm(CAR1_2 ~ news2*ES_quantile + ES_quantile*(month +logMktCap + BtoM + mean_IO_share + mean_analyst + share_turnover30), data = stock_data)
#mod3 <- stock_data %>% filter(ES_quantile == 1 | ES_quantile == 5) %>% lm(CAR1_2 ~ news2*I(ES_quantile), data = .)
mod4 <- stock_data %>% filter(ES_quantile == 1 | ES_quantile == 5) %>% lm(CAR1_2 ~ news2*I(ES_quantile) + I(ES_quantile)*(month + logMktCap + BtoM + mean_IO_share + mean_analyst + share_turnover30), data = .)


t_vals <- list(coef(mod1)/sqrt(diag(vcovHC(mod1, type = "HC1"))),
               coef(mod2)/sqrt(diag(vcovHC(mod2, type = "HC1"))),
               #coef(mod3)/sqrt(diag(vcovHC(mod3, type = "HC1"))),
               coef(mod4)/sqrt(diag(vcovHC(mod4, type = "HC1"))))

p_vals <- list(coeftest(mod1, vcov = vcovHC(mod1, type = "HC1"))[,4], 
               coeftest(mod2, vcov = vcovHC(mod2, type = "HC1"))[,4],
               #coeftest(mod3, vcov = vcovHC(mod3, type = "HC1"))[,4],
               coeftest(mod4, vcov = vcovHC(mod4, type = "HC1"))[,4])


# Multiply all dependent variable with 100 to get basis points.

stargazer(mod1, mod2, mod4,
          digits = 3,
          header = FALSE,
          type = "html",
          se = t_vals,
          p = p_vals,
          dep.var.labels = c("CAR[0,1]"),
          omit.stat = c("adj.rsq","f","ser"),
          omit = c("month", "logMktCap", "BtoM", "mean_IO_share", "mean_analyst", "share_turnover30", "Constant"),
          add.lines = list(c("Controls Interacted","", "X", "X")),
          table.layout = "n-=ldc-tas-",
          #covariate.labels = c("Corona News", "ES","ES * Corona News","TOP ES", "TOP ES * Corona News"),
          title = "Table 5: <br> Linear Regression Models of how Corona-News effect short-term abnormal returns",
          notes = "Table 5 shows the results of the multivariate regression on how the amount of clicks on Corona articles published by NRK affects abnormal returns of stocks around earnings announcements. Abnormal returns are the returns for each company adjusted for the market return for the industry they are within. Earnings surprises are divided into 5 quantiles where the first quintile is the most negative surprise and the fifth quintile the most positive. Control variables include indicators for each month, market capitalization deciles, market-to-book ratio deciles, share of institutional ownership,  Number of Analysts, and share turnover the last 30 days. All control variables are averaged for each month and interacted with the earning surprise quantiles.. Standard errors are adjusted for heteroskedasticity and t-values are reported in the parentheses. *, **, and *** represent significance at the 10%, 5% and 1% level, respectively.",
          notes.append = FALSE,
          notes.label = "",
          notes.align = "l",
          model.numbers = FALSE,
          report = "vc*s",
          column.labels = c("(1)", "(2)", "(3)"))

# Table 6: CAR40______________________________________________________________________________________
# CAR40 regressions 

stock_data$news2 <- stock_data$news/1000
stock_data$CAR40_2 <- stock_data$CAR40*100

mod1 <- lm(CAR40_2 ~ news2*ES_quantile, data = stock_data)
mod2 <- lm(CAR40_2 ~ news2*ES_quantile + month + ES_quantile*(logMktCap + logBtoM + mean_IO_share + mean_analyst + share_turnover30), data = stock_data)
mod3 <- stock_data %>% filter(ES_quantile == 1 | ES_quantile == 5) %>% lm(CAR40_2 ~ news2*I(ES_quantile), data = .)
mod4 <- stock_data %>% filter(ES_quantile == 1 | ES_quantile == 5) %>% lm(CAR40_2 ~ news2*I(ES_quantile) + month + I(ES_quantile)*(logBtoM + mean_IO_share + mean_analyst + share_turnover30), data = .)

t_vals <- list(coef(mod1)/sqrt(diag(vcovHC(mod1, type = "HC1"))),
               coef(mod2)/sqrt(diag(vcovHC(mod2, type = "HC1"))),
               coef(mod3)/sqrt(diag(vcovHC(mod3, type = "HC1"))),
               coef(mod4)/sqrt(diag(vcovHC(mod4, type = "HC1"))))

p_vals <- list(coeftest(mod1, df = Inf, vcov = vcovHC(mod1, type = "HC1"))[,4], 
               coeftest(mod2, df = Inf, vcov = vcovHC(mod2, type = "HC1"))[,4],
               coeftest(mod3, df = Inf, vcov = vcovHC(mod3, type = "HC1"))[,4],
               coeftest(mod4, df = Inf, vcov = vcovHC(mod4, type = "HC1"))[,4])

# By using CAR40 * 100 we get basis points

stargazer(mod1, mod2, mod3, mod4,
          digits = 3,
          header = FALSE,
          type = "html",
          se = t_vals,
          p = p_vals,
          dep.var.labels = c("CAR[3,40]"),
          omit.stat = c("adj.rsq","f","ser"),
          omit = c("month", "logMktCap", "logBtoM", "mean_IO_share", "mean_analyst", "share_turnover30", "Constant"),
          add.lines = list(c("Controls","", "X", "", "X")),
          table.layout = "n-=ldc-tas-",
          covariate.labels = c("Corona News", "ES","ES * Corona News","TOP ES", "TOP ES * Corona News"),
          title = "Table 6: <br> Linear Regression Models of how Corona-News effect long-term abnormal returns",
          notes = "Table 6 shows the results of the multivariate regression on how the amount of clicks on Corona articles published by NRK affects abnormal returns of stocks around earnings announcements. Abnormal returns are the returns for each company adjusted for the market return for the industry they are within. Cumulative abnormal returns are calculated from 2 days after announcement to 40 days after announcement. Earnings surprises are divided into 5 quantiles where the first quintile is the most negative surprise and the fifth quintile the most positive. Control variables include indicators for each month, market capitalization deciles, market-to-book ratio deciles, share of institutional ownership,  number of analysts, and share turnover the last 30 days. All control variables are averaged for each month and interacted with the earning surprise quantiles. Standard errors are adjusted for heteroskedasticity and t-values are reported in the parentheses. *, **, and *** represent significance at the 10%, 5% and 1% level, respectively.",
          notes.append = FALSE,
          notes.label = "",
          notes.align = "l",
          model.numbers = FALSE,
          report = "vc*s",
          column.labels = c("(1)", "(2)", "(3)", "(4)"))


# Plots
# Figure 5a: CAR[0,1]_________________________________________________________________________________
stock_data$news_q <- stock_data$news_month4
stock_data %>% 
  group_by(ES_quantile) %>% 
  mutate(ES_names = round(mean(ES), 3)) %>% ungroup() %>% 
  select(news_q, ES_quantile, CAR1, ES_names, ES) %>%
  filter(news_q == "N1" | news_q == "N4") %>%
  mutate(names = ifelse(news_q == "N1", "Corona Bottom", "Corona Top")) %>% 
  group_by(names, ES_quantile) %>%
  summarise(mean = mean(CAR1, na.rm = T), ES_names = ES_names, ES = ES) %>% ungroup() %>%
  na.omit(ES) %>% 
  
  ggplot(., aes(x = as.factor(ES_names), y = mean*100, group = names, linetype = names, colour = names, shape = names)) +
  geom_line() + 
  geom_point() +
  scale_color_manual(name = "News", values = c("#4EBCD5", "#1C237E")) +
  scale_linetype_manual(name = "News", values = c("solid", "longdash")) +
  scale_shape_manual(name = "News", values = c(15, 17)) +
  labs(title = "Average CAR[0,1] for Each Earnings Surprise Quintiles",
       x = "Average Earnings Surprise in each Quintiles", y = "CAR[0,1] (%)") +
  theme_bw() +
  theme(text = element_text(family = "serif"),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank())




# Figure 5b: CAR[2,40]
stock_data$news_q <- stock_data$news_month
stock_data %>% 
  group_by(ES_quantile) %>% 
  mutate(ES_names = round(mean(ES), 3)) %>% ungroup() %>% 
  select(news_q, ES_quantile, CAR40, ES_names, ES) %>%
  filter(news_q == "N1" | news_q == "N5") %>%
  group_by(news_q, ES_quantile) %>%
  summarise(mean = mean(CAR40, na.rm = T), ES_names = ES_names, ES = ES) %>% ungroup() %>%
  na.omit(ES) %>% 
  
  ggplot(., aes(x = as.factor(ES_names), y = mean*100, group = news_q, linetype = news_q, colour = news_q, shape = news_q)) +
  geom_line() + 
  geom_point() +
  scale_color_manual(name = "News Q", values = c("#4EBCD5", "#1C237E")) +
  scale_linetype_manual(name = "News Q", values = c("solid", "longdash")) +
  scale_shape_manual(name = "News Q", values = c(15, 17)) +
  labs(title = "Figure 6: Average CAR[2,40] for Each Earnings Surprise Quantile",
       x = "Average Earnings Surprise in each Quantile", y = "CAR[2,40] in %") +
  theme_bw() +
  theme(text = element_text(family = "serif"),
        panel.grid.minor.y = element_blank())