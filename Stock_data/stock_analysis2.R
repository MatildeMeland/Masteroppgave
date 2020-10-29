# Libraries
library(readxl)
library(tidyverse)
library(lubridate)
library(zoo)

# Load and format data ----------------------------------------------------
stock_data <- read_excel("Stock_data/stock_final.xlsx") %>% as.data.frame()

# Fix problem with mismatching dates
stock_data <- merge(stock_data[,-2],stock_data[,1:2], by=1:2, all.y = T) %>% 
  select(-EQY_SH_OUT)

colnames(stock_data)[2] <- "date"

# Change structure of variables from character to numeric
stock_data[,3:7] <- sapply(stock_data[,3:7],as.numeric)

# Change NAN to 0 in volume
stock_data$PX_VOLUME[is.na(stock_data$PX_VOLUME)] <- 0

# Change NA's in PX_LAST to last value of PX_LAST


# Fix px_last
for (i in unique(stock_data$Security)) {
  stock_data$PX_LAST[stock_data$Security == i] <- na.locf(stock_data$PX_LAST[stock_data$Security == i], na.rm = FALSE)
}


# Fix px_open
for (i in unique(stock_data$Security)) {
  x <- min(as.numeric(row.names(stock_data[stock_data$Security == i,]))) + 1
  z <- max(as.numeric(row.names(stock_data[stock_data$Security == i,])))
  
  for (j in x:z) {
    if (is.na(stock_data$PX_OPEN[j])) {
      stock_data$PX_OPEN[j] <- stock_data$PX_LAST[j-1]
    }
  }
}



# Accounting variables ----------------------------------------------------

acc_vars <- read_excel("Stock_data/accounting_vars.xlsx") %>% as.data.frame() %>% 
  select(-c(time, PX_TO_BOOK_RATIO, date2, estimated, actual, comparable, date6, period))

acc_vars$date5 <- as.Date(as.numeric(acc_vars$date5), origin = "1899-12-30")

# Remove PX to book ratio? Cant seem to understand why one of them has more missing values than the other. 
# Remove earning estimates, it has so little data it is useless
# From my understanding they should have the exact same value?

# An alternative to EQY_INST_SH_OUT is to take the average of each month and use that as values instead? This value rarely changes alot
# Also need to apply the calculations for all months 


test <- acc_vars %>% # select(Security, date) %>% 
  mutate(month = month(date),
         year = year(date) )

test2 <- acc_vars %>% select(Security, date, date3, EQY_INST_PCT_SH_OUT) %>% 
  mutate(month = month(date3),
         year = year(date3) ) %>% 
  group_by(Security, year, month) %>% 
  summarize(mean_share = mean(EQY_INST_PCT_SH_OUT))

acc_vars <- merge(test,test2, all.x = T)

rm(test, test2)
 

# Try to change number of analysts similar to that of IO percentage

test <- acc_vars %>% 
  mutate(month = month(date),
         year = year(date) )

test2 <- acc_vars %>% select(Security, date, date5, TOT_ANALYST_REC) %>% 
  mutate(month = month(date5),
         year = year(date5) ) %>% 
  group_by(Security, year, month) %>% 
  summarize(mean_analyst = mean(TOT_ANALYST_REC))

acc_vars <- merge(test,test2, all.x = T) %>% 
  select(-c(EQY_INST_PCT_SH_OUT, date3, date5, TOT_ANALYST_REC, date1, date4, month, year ))

rm(test, test2)


# Announcement date modification ------------------------------------------


# Loading in data
earning_data <- read_csv("Stock_data/earning_data.csv") %>% 
  select(-c(X1,industri, industry))

# Create equal tickers
stock_data$Security <- gsub(" .*$", "", stock_data$Security, ignore.case = T)

# Change column names
colnames(earning_data) <- c("Security", "Name", "date")

# Remove duplicated dates with about 10 days in between
earning_data <- earning_data %>%
  group_by(Security) %>% 
  mutate(DUP = ifelse(abs(difftime(date, lag(date))) <= 10,1,0)) %>% 
  subset(DUP %in%  0| DUP %in%  NA)


# Creates a dataset with only earnings dates in stock data.
combined <- right_join(earning_data, stock_data, by = "Security" ) %>% 
  mutate(AD = ifelse(date.x == date.y,1,0)) %>% 
  subset(AD ==1) %>% select(-c(date.y, AD))

colnames(combined[3]) <- "date"


                 
# Calculate abnormal volume -----------------------------------------------

# Volume sum
#stock_data$volume_sum <- rollsumr(df$PX_VOLUME, k = 30, fill = NA)


# Odeen method
stock_data$AV_ODEEN <- stock_data$PX_VOLUME/(rollsumr(stock_data$PX_VOLUME, k = 30, fill = NA)/30)


# DellaVigna formula
stock_data$AV_DV <- log(((stock_data$PX_VOLUME + lead(stock_data$PX_VOLUME))/2)+1) - log(((rollsumr(stock_data$PX_VOLUME, k = 41, fill = NA)-rollsumr(stock_data$PX_VOLUME, k = 11, fill = NA))/30)+1)


# Cumulative abnormal return ----------------------------------------------


# Calculating market return for each company in each industry
# - Import dataset with industry specifications

Ticker_list <- read_excel("Peer_companies/Ticker-list.xlsx")

colnames(Ticker_list)[5] <- "Security"

stock_data <- merge(stock_data, Ticker_list, by = "Security") %>% 
  select(-c(ticker, name, industri))

# Create a value for total market cap for each industry and market return for each security.
# Further use these values to calculate cumulative abnormal return
stock_data <- stock_data %>% 
  group_by(industry, date) %>%
  summarize(total_mkt_cap = sum(CUR_MKT_CAP, na.rm = T)) %>% ungroup %>% 
  merge(., stock_data, by = c("industry", "date")) %>% 
  arrange(., Security) %>% 
  group_by(Security) %>% 
  
  mutate(MR = c(NA,diff(total_mkt_cap))/lag(total_mkt_cap, 1)) %>%        # market return for each industry
  
  mutate(daily_return = (PX_LAST-PX_OPEN)/PX_OPEN) %>%                    # daily return for each company

  mutate(CAR1 = c(rep(NA,times = 1), as.numeric(rollapply(1 + daily_return, 2, prod,partial = FALSE, align = "left"))) # Cumulative abnormal return (CAR) for each company in each industry first 2 days
         -c(rep(NA,times = 1), as.numeric(rollapply(1 + MR, 2, prod,partial = FALSE, align = "left")))) %>% 
  
  mutate(CAR40 = c(rep(NA,times = 39), as.numeric(rollapply(1 + daily_return, 40, prod,partial = FALSE, align = "left"))) # Cumulative abnormal return (CAR) for each company in each industry first 40 days
         -c(rep(NA,times = 39), as.numeric(rollapply(1 + MR, 40, prod,partial = FALSE, align = "left"))))









# Econometrics ------------------------------------------------------------

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











# Plots -------------------------------------------------------------------


# Plot of market over time
test %>% ggplot(.,aes(x=date, y=total_mkt_cap/3435)) + #divide by 3435 to make it look similar to that of OSBX
  ylim(550, 900) +
  geom_line() +
  ggtitle("Market over time for all stocks listed on OSBX") +
  labs(y = "Normalized market value") +
  theme_bw()

# Plot of return over time
test %>% ggplot(.,aes(x=date, y=return)) +
  #ylim(550, 900) +
  geom_line() +
  ggtitle("Market return over time for all stocks listed on OSBX") +
  labs(y = "Return") +
  theme_bw()



# Graph of trading volume over time
stock_data %>% 
  group_by(date) %>% 
  summarize(total_volume = sum(PX_VOLUME)) %>% ungroup %>% 
  ggplot(., aes(x=date, y=total_volume/10^6)) +
  geom_line() +
  ggtitle("Total trading volume over time") +
  labs(y = "Million trades", x= "Date") +
  theme_bw()

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

# Could it be that most of this variation in volume is caused by Norwegian?
tot_vol <- stock_data %>% 
  group_by(Security) %>% 
  summarize(total_volume = sum(PX_VOLUME))
  # No.. KOA has had most trades over the period.

tot_vol2 <- stock_data %>% 
  group_by(Security) %>%
  summarize(avg_mkt_cap = mean(CUR_MKT_CAP, na.rm = T)) %>%
  remove_missing() %>% 
  mutate(Size_Level = cut(avg_mkt_cap,
                          quantile(avg_mkt_cap, c(0, .25, .75, 1)),
                          labels = c('Small', 'Medium', 'Big'),
                          include.lowest = TRUE)) %>% 
  merge(.,tot_vol, by = "Security")

# Various plots 

# Calculate volatility ----------------------------------------------------
stock_data$PX_LAST <- na.locf(stock_data$PX_LAST) # Replace NAs in PX_LAST with previous non NA value, there may be a problem if first value is NA - need to look at this


stock_data$rn <- log(stock_data$PX_LAST/lag(stock_data$PX_LAST))

stock_data <- stock_data[-1,]

stock_data$volatility_calc <- rollmeanr(stock_data$rn, k = 21, fill = NA)

mean(stock_data$rn[2:21])







 
