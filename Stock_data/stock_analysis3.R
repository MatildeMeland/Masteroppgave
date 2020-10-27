# Libraries
library(readxl)
library(tidyverse)
library(zoo)

# Load and format data ----------------------------------------------------
stock_data <- read_excel("Stock_data/stock_final.xlsx") %>% as.data.frame()

# Fix problem with mismatching dates
stock_data <- 
  merge(stock_data[,-2], stock_data[,1:2], by=1:2, all.y = T) %>% 
  select(-EQY_SH_OUT)

colnames(stock_data)[2] <- "date"

stock_data$date <- as.Date(stock_data$date, format="%d.%m.%Y")

# Change structure of variables from character to numeric
stock_data[,3:7] <- sapply(stock_data[,3:7],as.numeric)

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

# Remove unnecessary variables
rm(i, j, x, z)

# Calculate abnormal returns ----------------------------------------------

# Calculate return 
stock_data$daily_return <- (stock_data$PX_LAST-stock_data$PX_OPEN)/stock_data$PX_OPEN


# Calculate abnormal volume -----------------------------------------------
# Odeen method
for (i in unique(stock_data$Security)) {
  stock_data$AV_ODEEN[stock_data$Security == i] <- stock_data$PX_VOLUME[stock_data$Security == i]/(rollsumr(stock_data$PX_VOLUME[stock_data$Security == i], k = 30, fill = NA)/30)
}

# DellaVigna formula
for (i in unique(stock_data$Security)) {
  stock_data$AV_DV[stock_data$Security == i] <- log(((stock_data$PX_VOLUME[stock_data$Security == i] + lead(stock_data$PX_VOLUME[stock_data$Security == i]))/2)+1) - log(((rollsumr(stock_data$PX_VOLUME[stock_data$Security == i], k = 41, fill = NA)-rollsumr(stock_data$PX_VOLUME[stock_data$Security == i], k = 11, fill = NA))/30)+1)
}

# Average abnormal volume by day
AV_avg <- stock_data %>% 
  select(date, AV_ODEEN) %>% 
  group_by(date) %>% 
  summarise(av.mean = mean(AV_ODEEN, na.rm = T))


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


# Data formating for regression -------------------------------------------
# Dummy variable for government announcments
news_data$xl <- ifelse(news_data$xl == 'XL', 1, 0)
stock_data$gov <- ifelse(stock_data$date %in% ea)

# Dummy variable for earnings announcments
stock_data$earnings <- earnings[earnings$date %in% intersect(earnings$date, df2$date),]

# Looking at companies that have earnings report on a day with a conference on TV
# Load in data from Peer_companies -> MasterTest
high_distraction <- earnings[earnings$date %in% intersect(earnings$date, df2$date),]# Extract the common rows in the dataset
low_distraction <- earnings[!earnings$date %in% intersect(earnings$date, df2$date),]

# Comparing AV for companies on high and low distraction days
# Exctracting AV day of announcment
high_distraction <- high_distraction[,-2]
stock_data$Security <- gsub(" .*$", "", stock_data$Security, ignore.case = T)
high_distraction <- merge(high_distraction, stock_data, by=1:2)

high_distraction[1,1] == stock_data[1,1]

high_distraction[1,1:2] == stock_data[145,1:2]

# Exctracting AV k days after announcment 

# Calculating market return
# - weight each stock by market cap?
# - return of each stock
test <- stock_data %>% 
  group_by(date) %>%
  summarize(total_mkt_cap = sum(CUR_MKT_CAP, na.rm = T)) %>% ungroup %>%
  remove_missing() %>% 
  mutate(return = (total_mkt_cap - lag(total_mkt_cap)) / lag(total_mkt_cap)) # lag gives the previous value



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

# Plots -------------------------------------------------------------------


stock_data$rn <- log(stock_data$PX_LAST/lag(stock_data$PX_LAST))

stock_data <- stock_data[-1,]

stock_data$volatility_calc <- rollmeanr(stock_data$rn, k = 21, fill = NA)

mean(stock_data$rn[2:21])