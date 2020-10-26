# Libraries
library(readxl)
library(tidyverse)
library(zoo)


# Load and format data ----------------------------------------------------
stock_data <- read_excel("Stock_data/stock_final.xlsx") %>% as.data.frame()

# Fix problem with mismatching dates
stock_data <- 
  merge(stock_data[,-2],stock_data[,1:2], by=1:2, all.y = T) %>% 
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


# Calculate abnormal returns ----------------------------------------------

# Calculate return 
stock_data$daily_return <- (stock_data$PX_LAST-stock_data$PX_OPEN)/stock_data$PX_OPEN


# Calculate abnormal volume -----------------------------------------------

# Volume sum
#stock_data$volume_sum <- rollsumr(df$PX_VOLUME, k = 30, fill = NA)


# Odeen method
stock_data$AV_ODEEN <- stock_data$PX_VOLUME/(rollsumr(stock_data$PX_VOLUME, k = 30, fill = NA)/30)


# DellaVigna formula
stock_data$AV_DV <- log(((stock_data$PX_VOLUME + lead(stock_data$PX_VOLUME))/2)+1) - log(((rollsumr(stock_data$PX_VOLUME, k = 41, fill = NA)-rollsumr(stock_data$PX_VOLUME, k = 11, fill = NA))/30)+1)


# Calculating market return
# - weight each stock by market cap?
# - return of each stock
test <- stock_data %>% 
  group_by(date) %>%
  summarize(total_mkt_cap = sum(CUR_MKT_CAP, na.rm = T)) %>% ungroup %>%
  remove_missing() %>% 
  mutate(return = (total_mkt_cap - lag(total_mkt_cap)) / lag(total_mkt_cap)) # lag gives the previous value


#




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







