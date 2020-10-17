# Libraries
library(readxl)
library(tidyverse)
library(zoo)

# All data
stock_final <- read_excel("Stock_data/stock_final.xlsx")

# Fix problem with mismatching dates
df <- merge(stock_final[,1:2], stock_final[,-2], by=1:2, all.y = T) %>% 
  select(-c(EQY_SH_OUT))

# Change structure of variables from character to numeric
df[,3:7] <- sapply(df[,3:7],as.numeric)

# Change NAN to 0 in volume
df$PX_VOLUME[is.na(df$PX_VOLUME)] <- 0

# Calculate return 
df$daily_return <- (df$PX_LAST-df$PX_OPEN)/df$PX_OPEN

# Calculate abnormal volume

# Volume sum
#df$volume_sum <- rollsumr(df$PX_VOLUME, k = 30, fill = NA)


# Odeen method
df$AV_ODEEN <- df$PX_VOLUME/(rollsumr(df$PX_VOLUME, k = 30, fill = NA)/30)



# DellaVigna formula
df$AV_DV <- log(((df$PX_VOLUME + lead(df$PX_VOLUME))/2)+1) - log(((rollsumr(df$PX_VOLUME, k = 41, fill = NA)-rollsumr(df$PX_VOLUME, k = 11, fill = NA))/30)+1)


# Calculating market return
# - weight each stock by market cap?
# - return of each stock
test <- df %>% 
  group_by(Security) %>%
  summarize(mkt_cap_share = sum(CUR_MKT_CAP, na.rm = T)) %>% ungroup %>%
  remove_missing()

test$weight <- test$mkt_cap_share/sum(test$mkt_cap_share)
sum(test$avg_mkt_cap)

# Graph of trading volume over time
df %>% 
  group_by(date) %>% 
  summarize(total_volume = sum(PX_VOLUME)) %>% ungroup %>% 
  ggplot(., aes(x=date, y=total_volume/10^6)) +
  geom_line() +
  ggtitle("Total trading volume over time") +
  labs(y = "Million trades", x= "Date") +
  theme_bw()

# Graph of trading volume over time
# Differentiating between company size.

grouped_df <- df %>% 
  group_by(Security) %>%
  summarize(avg_mkt_cap = mean(CUR_MKT_CAP, na.rm = T)) %>%
  remove_missing() %>% 
  mutate(Size_Level = cut(avg_mkt_cap,
                           quantile(avg_mkt_cap, c(0, .50, .75, 1)),
                           labels = c('Small', 'Medium', 'Big'),
                           include.lowest = TRUE)) %>% 
  merge(.,df, by = "Security")  # idk if this is the most efficient way..

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
tot_vol <- df %>% 
  group_by(Security) %>% 
  summarize(total_volume = sum(PX_VOLUME))
  # No.. KOA has had most trades over the period.

tot_vol2 <- df %>% 
  group_by(Security) %>%
  summarize(avg_mkt_cap = mean(CUR_MKT_CAP, na.rm = T)) %>%
  remove_missing() %>% 
  mutate(Size_Level = cut(avg_mkt_cap,
                          quantile(avg_mkt_cap, c(0, .25, .75, 1)),
                          labels = c('Small', 'Medium', 'Big'),
                          include.lowest = TRUE)) %>% 
  merge(.,tot_vol, by = "Security")

