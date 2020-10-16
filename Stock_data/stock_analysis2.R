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





