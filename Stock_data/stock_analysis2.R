
d <- data.frame(x = c("A", "B", "C", "D", "E", "F"), 
                y = c("D", "E", "F", NA, NA, NA),
                z = c(10, 20, 30, NA, NA, NA))

# merge two dataframes by date
a <- d[1]
b <- d[-1]

colnames(a) <- ("date")
colnames(b) <- c("dates", "price")

df <- merge(b, a, by.x = "dates", by.y = "date", all.y = T)

# Testing with a subset of data
library(readxl)
stock_final <- read_excel("Stock_data/stock_final.xlsx")

stock_test1 <- stock_final[grep("AKBMME", stock_final$Security, ignore.case = T), ]
stock_test2 <- stock_final[grep("ANDFME", stock_final$Security, ignore.case = T), ]

stock_test <- rbind(stock_test1, stock_test2)

a <- stock_test[,1:2]
b <- stock_test[,-2]

df <- merge(b, a, by.x = c("formula", "Security"), by.y = "date", all.y = T)

df <- merge(b, a, by=1:2, all.y = T)

# All data
library(readxl)
stock_final <- read_excel("Stock_data/stock_final.xlsx")

subset1 <- stock_final[,1:2]
subset2 <- stock_final[,-2]

df <- merge(subset2, subset1, by=1:2, all.y = T)

