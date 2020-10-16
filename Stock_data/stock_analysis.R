
d <- data.frame(x = c("A", "B", "C", "D", "E", "F"), 
                y = c("D", "E", "F", NA, NA, NA),
                z = c(10, 20, 30, NA, NA, NA))

# merge two dataframes by date
a <- d[1]
b <- d[-1]

colnames(a) <- ("date")
colnames(b) <- c("dates", "price")

df <- merge(b, a, by.x = "dates", by.y = "date", all.y = T)

library(readxl)
stock_final <- read_excel("Stock_data/stock_final.xlsx")

stock_test <- stock_final[grep("AKBMME", stock_final$Security, ignore.case = T), ]

a <- stock_test[,1:2]
b <- stock_test[,-1:-2]

df <- merge(b, a, by.x = "formula", by.y = "date", all.y = T)

df <- df[,c(8,1,2,3,4,5,6,7)]

