
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

a <- stock_final[1:5000,2]
b <- stock_final[1:5000,-2]

colnames(b)[1] <- ("date")

df <- merge(b, a, by.x = "formula", by.y = "date", all.x = T)

df_test <- unique(df)

