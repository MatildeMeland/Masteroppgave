raw <- readLines("Gov_announcments/Test.txt")

raw <- raw %>% str_replace_all(c(" 2020" = "2020",
                          
                          " januar" = "01."," februar" = "02."," mars" = "03."
                          
                          ," april" = "04."," mai" = "05."," juni" = "06."
                          
                          ," juli" = "07."," august" = "08."," september" = "09."
                          
                          ," oktober" = "10."," november" = "11."," desember" = "12."))

df <- as.data.frame(matrix(raw, ncol = 7, byrow = TRUE))

df <- df[,1:3]

colnames(df) <- c("date", "title", "content")

df[,2] <- gsub("\\(.+","",df[,2],ignore.case=T)

df$date <- as.Date(df$date,format="%d.%m.%Y")

