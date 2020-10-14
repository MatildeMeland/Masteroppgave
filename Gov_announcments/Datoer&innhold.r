# Load text file
raw <- readLines("Gov_announcments/Test.txt")

library(tidyverse)

# Create date by replacing words with numbers
raw <- raw %>% str_replace_all(c(" 2020" = "2020",
                          
                          " januar" = "01."," februar" = "02."," mars" = "03."
                          
                          ," april" = "04."," mai" = "05."," juni" = "06."
                          
                          ," juli" = "07."," august" = "08."," september" = "09."
                          
                          ," oktober" = "10."," november" = "11."," desember" = "12."))

# Data frame where each column contains seperate information, each row a new announcement
df <- as.data.frame(matrix(raw, ncol = 5, byrow = TRUE))

# Keep first 3 colums, only ones with relevant information
df <- df[,1:3]

# Change column names
colnames(df) <- c("date", "title", "content")

# Remove date from title
df[,2] <- gsub("\\(.+","",df[,2],ignore.case=T)

# Create date format
df$date <- as.Date(df$date,format="%d.%m.%Y")

# Some problems due to some announcements containing "les mer" and others do not.
# Some problems due to some articles not providing a link 
# Problems with encoding. Æ,Ø,Å returns "?" in UTF-8.

# These problems can be fixed with manual work
# Current file gives us the timeline.