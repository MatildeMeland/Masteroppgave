# library
library(readxl)
library(tidyverse)

# loading dataset
NRK <- read_excel("NRK/Artikler NRK.no oktober 2019-september 2020.xlsx")

NRK <- NRK[1:200,]

# Can remove the first columns that describes which "bin" in total readership an articles is in.

NRK <- NRK %>% select(-one_of(c("1000+ ord", "Pageviews (bin)")))
