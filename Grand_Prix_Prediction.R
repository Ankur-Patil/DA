library(htmltab)
url <- "https://www.racefans.net/2016-f1-season/statistics/strategy-pit-stops/"
tyres <- htmltab(doc=url, which = 1)
show(tyres)

install.packages('rvest')
library('rvest')
url <- "https://www.racefans.net/2016-f1-season/statistics/strategy-pit-stops/"
webpage <- read_html(url)

library(rvest)
install.packages('tidyverse')
library(tidyverse)

h <- read_html("https://www.racefans.net/2016-f1-season/statistics/strategy-pit-stops/")

reps <- h %>%
  html_node("#mw-content-text > div > table:nth-child(18)") %>%
  html_table()

reps <- reps[,c(1:2,4:9)] %>% as_tibble()

library(rvest)
url <- "https://www.racefans.net/2018-f1-season/2018-f1-statistics/2016-f1-race-data/"
page <- read_html(url)
table <- html_table(page, header = TRUE ,fill = TRUE)

View(table)
View(table[[1]])
View(data)
data = table[[1]] 
write.csv(table[[1]], "C:/Users/Ankur/Desktop/Lapperpos2018.csv")            
