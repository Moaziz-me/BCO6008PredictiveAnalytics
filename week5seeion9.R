newdata=iris
View(newdata)
str(newdata)

table (newdata$Species)


head(newdata)

set.seed(9850)

group<-runif(5)
group

group<-runif(nrow(newdata))
group

newdata1<-newdata[order(group),]

head(newdata1)

summary(newdata1[,c(1,2,3,4)])

normalize<-function(x){
  
  +return((x - min(x))/(max(x) - min(x))) }

normalize(c(1,2,3,4,5))


normalize(c(20,30,40,50,60))

newdata2<-as.data.frame(lapply(newdata1[, c(1,2,3,4)], normalize))
str(newdata2)

summary(newdata2)


#TidyTuesday data

# Get the Data

# Read in with tidytuesdayR package 
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# Either ISO-8601 date or year/week works!

tuesdata <- tidytuesdayR::tt_load('2021-02-23')
tuesdata <- tidytuesdayR::tt_load(2021, week = 9)

employed <- tuesdata$employed

# Or read in the data manually

employed <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-23/employed.csv')

earn <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-23/earn.csv')

#basic url
# https://www.bls.gov/cps/aa2020/cpsaat17.xlsx

library(tidyverse)
library(glue)


get_bls_report <- function(year){
  
  report_url <- glue::glue("https://www.bls.gov/cps/aa{year}/cpsaat17.xlsx")
  
  download.file(report_url, destfile = glue("2021/2021-02-23/bls-{year}.xlsx"))
}

ex_2019 <- readxl::read_excel("2021/2021-02-23/bls-2019.xlsx")

2015:2019 %>% 
  walk(get_bls_report)

# 2020 has no year in front of it
download.file(
  "https://www.bls.gov/cps/cpsaat17.xlsx", 
  destfile = "2021/2021-02-23/bls-2020.xlsx"
)

# Raw BLS -----------------------------------------------------------------