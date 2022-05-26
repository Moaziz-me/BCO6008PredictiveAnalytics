#week4workshop7and8

install.packages("party")
install.packages("partykit")
library(party)
library(partykit)

mydata <- read_csv("wine.csv")

set.seed(1234)
ndata <- sample(2, nrow(mydata), replace = T, prob = c(0.7, 0.3))
train.data <- mydata[ndata==1,]
test.data <- mydata[ndata==2,]

myv <- Type ~ Alcohol + Ash + Ash_Alcalinity + Flavanoids + Proline

wine_ctree <- ctree(myv, data = train.data)
table(predict(wine_ctree), train.data$Type)
plot(wine_ctree)

testpred <- predict(wine_ctree, newdata = test.data)
table(testpred, test.data$Type)
plot(testpred)


#workshop 8

#install.packages("randomForest")
library(randomForest)
library(dplyr)

mydata=iris
view(mydata)
str(mydata)

index=sample(2, nrow(mydata), replace = T, prob = c(0.7, 0.3))
train=mydata[index==1,]
test=mydata[index==2,]

tb=table(train$Species, train$Sepal.Length)
tb


rfModel=randomForest(Species~.,data = train)
Species_pred=predict(rfModel, train)
train$Species_pred=Species_pred
View(train)

rfModel

CM=table(train$Species, train$Species_pred)
CM

CM1=table(train$Species, train$Petal.Length)
CM1


tuesdata <- tidytuesdayR::tt_load(2020, week = 5)

sf_trees <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-28/sf_trees.csv')


library(tidyverse)
library(here)
library(tidytuesdayR)
library(pryr)
library(visdat)
library(skimr)
library(lubridate)
library(leaflet)
install.packages("tidytuesdaymeta")

#create_tidytuesday_folder()

raw_df <- read_csv(here::here("2020", "2020-01-28", "Street_Tree_Map.csv"),
                   col_types = 
                     cols(
                       TreeID = col_double(),
                       qLegalStatus = col_character(),
                       qSpecies = col_character(),
                       qAddress = col_character(),
                       SiteOrder = col_double(),
                       qSiteInfo = col_character(),
                       PlantType = col_character(),
                       qCaretaker = col_character(),
                       qCareAssistant = col_character(),
                       PlantDate = col_character(),
                       DBH = col_double(),
                       PlotSize = col_character(),
                       PermitNotes = col_character(),
                       XCoord = col_double(),
                       YCoord = col_double(),
                       Latitude = col_double(),
                       Longitude = col_double(),
                       Location = col_character()
                     )) %>% 
  janitor::clean_names()

small_df <- raw_df %>% 
  select(-x_coord,-y_coord,-q_care_assistant, -permit_notes) %>% 
  filter(plant_type != "Landscaping") %>% 
  select(-plant_type) %>% 
  separate(plant_date, into = c("date", "time"), sep = " ") %>% 
  mutate(date = parse_date(date, "%m/%d/%Y")) %>% 
  select(-time, -location) %>% 
  arrange(date) %>% 
  rename(legal_status = q_legal_status,
         species = q_species,
         address = q_address,
         site_info = q_site_info,
         caretaker = q_caretaker)

small_df %>% skimr::skim()

small_df %>% 
  write_csv(here::here("2020", "2020-01-28", "sf_trees.csv"))