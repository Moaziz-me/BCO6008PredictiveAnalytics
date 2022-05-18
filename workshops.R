##Week1session2workshop

bird1<-read.csv("birds.csv") #upload file - make sure file is in same dir to the Rprogram-file
View(bird1) #view the data set to make a blue print of it in you current project

sex<-bird1$sex #assign sex column to a seperate value table "sex"
curlen<-bird1$curlen   #assign curlen column to a seperate value table "curlen"
sexcode<-ifelse(sex=="F", 1, 0) #code famale to 1 and male to 0 and give it 'sexcode' name
plot(curlen, jitter(sexcode, 0.15), pch=19, xlab="Bill length(mm)", ylab = "sex(0 - male, 1 - female)")
#plot "CURLEN" use "JITTER PLOT" plot sex code 0.15 show how disterbuted they are
#pch19 is the type of shape you want to us, then xy labs name.
model <- glm(sexcode~curlen, binomial()) #apply glm model to sexcode&curlen
xv<-seq(min(curlen), max(curlen), 0.01) #use on your regression model 'Curlen as min'
yv<-predict(model, list(curlen=xv), type="response") #predict your model
lines(xv, yv, col="green") #create a line

#week2workshop3

library(tidyverse)
?summarise
?group_by
diamonds
summarise(diamonds, mean(price))

diamonds%>%
  group_by(cut)%>%
  summarise(mean(price))

diamonds%>%
  group_by(cut)%>%
  summarise(avg_price=mean(price), avg_carat=mean(carat))

diamonds%>%
  group_by(carat>1, cut)%>%
  summarise(avg_price=mean(price))

diamonds%>%
  group_by(big=carat>1, cut)%>%
  summarise(avg_price=mean(price))

#week2Workshop4
install.packages("tidymodels", "dplyr", "recipes", "rsample", "modeldata")

library(recipes)
library(rsample)
library(modeldata)

data("credit_data")

set.seed(55)
train_test_split <- initial_split(credit_data)

credit_train <- training(train_test_split)
credit_test <- testing(train_test_split)


#week3workshop5
install.packages("MASS")
install.packages("ISLR")
install.packages("readxl")
library(MASS)
library(ISLR)
library(readxl)

#age<-read_excel('age.xls', sheet='Hoja2')
age<-read_excel('age.xls')

lmheight=lm(height~age, data=age)
summary(lmheight)

lmheight2 <- lm(height~age, data=age)
summary(lmheight2)

pressure <- read_excel('pressure.xlsx')
lmTemp<-lm(Pressure~Temperature, data = pressure)
plot(pressure, pch=17, col='blue')
abline(lmTemp)

item<-read_excel('item.xlsx')
lmItem<-lm(id~cost, data = item)
plot(item, pch=15, col="red")
abline(lmItem)


#bagging and boosting?

linear_reg(item)%>%
  set_engine()%>%
  translate()

?fit()
fit_xy()

##inclass activity:
#lda MASS predict(obj)
#glm stats predict(obj, type = "response")
#gbm bgm predict(obj, type = "response", n.trees)
#mda mda predict(obj, type = "response")
#rpart rpart predict(obj, type = "prob")
#weka Rweka predict(obj, type = "probability")

library(tidymodels)
reg_model <- linear_reg(penalty = 0.01)
reg_model

reg_model %>%
  set_engine("glmnet")%>%
  translate()

install.packages("glmnet")
library(glmnet)

#reg_model %>%
#  set_engine("glmnet")%>%
#  fit(price ~ ., data = item)

reg_model %>%
  set_engine("glmnet")%>%
  fit(mpg ~ ., data = mtcars)

reg_model %>%
  set_engine("glmnet")%>%
  fit(mpg ~ ., data = mtcars %>% slice(1:29))%>%
  predict(new_data = mtcars %>% slice(30:32))

reg_model %>%
  set_engine("glmnet")%>%
  fit_xy(x = mtcars %>% select(-mpg),
         y =mtcars$mpg)
view(mtcars)

#week2workshop6

#descriptive data analysis 
install.packages("xlsx")
library(xlsx)
data<-read.csv("commerce.csv")
print(data)
#header in the data file 
head(data)
#file structure
str(data)
min(data$ProductPrice)
max(data$ShippingFee)
max(data$ProductPrice) 
range(data$ProductPrice)
max(data$ProductPrice)-min(data$ProductPrice)
mean(data$ProductPrice)
median(data$ProductPrice)
quantile(data$ProductPrice) 
quantile(data$ProductPrice, 0.5)
quantile(data$ProductPrice, 0.75)
IQR(data$ProductPrice)
sd(data$ProductPrice)
summary(data)
View(data)
#CV
sd(data$ProductPrice)/mean(data$ProductPrice)
sort(data$OrderID)
cor(data$ProductPrice, data$ShippingFee)
barplot(data$ProductPrice, data$ShippingFee)
hist(data$ProductPrice)
boxplot(data$ProductPrice)

# Get the Data

# Read in with tidytuesdayR package 
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# Either ISO-8601 date or year/week works!

install.packages("tidytuesdayR")

tuesdata <- tidytuesdayR::tt_load('2020-07-14')
tuesdata <- tidytuesdayR::tt_load(2020, week = 29)

astronauts <- tuesdata$astronauts

# Or read in the data manually

astronauts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-14/astronauts.csv')

library(tidyverse)
install.packages("janitor")
library(janitor)
library(knitr)

astronauts%>% 
  clean_names() %>% 
  filter(!is.na(number)) %>%  # remove last row (all values are NA)
  mutate(
    sex = if_else(sex == "M", "male", "female"),
    military_civilian = if_else(military_civilian == "Mil", "military", "civilian")
  )

astronauts %>%
  mutate(year_of_mission = 10 * (year_of_mission %/% 10),
         year_of_mission = factor(year_of_mission))%>%
  ggplot(aes(year_of_mission, hours_mission,
             fill = year_of_mission, color = year_of_mission))+
  geom_boxplot(show.legend = F, alpha = 0.2, size = 0.5)+
  scale_y_log10()+
  labs(x = NULL, y = "Duration of mission in hours")

astronauts_df <- astronauts %>%
  select(name, mission_title, hours_mission,
         military_civilian, occupation, year_of_mission, in_orbit) %>%
  mutate(in_orbit = case_when(str_detect(in_orbit, "^Salyut") ~ "Salyut",
                              str_detect(in_orbit, "^STS") ~ "STS",
                              TRUE ~ in_orbit),
         occupation = str_to_lower(occupation))%>%
  filter(hours_mission > 0)%>%
  mutate(hours_mission = log(hours_mission)) %>%
  na.omit()

library(tidymodels)

set.seed(123)
astro_split <- initial_split(astronauts_df, strata = hours_mission)
astro_train <- training(astro_split)
astro_test <- testing(astro_split)

astro_recipe <- recipe(hours_mission ~ ., data = astro_train) %>%
  update_role(name, mission_title, new_role = "id") %>%
  step_other(occupation, in_orbit, threshold = 0.005) %>%
  step_dummy(all_nominal(), -has_role("id"))

astro_recipe %>% prep() %>% juice()

install.packages("baguette")
library(baguette)

astro_wf <- workflow() %>%
  add_recipe(astro_recipe)

tree_spec <- bag_tree() %>%
  set_engine("rpart", times = 25) %>%
  set_mode("regression")

mars_spec <- bag_mars() %>%
  set_engine("earth", times = 25) %>%
  set_mode("regression")

astro_wf %>%
  add_model(tree_spec)
