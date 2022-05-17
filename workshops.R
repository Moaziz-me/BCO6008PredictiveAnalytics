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

install.packages("xlsx")
library(xlsx)

data<-read_excel('commerce.xlsx')
