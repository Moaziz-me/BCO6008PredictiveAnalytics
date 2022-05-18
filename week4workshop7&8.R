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
