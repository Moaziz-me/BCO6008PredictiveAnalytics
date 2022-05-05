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