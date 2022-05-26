library(dplyr)
library(tidyverse)
View(mtcars)

distance_mat <- dist(mtcars, method = "euclidean")
distance_mat

set.seed(123)
hier_cl<-hclust(distance_mat, method = "average")
hier_cl

plot(hier_cl)
abline(hier_cl, col="red")
fit<-cutree(hier_cl, k=3)
fit

table(fit)
rect.hclust(hier_cl, k=3, border="red")


library(ggplot2)

df<-data.frame(age = c(18, 21, 22, 24, 26, 26, 27, 30, 31, 35, 39, 40, 41, 42, 44, 46, 47, 48, 49, 54),
               spend = c(10, 11, 22, 15, 12, 13, 14, 33, 39, 37, 44, 27, 29, 20, 28, 21, 30, 31, 23, 24)
)
ggplot(df, aes(x = age, y = spend)) +
  geom_point()
