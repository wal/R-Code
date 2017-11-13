require(tidyverse)


testing.dataset <- read.csv('datasets/titanic/test.csv')
training.dataset <- read.csv('datasets/titanic/train.csv')

head(testing.dataset)
head(training.dataset)

summary(training.dataset)
summary(testing.dataset)


alldata <- rbind(testing.dataset, training.dataset, fill=TRUE)



data(pima)