
library(ggplot2)

train <- read.csv("/Users/Abdulaziz/Desktop/lab4/train.csv")

train <- train[which(train$Age!= 'N/A'),] ## remove the available age

library(vioplot) ## downloading the viplot library

x1 <-train$Age[train$Embarked =='S']  ## assign the ages who departed from S city to x1 variable

x2 <- train$Age[train$Embarked == 'Q']

x3 <- train$Age[train$Embarked == 'C']


## using vioplot show the distribution between the ages and the three cities 

vioplot(x1, x2, x3, names=c("S", "Q", "C"),  
        col="purple")      

