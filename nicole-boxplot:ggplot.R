> library(ggplot2)
> train <- read.csv("/Users/yueli/DataAnalytics-Lab4/train.csv")
##find survived people 
> trainSurvived <- train[which(train$Survived == 1),]

############ make a graph counting the number of male and female survived ##########
ggplot(data = trainSurvived, aes(trainSurvived$Sex)) + 
+     geom_histogram(breaks = seq(0, 80, by = 5), col = "white", aes(fill = ..count..), alpha = 0.7) +
+     scale_fill_gradient("Count", low = "pink", high = "purple") + ##set the view of the graph
+     labs(title = "sex of survived people") + ##add title of the graph
+     labs(x = "sex", y = "Count") + 
+     ylim(c(0, 250)) ## set range of y axis

########## make a graph showing the relation of passenger calss and survived ###########
train$Survived=factor(train$Survived, labels = c("died","survived"))##change "0" and "1" to "died" to "survived"
> boxplot(train$Pclass~train$Survived, data=train, xlab="survived or not", ylab="Passenger class", main="relation between survive and pclass")