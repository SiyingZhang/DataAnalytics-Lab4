library(ggplot2)

## Read csv file from the local directory
train <- read.csv("/Users/siying/Documents/edu/PITT/2015\ Fall/Data\ Analytics/Lab/DataAnalytics-Lab4/train.csv")

## Select survived people
trainSurvived <- train[which(train$Survived == 1),]
## To see the summary of age value
summary(trainSurvived$Age) # Min 0.42  Median 28.00  Mean28.34  Max 80.00

attach(trainSurvived)
## Use geom_histogram to present the count of the age of survived people 
(p <- ggplot(data = trainSurvived, aes(trainSurvived$Age)) + geom_histogram(breaks = seq(0, 80, by = 5), col = "white", aes(fill = ..count..), alpha = 0.7) +
	scale_fill_gradient("Count", low = "pink", high = "purple") +
	labs(title = "Age of survived people") + 
	labs(x = "Age", y = "Count") +
	ylim(c(0, 50))
)

