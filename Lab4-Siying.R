library(ggplot2)

## Read csv file from the local directory
train <- read.csv("/Users/siying/Documents/edu/PITT/2015\ Fall/Data\ Analytics/Lab/DataAnalytics-Lab4/train.csv")

########################
#Age of Survived People#
########################
## Select survived people
trainSurvived <- train[which(train$Survived == 1),]
## To see the summary of age value
summary(trainSurvived$Age) # Min: 0.42  Median: 28.00  Mean: 28.34  Max: 80.00

head(trainSurvived)
attach(trainSurvived)
## Use geom_histogram to present the count of the age of survived people 
(p0 <- ggplot(data = trainSurvived, aes(trainSurvived$Age)) + 
	geom_histogram(breaks = seq(0, 80, by = 5), col = "white", aes(fill = ..count..), alpha = 0.7) +
	scale_fill_gradient("Count", low = "pink", high = "purple") +
	labs(title = "Age of survived people") + 
	labs(x = "Age", y = "Count") +
	ylim(c(0, 50))
)


###########################################################
#Connections Between Age & Fare under different Sex&Pclass#
###########################################################
head(train)
attach(train)

## To figure out the summary of Fare value
summary(train$Fare) # Min: 0.00 Median: 14.45  Mean: 32.20  Max: 512.30 

(p1 <- ggplot(data = train, aes(x = Age, y = Fare)) + 
	geom_point(aes(color = SibSp)) + 
	labs(title = "Connections between Age & Fare") +
	ylim(c(0, 400)) +
	facet_grid(Sex ~ Pclass)
)