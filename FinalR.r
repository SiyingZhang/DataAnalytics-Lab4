### Done By • Siying Zhang (siying.zhang@pitt.edu);  Chukun Xia(CHX26@pitt.edu); Abdulaziz Almuzaini (aaa169@pitt.edu);  Yue Li (YUL134@pitt.edu); Abhishek Mukherjee (ABM84@pitt.edu)  


#### Downloading the required libraries
library(ggplot2)
library(scales)
library(reshape)
library(vioplot)
library(plyr)


## Read csv file from the local directory
train <- read.csv("./train.csv")


# Remove all observations with unknown Age
attach(train)
train <- train[ which(Age!='N/A'),]

## assign the ages who departed from  the three different cities to x variables
x1 <-train$Age[train$Embarked =='S']  
x2 <- train$Age[train$Embarked == 'Q']
x3 <- train$Age[train$Embarked == 'C']


## Select age of survived people
trainSurvived <- train[which(train$Survived == 1),]

## To see the summary of age value
summary(trainSurvived$Age) # Min: 0.42  Median: 28.00  Mean: 28.34  Max: 80.00





################# Heatmap ######################
################################################

detach(train)

train$Age<-as.numeric(train$Age)


# Generate ageRank used as the Y-axis variable.
for(i in 1:length(train$Age)){
  if(train$Age[i]<5) train$ageRank[i]<-'0-5'
  else if(train$Age[i]<10) train$ageRank[i]<-'5-10'
  else if(train$Age[i]<15) train$ageRank[i]<-'10-15'
  else if(train$Age[i]<20) train$ageRank[i]<-'15-20'
  else if(train$Age[i]<25) train$ageRank[i]<-'20-25'
  else if(train$Age[i]<30) train$ageRank[i]<-'25-30'
  else if(train$Age[i]<35) train$ageRank[i]<-'30-35'
  else if(train$Age[i]<40) train$ageRank[i]<-'35-40'
  else if(train$Age[i]<45) train$ageRank[i]<-'40-45'
  else if(train$Age[i]<50) train$ageRank[i]<-'45-50'
  else if(train$Age[i]<55) train$ageRank[i]<-'50-55'
  else if(train$Age[i]<60) train$ageRank[i]<-'55-60'
  else if(train$Age[i]<65) train$ageRank[i]<-'60-65'
  else if(train$Age[i]<70) train$ageRank[i]<-'65-70'
  else if(train$Age[i]<75) train$ageRank[i]<-'70-75'
  else if(train$Age[i]<80) train$ageRank[i]<-'75-80'
  else if(train$Age[i]<85) train$ageRank[i]<-'80-85'
  else if(train$Age[i]<90) train$ageRank[i]<-'85-90'
  else train$ageRank[i]<-'>=90'
}


# Rearrange the dataframe by Age
# with http://www.statmethods.net/stats/withby.html 
# reorder: https://stat.ethz.ch/R-manual/R-patched/library/stats/html/reorder.factor.html
train$ageRank <- with(train, reorder(ageRank,Age))



# Generate new dataframe using columns selected.
myvars <- names(train) %in% c("Age","ageRank","Survived","Pclass","SibSp","Parch","Fare") 
train.new <- train[myvars]



# Generate scale needed in heatmap
# melt: http://www.statmethods.net/management/reshape.html
train.m<-melt(train.new)

# ddply: http://www.inside-r.org/packages/cran/plyr/docs/ddply
# transform: https://stat.ethz.ch/R-manual/R-devel/library/base/html/transform.html
# rescale: http://www.inside-r.org/packages/cran/scales/docs/rescale
train.m <- ddply(train.m, .(variable), transform, rescale = scale(value))
(p <- ggplot(train.m, aes(variable, ageRank)) + geom_tile(aes(fill = rescale), colour = "white") + scale_fill_gradient(low = "white", high = "steelblue"))



# Optimize the heatmap
# theme_grey: http://docs.ggplot2.org/current/theme_grey.html
# scale_x_discrete: http://docs.ggplot2.org/current/scale_discrete.html
# theme: http://docs.ggplot2.org/current/theme.html
base_size <- 12
p + theme_grey(base_size = base_size) + labs(x = "", y = "") + scale_x_discrete(expand = waiver()) +
  scale_y_discrete(expand =  waiver()) + 
  theme(legend.position = "none", axis.ticks = element_blank(), axis.text.y = element_text(colour = "grey50"))



################## Historgram ########################
######################################################

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

############ make a graph counting the number of male and female survived ##########
ggplot(data = trainSurvived, aes(trainSurvived$Sex)) + 
  geom_histogram(breaks = seq(0, 80, by = 5), col = "white", aes(fill = ..count..), alpha = 0.7) +
  scale_fill_gradient("Count", low = "pink", high = "purple") + ##set the view of the graph
  labs(title = "sex of survived people") + ##add title of the graph
  labs(x = "sex", y = "Count") + 
  ylim(c(0, 250)) ## set range of y axis



################### Violin Plot  ######################
#######################################################

## using vioplot show the distribution between the ages and the three cities 
vioplot(x1, x2, x3, names=c("S", "Q", "C"),  
        col="purple")




##################  Facet Grid #####################
####################################################


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




#########################  Whisker-plot  ########################
#################################################################


########## make a graph showing the relation of passenger class and survived ###########
train$Survived=factor(train$Survived, labels = c("died","survived"))##change "0" and "1" to "died" to "survived"
boxplot(train$Pclass~train$Survived, data=train, xlab="survived or not", ylab="Passenger class", main="relation between survive and pclass")
