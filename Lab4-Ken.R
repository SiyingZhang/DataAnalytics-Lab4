library(ggplot2)
library(scales)
library(reshape)
library(plyr)
# inspired by: https://learnr.wordpress.com/2010/01/26/ggplot2-quick-heatmap-plotting/

## Read csv file from the local directory
train <- read.csv("./train.csv")

# Remove all observations with unknown Age
attach(train)
train <- train[ which(Age!='N/A'),]
detach(train)
train$Age<-as.numeric(train$Age)

# Generate ageRank used as the Y-axis variable.
# Make it like R code.
train$ageRank[train$Age<=5]<-'0-5'
train$ageRank[train$Age>5]<-'5-10'
train$ageRank[train$Age>10]<-'10-15'
train$ageRank[train$Age>15]<-'15-20'
train$ageRank[train$Age>20]<-'20-25'
train$ageRank[train$Age>25]<-'25-30'
train$ageRank[train$Age>30]<-'30-35'
train$ageRank[train$Age>35]<-'35-40'
train$ageRank[train$Age>40]<-'40-45'
train$ageRank[train$Age>45]<-'45-50'
train$ageRank[train$Age>50]<-'50-55'
train$ageRank[train$Age>55]<-'55-60'
train$ageRank[train$Age>60]<-'60-65'
train$ageRank[train$Age>65]<-'65-70'
train$ageRank[train$Age>70]<-'70-75'
train$ageRank[train$Age>75]<-'75-80'
train$ageRank[train$Age>80]<-'80-85'
train$ageRank[train$Age>85]<-'85-90'
train$ageRank[train$Age>90]<-'>90'


# Rearrange the dataframe by Age
# with http://www.statmethods.net/stats/withby.html 
# reorder: https://stat.ethz.ch/R-manual/R-patched/library/stats/html/reorder.factor.html
train$ageRank <- with(train, reorder(ageRank,Age))

# Generate new dataframe using columns selected.
myvars <- names(train) %in% c("ageRank","Survived","Pclass","SibSp","Parch","Fare") 
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
