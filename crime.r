getwd()
setwd("D:\\R")
disease
d <- read.csv("crime.csv", na.strings = c(""," ","NA"))
d<-d[!duplicated(d),]
library(Amelia)
missmap(d)
sapply(d,function(x) sum(is.na(x)))
x=na.omit(d)
d<-d[!names(d) %in% c("X")]
d
prop.table(table(d$year))
is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}
is_outlier
dput(colnames(d))
meanvar=c( "Group_Name", "Sub_Group_Name")
library(corrplot)

corrplot(cor(d[,names(d)%in% meanvar]),type="full",order="hclust",tl.cex = 1,tl.col = "Black",addrect = 8,method = "circle")
dput(colnames(disease))
SEvar=c( "Cases_Property_Recovered", "Cases_Property_Stolen")
library(corrplot)

corrplot(cor(d[,names(d)%in% SEvar]),type="full",order="hclust",tl.cex = 1,tl.col = "Black",addrect = 8,method = "circle")
worsevar=c(  "Value_of_Property_Recovered", "Value_of_Property_Stolen")
library(corrplot)

corrplot(cor(d[,names(d)%in% worsevar]),type="full",order="AOE",tl.cex = 1,tl.col = "Black",addrect = 8,method = "color",addCoef.col = "gray")
library("caret")
library("dplyr")
#Remove Highly correlated 
d1=d %>% select(-findCorrelation(cor(d %>% select(-id,-year)),cutoff=.8))
d1
