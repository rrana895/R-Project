
#### Knowledge Discovery and Data Mining (CS 513A)####

#Final Project

# Course  : CS 513 - A

#Name :: Haijun HUnag
#Id :10432867

#Name:: Rachi Rana
#Id : 10455300


####### *************************************************** ########



rm(list=ls())

house_info <-
  read.csv(file = "F:/SIT/4th term/cs-513/finalProject/kc_house_data.csv",
           na.strings = "0")


#data preprocessing

#drop useless columns
house_info <- house_info[,c(-1,-2,-9,-10,-14,-16,-17,-18,-19)]

#delete the row which has N/A
new_house_info <- house_info[complete.cases(house_info),]
new_house_info <- na.omit(house_info)

#make sure there is no NA in matrix
sum(is.na(new_house_info))

#house price partition sections
parts <- function(x,minx,maxx) {
  z <- x/(max(x)-min(x))*30
  return(round(z))
}

#normalized the matrix
mmnorm <- function(x,minx,maxx) {
  z <- (x - min(x))/(max(x)-min(x))
  return(z)
}
house_data <- as.data.frame(
  cbind(parts(new_house_info[,1]),
        mmnorm(new_house_info[,2]),
        mmnorm(new_house_info[,3]),
        mmnorm(new_house_info[,4]),
        mmnorm(new_house_info[,5]),
        mmnorm(new_house_info[,6]),
        mmnorm(new_house_info[,7]),
        mmnorm(new_house_info[,8]),
        mmnorm(new_house_info[,9]),
        mmnorm(new_house_info[,10]),
        mmnorm(new_house_info[,11]),
        mmnorm(new_house_info[,12]))
)

names(house_data) <- c("price","bedrooms","bathrooms","sqft_living","sqft_lot","floors",
                       "condition","grade","sqft_above","yr_built","sqft_living15","sqft_lot15")
View(house_data)

idx <- seq(1,nrow(house_data),by=4)
training <- house_data[-idx,]
test <- house_data[idx,]


#Imlenment

#knn method
library(kknn)
predict_k <- kknn(formula = as.factor(price)~., training, test,
                  k=30, kernel = "rectangular")
fit <- fitted(predict_k)

table(predict=fit, real=test$price)
knn_error_rate=sum(fit!=test$price)/length(test$price)
knn_error_rate

#Na??ve Bayes
library(e1071)
library(class)
nBayes <- naiveBayes(factor(price)~., data=training)
category_all <- predict(nBayes,test)
NB_wrong <- sum(category_all!=test$price)
NB_error_rate <- NB_wrong/length(category_all)
NB_error_rate

#Cart
library(rpart)
dev.off()
cart_class <- rpart(formula = factor(price)~.,data=training)
cart_predict <- predict(cart_class,test,type="class")
error_rate <- sum(cart_predict!=test$price)/length(cart_predict)
error_rate

#C50
library("C50")
set.seed(123)
C50_class <- C5.0(formula=factor(price)~.,data=training)
C50_predict <- predict(C50_class,test,type="class")
error_rate<-sum(C50_predict!=test$price)/length(C50_predict)
error_rate

#Random forest
library(randomForest)
set.seed(123)
fit <- randomForest(factor(price)~.,data=training,ntree=1000)
rf_predict <- predict(fit,test)
error_rate <- sum(rf_predict!=test$price)/length(rf_predict)
error_rate

