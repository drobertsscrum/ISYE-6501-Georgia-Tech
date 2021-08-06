# add kernlab 
library(kernlab)
# Set the working directory
setwd("~/Documents/ISYE6501 Intro to Analytics Modeling")
# Read the data in
matrix1 <- read.table("credit_card_data-headers.csv",header = TRUE, sep =",")
x <- as.matrix(matrix1[,1:10])
y <- as.factor(matrix1[,11])

modelvanilla <- ksvm(x,y,type="C-svc",kernel="vanilladot",C=100,scaled=TRUE)
modelvanilla
# calculate a1…am
a <- colSums(modelvanilla@xmatrix[[1]] * modelvanilla@coef[[1]])
a
# calculate a0
a0 <- modelvanilla@b
a0
# see what the model predicts
pred <- predict(modelvanilla,matrix1[,1:10])
pred
# see what fraction of the model’s predictions match the actual classification
sum(pred == matrix1[,11]) / nrow(matrix1)


#Change the value of C for vanilladot
modelCvanilla <- ksvm(x,y,type="C-svc",kernel="vanilladot",C=.0000001,scaled=TRUE)
modelCvanilla
# calculate a1…am
a <- colSums(modelCvanilla@xmatrix[[1]] * modelCvanilla@coef[[1]])
a
# calculate a0
a0 <- modelCvanilla@b
a0
# see what the model predicts
pred <- predict(modelCvanilla,matrix1[,1:10])
pred
# see what fraction of the model’s predictions match the actual classification
sum(pred == matrix1[,11]) / nrow(matrix1)

#use a Gaussian kernel to determine the difference
modelrbfdot <- ksvm(x,y,type="C-svc",kernel="rbfdot", C = 100, scaled=TRUE)
modelrbfdot
# calculate a1…am
a <- colSums(modelrbfdot@xmatrix[[1]] * modelrbfdot@coef[[1]])
a
# calculate a0
a0 <- modelrbfdot@b
a0
# see what the model predicts
pred <- predict(modelrbfdot,matrix1[,1:10])
pred
# see what fraction of the model’s predictions match the actual classification
sum(pred == matrix1[,11]) / nrow(matrix1)

# use the kknn model for k nearest neighbor
# example is just the base knn code

library(kknn)
#create a function for creating the data for prediction
i = 250
# I used k = 25 as the square root of 654
modelknn = kknn(R1~.,
                matrix1[-i,],
                matrix1[i,],
                k=25,
                distance=2,
                kernel = "optimal",
                scale = TRUE)
fitted.values(modelknn)
matrix1[i,11]

#next attempt is to create a loop
library(kknn)
setwd("~/Documents/ISYE6501 Intro to Analytics Modeling")
# Read the data in
matrix1 <- read.table("credit_card_data-headers.csv",header = TRUE, sep =",")
numberofobservations <- nrow(matrix1)
pred <- vector(length = numberofobservations)
for (i in 1:numberofobservations) {
  modelknn = kknn(R1~.,
                  matrix1[-i,],
                  matrix1[i,],
                  k=25,
                  distance=2,
                  kernel = "optimal",
                  scale = TRUE)
  pred[i] <- fitted.values(modelknn)
}
pred
for (i in 1:numberofobservations){
  cat("Observation:",i, "Value", matrix1[i,11], "\n")
}

#next code is to evaluate k from 1 to 25 and plot the values
library(kknn)
setwd("~/Documents/ISYE6501 Intro to Analytics Modeling")
# Read the data in
matrix1 <- read.table("credit_card_data-headers.csv",header = TRUE, sep =",")
numberofobservations <- 25
pred <- vector(length = numberofobservations)
for (i in 1:numberofobservations) {
  #model.knn <- train.kknn(matrix1[-i,11]~., data=data[-i,1:10], kmax=7, scale=T)
  modelknn = kknn(R1~.,
                  matrix1[-i,],
                  matrix1[i,],
                  k=25,
                  distance=2,
                  kernel = "optimal",
                  scale = TRUE)
  pred[i] <- fitted.values(modelknn)
}
pred
plot(pred)
max(pred)
min(pred)

#next attempt is to create a function that can be called to 
#provide multiple i and k values
create_knnmodel = function(X){
#create a vector of zeros to store the kernel data  
  
  predicted_data <- rep(0,(nrow(matrix1)))

for(i in 1: nrow(matrix1)){
modelknn = kknn(R1~.,
                matrix1[-i,],
                matrix1[i,],
                k=25,
                distance=2,
                kernel = "optimal",
                scale = TRUE)

predicted_data[i] <- as.integer(fitted.values(modelknn) + 0.5)
matrix1[i,11]
}

accumulated_data = sum(predicted_data == matrix1[,11]) / nrow(matrix1)
return(accumulated_data)
}

#create a vector of values for predictions. Use 25 based on the square root of 654
calling_vector = rep(0,25)
for (X in 1:25){
  calling_vector[X] = create_knnmodel(x)
}

plot(calling_vector)
title("Week 1 2.2.3 Homework K-Nearest-Neighbors")

max(calling_vector)
min(calling_vector)

