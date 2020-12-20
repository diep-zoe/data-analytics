# -----------------------------------------------------------------------------------------------
# Course: MANAGEMENT ANALYTICS FOUNDATIONS II
# Module: Machine Learning II
# Lecturer: Prof. Dr. JosE Parra-Moyano
# -----------------------------------------------------------------------------------------------
rm(list=ls())
#Install the keras R package
#install.packages("keras")
#install.packages("tidyverse")

# Load in the keras package
library(keras)
library(corrplot)
library(tensorflow)
library(tidyverse)
library(datasets)

# Read in `iris` data
rm(list=ls())
iris <- read.csv(url("http://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data"), header = FALSE) 

iris[,5]<-ifelse(iris[,5]=="Iris-setosa","Healthy",iris[,5])
iris[,5]<-ifelse(iris[,5]=="Iris-versicolor","Little Water",iris[,5])
iris[,5]<-ifelse(iris[,5]=="Iris-virginica","Much Water",iris[,5])

# Return the first part of `iris`
head(iris)

# Obtain the dimensions
dim(iris)

names(iris) <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Status")

# Boxplot
boxplot(iris$Sepal.Length ~ iris$Status)

# Plot
mycolor <- c("red","green3","blue")[as.factor(iris$Status)]
plot(iris$Petal.Length, iris$Petal.Width, pch = 8, col = mycolor, main = "Edgar Anderson's iris Data", xlab = "Petal Length", ylab = "Petal Width", xlim = c(0,7), ylim= c(0,3))
legend('topleft', legend = unique(iris$Status), col = c("green3","red","blue"), pch = 8, bty = 'n')

# Overall correlation between `Petal.Length` and `Petal.Width` 
cor(iris$Petal.Length, iris$Petal.Width)
# 0.96

# Store the overall correlation in `M`
M <- cor(iris[,1:4])

# Plot the correlation plot with `M`
corrplot(M, method="circle")

# Pull up a summary of `iris`
summary(iris)

# Inspect the structure of `iris`
str(iris)


# -----------------------------------------------------------------------------------
# Data Scientist Jobs ;-)
# -----------------------------------------------------------------------------------

data(iris)

iris[,5] <- as.numeric(iris[,5]) -1

# Turn `iris` into a matrix
iris <- as.matrix(iris)

# Set iris `dimnames` to `NULL`
dimnames(iris) <- NULL

# Determine sample size
ind <- sample(2, nrow(iris), replace=TRUE, prob=c(0.67, 0.33))

# Split the `iris` data
iris.training <- iris[ind==1, 1:4]
iris.test <- iris[ind==2, 1:4]

# Split the class attribute
iris.trainingtarget <- iris[ind==1, 5]
iris.testtarget <- iris[ind==2, 5]

# One hot encode training target values
iris.trainLabels <- to_categorical(iris.trainingtarget)

# One hot encode test target values
iris.testLabels <- to_categorical(iris.testtarget)

# Print out the iris.testLabels to double check the result
print(iris.testLabels)


#-------------------------------------------
# Construct the model
#-------------------------------------------

# Initialize a sequential model
model <- keras_model_sequential() 

# Add layers to the model
model %>% 
  layer_dense(units = 16, activation = 'relu', input_shape = c(4)) %>% 
  layer_dense(units = 9, activation = 'relu') %>% #Hidden layer
  layer_dense(units = 3, activation = 'softmax')

# Print a summary of a model
summary(model)

# Get layer configuration
get_layer(model, index = 1)

# Compile the model
model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = 'adam',
  metrics = 'accuracy'
)

# Store the fitting history in `history` 
history <- model %>% fit(
  iris.training, 
  iris.trainLabels, 
  epochs = 30,
  batch_size = 4, 
  validation_split = 0.2
)

# Plot the history
plot(history)

# Predict the classes for the test data
classes <- model %>% predict_classes(iris.test, batch_size = 128)

# Evaluate on test data and labels
score <- model %>% evaluate(iris.test, iris.testLabels, batch_size = 128)

# Print the score
print(score)
# loss 0.20 & accuracy 0.89
# ---------------------------------------------------------------------------
# Logistic Model
# ---------------------------------------------------------------------------

# Create a data frame. Attention: V4 is the outcome (i.e. more than 100K assets for the bank)
d<-as.data.frame(cbind(iris.training,iris.trainLabels))

summary(d)

dreal<-as.data.frame(cbind(iris.test,iris.testLabels))

# Creat a probit model using the dataset d
myprobit <- glm(V5 ~ V1 + V2 +V3 + V4, family = binomial(link = "probit"), maxit = 100, data = d)
summary(myprobit)

# Manually set a threhold
prob<-predict(myprobit)
prob

pred<-ifelse(prob>1000000,1,0)
pred

error<-sum(d$V5-pred)
error
















