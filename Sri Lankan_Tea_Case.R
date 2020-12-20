#####################################################################
# SRI LANKAN TEA CASE
#####################################################################
# TEAM MEMBERS
# Diep Tran Ngoc, Philipp Rotter, Moritz

library(rpart)
library(tree)
library(ISLR)
library(partykit)
library(titanic)

# Import the tea dataset
d <- tea

# Understand the data
summary(d)
d <- na.omit(d)

# Set a train and test data set
set.seed(2)
train=sample(1:nrow(d),3*nrow(d)/4)
test=-train
test

training_data=d[train,]
testing_data=d[test, ]
testing_Pclass=testing_data$Pclass[test]

# plot
mycolor <- c("red","green3","blue")[as.factor(d$class)]
plot(d$stem, d$area, pch = 8, col = mycolor, main = "Tea Data", xlab = "Tea leaf stem", ylab = "Tea leaf area")
legend('topleft', legend = unique(d$class), col = c("green3","red","blue"), pch = 8, bty = 'n', cex=0.5)



####################### Model Training #######################

# Change the dependent variable to a factor
d$class<-as.factor(d$class) 
# indicate that this is not a numerical scala => factor!
summary(d$class)

# Fit the tree model using training data
d.rpart=rpart(class~ area+perimeter+compactness+length+width+asymmetry+stem, d, subset=train)

names(d)
plotcp(d.rpart) 


# Identify the value of the complexity parameter that produces
# the lowest CV error
cp.min <- d.rpart$cptable[which.min(d.rpart$cptable[,"xerror"]),"CP"]
cp.min
# Prune using the CV error minimizing choice of the complexity parameter cp
d.rpart.pruned <- prune(d.rpart, cp = cp.min)

# Convert pruned tree to a party object
d.party <- as.party(d.rpart.pruned)

d.party

# Plot
plot(d.party)

####################### Accuracy measurement #######################
# Accuracy of the best model (ratio of correct guesses)
predictions<-predict(d.party,newdata = testing_data)
predictions==testing_data$class
correct_rate<-(sum(ifelse(predictions==testing_data$class,1,0)))/nrow(testing_data)
correct_rate
# 0.88

# Plot a confusion matrix. This shows all the predictions in the test set that went actually wrong:
table(predictions)
table(predictions, testing_data$class)

###################################################################################
# Random Forest
###################################################################################

#rm(list = ls())


library(randomForest)
library(MASS)
library(MLmetrics)
library(ipred)
library(tree)

# This functions will later yield the RMSE:
#######################################################################
rmse_reg <- function(model_obj, testing = NULL, target = NULL) {
  #Calculates rmse for a regression decision tree
  #Arguments:
  # testing - test data set
  # target  - target variable (length 1 character vector)
  yhat <- predict(model_obj, newdata = testing)
  actual <- testing[[target]]
  sqrt(mean((yhat-actual)^2))
}

set.seed(1) 
# each tree gets access to no. of variables - 1
rf.tea <- randomForest(class ~ ., data = train, mtry = ncol(d.train)-1, importance = TRUE)
rmse_reg(rf.tea, d.train, "class") # This is a massive improvement over the previous models (classification, prunning, bagging...)
# 1.651117
plot(rf.boston,col = "blue", type = "l")

#############################################################################
# LOGISTIC MODEL
#############################################################################

logistic<-glm(formula = 1-vs ~ wt, family = binomial, data = mtcars)
summary(logistic)

xweight <- seq(0, 6, 0.01) 
yweight <- predict(logistic, list(wt = xweight),type="response") 
plot(mtcars$wt, 1-mtcars$vs, pch = 16,  xlab = "Motor's wieght in 1000 lbs", main="Weight VS V-Engine", ylab= "V-Engine (Yes/No)") 
lines(xweight, yweight,col="blue")

# Manually set a threhold
prob<-predict(logistic,type="response")
prob

pred<-ifelse(prob>0.7,1,0)
pred

error<-sum(abs(1-mtcars$vs-pred))
error

nrow(mtcars)

confint(logistic)
summary(logistic)

exp(logistic$coefficients["wt"])