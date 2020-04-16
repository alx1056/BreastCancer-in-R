#install.packages("mlbench")
library(mlbench)
data(BreastCancer, package="mlbench")
bc <- BreastCancer[complete.cases(BreastCancer), ]  # create copy

#checks for structure of bc data
str(bc)

#creates the LOGIT model to use
glm(Class ~ Cell.shape, family="binomial", data = bc)

# removes id column, too large abd redundant
bc <- bc[,-1]

# convert factor data to numeric
for(i in 1:9) {
  bc[, i] <- as.numeric(as.character(bc[, i]))
}
#classifies malignant or not
bc$Class <- ifelse(bc$Class == "malignant", 1, 0)
bc$Class <- factor(bc$Class, levels = c(0, 1))

#shows the classes, 0 being benign and 1 being malignant
table(bc$Class)


#install ML packages
install.packages("caret")

library(caret)
'%ni%' <- Negate('%in%')  # define 'not in' func
options(scipen=999)  # prevents printing scientific notations.


# Prep Training and Test data.
set.seed(100)
trainDataIndex <- createDataPartition(bc$Class, p=0.7, list = F)  # 70% training data
trainData <- bc[trainDataIndex, ]
testData <- bc[-trainDataIndex, ]


#seeds for training/test
set.seed(100)
down_train <- downSample(x = trainData[, colnames(trainData) %ni% "Class"],
                         y = trainData$Class)
#seeds for training/test
set.seed(100)
up_train <- upSample(x = trainData[, colnames(trainData) %ni% "Class"],
                     y = trainData$Class)

#shows table for class
table(up_train$Class)


#creates ML Logit function
logitmod <- glm(Class ~ Cl.thickness + Cell.size + Cell.shape, family = "binomial", data=down_train)
#summarizes function
summary(logitmod)



#creates prediction
pred <- predict(logitmod, newdata = testData, type = "response")


y_pred_num <- ifelse(pred > 0.5, 1, 0)
y_pred <- factor(y_pred_num, levels=c(0, 1))
y_act <- testData$Class


#Shows accuracy of BreastCancer Predictive model
mean(y_pred == y_act)  # ~95% check
