library(caret)
inputData <- read.csv("http://rstatistics.net/wp-content/uploads/2015/09/adult.csv")
head(inputData)

str(inputData)
summary(inputData)


table(inputData$ABOVE50K)

sum(is.na(inputData))

# Random Sampling

train_index <- sample(1:nrow(inputData), 0.8*nrow(inputData))

train_data <- inputData[train_index,]
test_data <- inputData[-train_index,]


#Stratified Random Sampling


# Create Training Data
input_ones <- inputData[which(inputData$ABOVE50K == 1), ]  # all 1's
input_zeros <- inputData[which(inputData$ABOVE50K == 0), ]  # all 0's



set.seed(100)  # for repeatability of samples

input_ones_training_rows <- sample(1:nrow(input_ones), 0.8*nrow(input_ones))  # 1's for training
input_zeros_training_rows <- sample(1:nrow(input_zeros), 0.8*nrow(input_zeros))  # 0's for training. Pick as many 0's as 1's

training_ones <- input_ones[input_ones_training_rows, ]  
training_zeros <- input_zeros[input_zeros_training_rows, ]

trainingData <- rbind(training_ones, training_zeros)  # row bind the 1's and 0's 

table(trainingData$ABOVE50K)





# Create Test Data
test_ones <- input_ones[-input_ones_training_rows, ]
test_zeros <- input_zeros[-input_zeros_training_rows, ]
testData <- rbind(test_ones, test_zeros)  # row bind the 1's and 0's


# Build Logit Models and Predict

logitMod <- glm(ABOVE50K ~., data=trainingData, family='binomial')

step(glm(ABOVE50K ~., data=trainingData, family='binomial'))

logitMod1 <- glm(ABOVE50K ~ AGE + WORKCLASS + FNLWGT + EDUCATION + 
                   MARITALSTATUS + OCCUPATION + RELATIONSHIP + RACE + SEX + 
                   CAPITALGAIN + CAPITALLOSS + HOURSPERWEEK, data=trainingData, family='binomial')




predicted <- predict(logitMod1, testData, type="response")  # predicted scores




# Identify the optimal cut-off value
install.packages(InformationValue)
library(InformationValue)
optCutOff <- optimalCutoff(testData$ABOVE50K, predicted)[1] 
#=> 0.41

pred <- ifelse(predicted<optCutOff,0,1)

testdata_pred <- cbind(testData, pred)


testdata_pred$accurate <- ifelse(testdata_pred$ABOVE50K==testdata_pred$pred,1,0)

sum(testdata_pred$accurate)/nrow(testdata_pred)

#Accuracy 85%


misClassError(testdata_pred$ABOVE50K, testdata_pred$pred)
#=> 0.1485
#or
misClassError(testData$ABOVE50K, predicted, threshold = optCutOff)


# plot ROC curve

plotROC(testdata_pred$ABOVE50K, predicted)

plotROC(testdata_pred$ABOVE50K, testdata_pred$pred)


sensitivity(factor(testdata_pred$ABOVE50K
                   ,levels = c("0", "1"),labels = c("0", "1")), factor(testdata_pred$pred
                                                                       ,levels = c("0", "1"),labels = c("0", "1")))

specificity(factor(testdata_pred$ABOVE50K
                   ,levels = c("0", "1"),labels = c("0", "1")), factor(testdata_pred$pred
                                                                       ,levels = c("0", "1"),labels = c("0", "1")))

confusionMatrix(factor(testdata_pred$ABOVE50K
                       ,levels = c("0", "1"),labels = c("0", "1")), factor(testdata_pred$pred
                                                                           ,levels = c("0", "1"),labels = c("0", "1")))

confusionMatrix(testdata_pred$ABOVE50K, testdata_pred$pred)

