# Logistic Regression
#data()  # datasets available for use in R

# Load the textbook R package
library(ISLR)   #install the package
?Default   # data set
# Load in the credit data
data("Default")
str(Default)
class(Default)
head(Default)
names(Default)
dim(Default)
summary(Default)

# How many people actual default?
(tmp = table(Default$default))
333/10000
table(Default$default, Default$student)
table(Default$student)
?glm
#Multiple Logistic Regression
logit1 = glm(default ~ income + balance + student, family='binomial', data=Default)  #use a dot instead of typing aa the IV
summary(logit1)
exp(coef(logit1))
#income is not significant - remove it

step(glm(default~.,data = Default, family="binomial"))
#this tell us the change in AIC values if one of the DV is dropped; so the softwae does this automatically by initially taking all the DV and then processing it for further AIC values


logit2 = glm(default ~ balance + student, family='binomial', data=Default)
summary(logit2)
coef(logit2)      #of logit model mathematical equation
exp(coef(logit2))
anova(logit2, logit1)

# Predict : use sample values
head(Default)
seq(1, 10000,500)
Default[c(1,501),]
ndata1= Default[seq(1, 10000,500),]
ndata1
nrow(Default[seq(1, 10000,500),])
10000/500

library(dplyr)
(ndata = (slice(Default, seq(1,n(),500))))
ndata
slice(Default, seq(1,n(),1000))  # another way
head(ndata)

addmargins(prop.table(table(Default$default,Default$student)))
0.2817/0.9667; 0.0127/0.0333

options(digits=10)

#Predict on test data
fitted.results = predict(logit2, newdata=ndata,type='response')
fitted.results
head(fitted.results)
fitted.results
cbind(ndata, fitted.results)

ndata
p2 <- ndata %>% mutate(predictnew = ifelse(fitted.results < 0.5, 'No','Yes'))
cbind(p2,fitted.results)
fitted.results
ifelse(fitted.results < 0.05, 0,1)


(ndata2 = data.frame(student=c('Yes','No'), balance=mean(Default$balance), income=mean(Default$income)))
(fitted.results2 <- predict(logit2, newdata=ndata2,type='response'))


Default
xtabs( ~ default + student, data=Default)
table(Default$default, Default$student)
ftable(Default$default, Default$student)

#Accuracy of Model
library(caret)
set.seed(3456)
str(Default)
trainIndex <- createDataPartition(Default$default, p = .67, list = FALSE,   times = 1)

Train <- Default[ trainIndex,]
Test  <- Default[-trainIndex,]
head(Train)
head(Test)

# Logistic Regression Model
model = glm(default ~ student, data=Default, family='binomial')
Test$model_prob <- predict(model, Test, type = "response")
head(Test)
Test <- Test  %>% mutate(default_pred = ifelse(model_prob > .5,'Yes','No'))
head(Test)
Test <- Test %>% mutate(accurate = 1*(default == default_pred))
sum(Test$accurate)/nrow(Test)                         
#96% Accuracy