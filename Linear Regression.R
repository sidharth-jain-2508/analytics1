#REGRESSION ANALYSIS

data(women)
women
names(women)
str(women)

cov(women$height,women$weight)

cor(women$height,women$weight)

plot(x=women$height, y=women$weight)
abline(lm(weight~height, data=women),col="red")  #abline is line of best fit
names(women)

fit1=lm(weight~height, data=women)
summary(fit1)


new1=data.frame(height=c(65,66,66.5,70.1))
new1

p1=predict(fit1, newdata=new1)
p1
cbind(new1,p1)

plot(fit1)






