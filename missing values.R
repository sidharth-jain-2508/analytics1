#missing values

x=c(NA,1,NA,2)
is.na(x)
mean(x)
mean(x,na.rm=T)
sum(is.na(x))

x[is.na(x)]=mean(x,na.rm=T)     #replacing values
x


x2=rnorm(100000, mean=50, sd=5)
length(x2)
posn= sample(100000, size=30)
x2[posn]=NA
mean(x2)


library(vim)

head(sleep)
complete.cases(sleep)
sleep[complete.cases(sleep),]
sleep[!complete.cases(sleep),]
colsums(is.na(sleep))
rowsums(is.na(sleep))