#clustering

subject1 = trunc(rnorm(30, mean=60, sd=15))
subject1
range(subject1)

marks=data.frame(subject1)
marks

k2=kmeans(marks, centers = 2)  #centres tell number of clusters
k2
k2$size

marks[k2$cluster==1,]
marks[k2$cluster==2,]

mean(marks[k2$cluster==1,])
mean(marks[k2$cluster==2,])
k2$centers
