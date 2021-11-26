rm(list =ls())
set.seed(123)
#Get the data of each subset
data(iris)
types = unique(iris[,5])
types[1]
# X1=as.matrix(subset(iris, Species == "setosa")[1:4])
# X2=as.matrix(subset(iris, Species == "versicolor")[1:4])
# X3=as.matrix(subset(iris, Species == "virginica")[1:4])

# train.data=as.matrix(iris[1:100,1:4])
# train.label=as.matrix(iris[1:100,5])
# test.data=as.matrix(iris[101:150,1:4])

#get random data
train.index=sample(150,100,replace=F)
train.data=as.matrix(iris[train.index,1:4])
train.label=as.matrix(iris[train.index,5])
test.data=as.matrix(iris[-train.index,1:4])

# KNN Regression/Classification function
knn <- function(train.data, train.label, test.data, K=3, distance='euclidean'){
  train.len <- nrow(train.data)
  test.len <- nrow(test.data)
  test.label <- rep(0,test.len)
  ## calculate distances between samples
  dist <- as.matrix(dist(rbind(test.data, train.data), method= distance))[1:test.len, (test.len+1):(test.len+train.len)]

  ##function to find the char which occurs the most
  getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }

  for (i in 1:test.len){
    nn <- as.data.frame(sort(dist[i,], index.return = TRUE))[1:K,2]
    ##knn for regression
    ##test.label[i]<-mean(train.label[nn])

    ##knn for classification
    test.label[i]=getmode(train.label[nn])
  }
  return (test.label)
}
knn(train.data, train.label, test.data)

