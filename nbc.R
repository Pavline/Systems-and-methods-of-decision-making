colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue")
plot(iris[, 3:4], pch = 17, bg = colors[iris$Species], col = colors[iris$Species], asp = 1)
title ("Naive Bayesian Classifier")

naiveBayesianClassifier <- function(point) { 

  m <- length(levels(iris$Species)) # number of classes (3)
  p <- c(0,0,0) # distribution density
  
  answer <- data.frame(1:m, levels(iris$Species))
  colnames(answer) <- c("OptimalBayesianRule", "Species")
  P_apriori <- 1 / 3 # apriori probability
  petalwidth <- c(0.246,1.326,2.026)
  petalwidthSigm <- c(0.1054,0.1978,0.2747)
  petalenght <- c(1.462,4.26,5.552)
  petalenghtSigm <- c(0.1737,0.4699,0.5519)

  p[1]<-log2(P_apriori) +log2(dnorm(point[4], mean = petalwidth[1], sd = petalwidthSigm[1], log = FALSE)) +log2(dnorm(point[3], mean = petalenght[1], sd = petalenghtSigm[1], log = FALSE))
  p[2]<-log2(P_apriori) +log2(dnorm(point[4], mean = petalwidth[2], sd = petalwidthSigm[2], log = FALSE))+log2(dnorm(point[3], mean = petalenght[2], sd = petalenghtSigm[2], log = FALSE))
  p[3]<-log2(P_apriori) +log2(dnorm(point[4], mean = petalwidth[3], sd = petalwidthSigm[3], log = FALSE))+log2(dnorm(point[3], mean = petalenght[3], sd = petalenghtSigm[3], log = FALSE))
 
  return(match(max(p), p))
}

col3 <- seq(from = min(iris[, 3]), to = max(iris[, 3]), by = 0.1)
col4 <- seq(from = min(iris[, 4]), to = max(iris[, 4]), by = 0.1)

for(i in col3) {
  for(j in col4) {
    point <- c(0, 0, i, j)
    points(point[3], point[4],  pch = 21, col = colors[naiveBayesianClassifier(point)])
  }
}

legend("bottomleft", c("virginica", "versicolor", "setosa"), pch = c(15,15,15), col = c("blue", "green3", "red"))