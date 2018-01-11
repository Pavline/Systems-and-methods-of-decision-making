colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue")
plot(iris[, 3:4], pch = 17, bg = colors[iris$Species], col = colors[iris$Species], asp = 1)
title ("KwNN K=6 q=0.5")

euclideanDistance <- function(u, v) {
  sqrt(sum((u - v)^2))
}

k <- 6
q <- 0.5 

xl <- iris[, 3:5]
l <- dim(xl)[1] 
n <- dim(xl)[2] - 1

col3 <- seq(from = min(iris[, 3]), to = max(iris[, 3]), by = 0.1)
col4 <- seq(from = min(iris[, 4]), to = max(iris[, 4]), by = 0.1)

for(i in col3) {
  for(j in col4) {
    point <- c(i, j)
    distances <- matrix(NA, l, 2)
    for(p in 1:l) {
      distances[p, ] <- c(p, euclideanDistance (xl[p, 1:n], point))
    }
    orderedxl <- xl[order(distances[ , 2]), ]
    weights <- c(NA)
    for(t in 1:l) {
      weights[t] <- q^t
    }
    orderedxl_weighted <- cbind(orderedxl, weights)
    classes <- orderedxl_weighted[1:k, (n + 1):(n + 2)] # number, name of class and weight
    sumSetosa <- sum(classes[classes$Species == "setosa", 2])
    sumVersicolor <- sum(classes[classes$Species == "versicolor", 2])
    sumVirginica <- sum(classes[classes$Species == "virginica", 2])
    answer <- matrix(c(sumSetosa, sumVersicolor, sumVirginica), 
                     nrow = 1, ncol = 3, byrow = T, list(c(1), c('setosa', 'versicolor', 'virginica')))
    points(point[1], point[2],  pch = 21, col = colors[which.max(answer)])
  }
}

for (i in 1:l) {
  points(iris[i, 3], iris[i, 4],  pch = 24, bg = colors[iris$Species[i]], col = colors[iris$Species[i]])
}

legend("bottomleft", c("setosa", "versicolor", "virginica"), pch = c(15,15,15), col = c("red", "green3", "blue"))