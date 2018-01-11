epanenchenkov_core <- function(r)

{
  if (abs(r) > 1)
    return (0.)
  return (0.75 * (1 - r^2)) 
}

gauss_core <- function(r)
{
  return ((2 * pi)^(-0.5) * exp(-0.5 * r^2))
}

rectangular_core <- function(r)
{
  if (abs(r) > 1)
    return (0.)
  return (0.5)
}

triangular_core <- function(r)
{
  if (abs(r) > 1)
    return (0.)
  return (1 - abs(r))
}

quadratic_core <- function(r)
{
  if (abs(r) > 1)
    return (0.)
  return (15/16 * (1 - r^2)^2)
}

parzen <- function(test, core, point, n,  h)
{
  classes = rep(0, n)
  size = nrow(test)
  for (row in 1:size)
  {
    cur_point = c(test[row, "x"], test[row, "y"])
    cur_dist = dist(rbind(point, cur_point))
    
    value = core(cur_dist / h)
    id = test[row, "class"]
    classes[id] = classes[id] + value
  }
  
  max = 0
  id = 0
  for (i in 1:n)
  {
    if (classes[i] > max)
    {
      max = classes[i]
      id = i
    }
  }
  return (id)
}


draw_map <- function(data, core, max_x, max_y, step)
{
  colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue")
  plot(data[1:2], pch = 17, bg = colors[iris$Species], col
       = colors[iris$Species], asp = 1, xlab = "Petal.Length", ylab = "Petal.Width")
  title ("PW rectangular")
  
  
  for (x in seq(1, max_x, step))
  {
    for (y in seq(0, max_y, step))
    {
      class = parzen(data, core, c(x, y), 3, 0.35)
    
    
      
      if (class != 0)
        points(x, y, pch = 1 , bg = "white", col = colors[class])
    }
  }
}

data <- data.frame(
  x = iris[, 3],
  y = iris[, 4],
  class = iris[, 5])
draw_map(data, rectangular_core, 7, 3, 0.1)
legend("bottomleft", c("setosa", "versicolor", "virginica"), pch = c(15,15,15), col = c("red", "green3", "blue"))