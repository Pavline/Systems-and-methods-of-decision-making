scalar_product <- function(a, b)
{
  return (sum(a * b))
}

segment_inf <- function(xs, ys, color)
{
  fit = lm(ys~xs)
  abline(fit, col = color)
}

LOO <- function(data, w)
{
  sum = 0;
  for (row in 1:nrow(data))
  {
    point = c(data[row, 1], data[row, 2])
    test_data = data[-row, ,]
    value = get_class(point, w)
    if (value < 0)
      ans = first
    else
      ans = second
    
    cur_res = data[row, 3]
    value = 1
    if (cur_res == ans)
      value = 0
    
    sum = sum + value
  }
  return (sum / nrow(data))
}

draw <- function(data, max_x, max_y, step)
{
  colors <- c("setosa" = rgb(1, 0, 0,  alpha = 1),
              "versicolor" = rgb(0, 1, 0, alpha = 1),
              "virginica" = rgb(0, 0, 1,  alpha = 1))
  colors_trans <- c("setosa" = rgb(1, 0, 0,  alpha = 0.1),
                    "versicolor" = rgb(0, 1, 0, alpha = 0.1), 
                    "virginica" = rgb(0, 0, 1,  alpha = 0.1))
  
  plot(data[1:2], pch = 17, bg = colors[data[, 3]],
       col = colors[data[, 3]], asp = 1, main = "adaline", xlab = "Petal.Length", ylab = "Petal.Width",
       xlim = c(-7, 7), ylim = c(-3, 3))
  
  w = find_w(data, 0.1, 0.05)
  
  x <- c(min(data[, 2]), max(data[, 2]))
  y <- c(-w[[2]]*x[2]/w[[1]], -w[[1]]*x[1]/w[[2]])
  res3 <- c(x[1], y[[1]])
  res4 <- c(y[[2]], x[2])
  segment_inf(res3, res4, "orange")
  
  for (x in seq(-7, max_x, step))
  {
    for (y in seq(-3, max_y, step)) 
    {
      sign = get_class(c(x, y), w)
      class = 2
      if (sign > 0)
        class = 1
      c = colors_trans[class]
      
      if (class != 0)
        points(x, y, pch = 1, bg = c, col = c)
    }
  }
}

find_w <- function(data, eta, eps = 0.1)
{
  l = nrow(data)
  n = ncol(data) - 2
  w = runif(n, -1 / (2 * n), 1 / (2 * n))
  
  Q = 0
  for (i in 1:l)
  {
    sign = data[i, 4]
    Q = Q + (scalar_product(data[i, 1:2], w) - sign)^2
  }
  
  while (Q > eps)
  {
    idx = sample(1:l, 1, replace = T)
    x = data[idx, 1:2]
    sign = data[i, 4]
    product = scalar_product(x, w) * sign
    e = ((1 - product)^2)
    step = eta * (scalar_product(w, x) - sign)
    w = w - x * step
    Q = (scalar_product(data[idx, 1:2], w) - sign)^2
    x <- c(min(data[, 2]), max(data[, 2]))
    y <- c(-w[[2]]*x[2]/w[[1]], -w[[1]]*x[1]/w[[2]])
    res3 <- c(x[1], y[[1]])
    res4 <- c(y[[2]], x[2])
    segment_inf(res3, res4, rgb(0, 0, 0, alpha = 0.1))
  }
  
  return (w)
}

get_class <- function(point, w)
{
  return (sign(scalar_product(point, w)))
}

midpoint <- function(v)
{
  return (min(v) + (max(v) - min(v))  / 2)
}

first = "setosa"
second = "versicolor"
data = iris[iris$Species == first | iris$Species == second, ]
data[ , 3] = data[ , 3] - midpoint(data[ , 3])
data[ , 4] = data[ , 4] - midpoint(data[ , 4])
data = data[3:5]
col = c(0, nrow(data))
data = cbind(data, col)
for (i in 1:nrow(data))
{
  value = -1
  if (data[i, 3] == second)
    value = 1
  data[i, 4] = value
}

draw(data, 7, 3, 0.1)