library(tidyverse)
data <- data.frame(x = seq(1:100))
data$x_squared <- data$x ^ 2
data$x_inverse <- 1 / data$x
data$x_double <- 2* data$x

data$x_complex <- data$x_squared + data$x_double


ggplot(data, aes(x, x_squared)) + geom_line()
ggplot(data, aes(x, x_inverse)) + geom_linet()
ggplot(data, aes(x, x_complex)) + geom_line()