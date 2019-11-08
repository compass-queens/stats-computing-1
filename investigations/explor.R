install.packages("tidyverse", dependencies = TRUE )
install.packages("tmap")
install.packages("maps")



library(dplyr)
library(readr)
library(ggplot2)

setwd("/home/alessio/Documents/uni/phd/project")

data <- read_csv("database.csv") 

data$Type <- as.factor(data$Type)

filtered_data %>% distinct(Type)

filtered_data <- filter(data, !is.na(Time))

View(filtered_data)

linear_regression <- function(X, y) {
  X <- cbind(X, X*X)
  w_1 <- solve((t(X) %*% X), t(X) %*% y)
  w_0 <-  mean(y) 
  return(list(w_1[[1]], w_1[[2]], w_0))
}

lin_reg_result = linear_regression(filtered_data$Depth, filtered_data$Magnitude)

mapWorld <-  borders("world", color="grey50", fill="grey50")
ggplot(data=filtered_data) + mapWorld + geom_point(aes(x=Longitude, y=Latitude, color=Type), size=0.5, alpha=0.1)

quadratic <- function(w_0, w_1, w_2, x) {
  return(w_0 + (w_1 * x) + (w_2 * x * x) )
}

unpack_quad <- function(W, x) {
  return(W[[2]] + (W[[1]] * x) + (W[[3]]*x*x) )
}

lin_reg_res

lm()

ggplot(data=filter(filtered_data)) + geom_point(aes(x=Depth, y=Magnitude, color=Type), alpha=0.2) + geom_abline(aes(slope=lin_reg_result[[1]], intercept=lin_reg_result[[2]]))

ggplot(data=filter(filtered_data)) + geom_point(aes(x=Depth, y=Magnitude, color=Type), alpha=0.2) + stat_function(fun=function(x){unpack_quad(lin_reg_result, x)})

ggplot(data=filter(filtered_data), aes(x=Depth, y=Magnitude)) + geom_point(alpha=0.2) + geom_smooth(method="lm", formula = y ~ poly(x, 10))


