library(ISLR)
data(Hitters)

set.seed(123)
Hitters_complete <- Hitters[complete.cases(Hitters), ]
