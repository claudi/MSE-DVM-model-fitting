library(ISLR)
data(Hitters)

set.seed(123)
Hitters_complete <- Hitters[complete.cases(Hitters), ]

rows <- sample(nrow(Hitters_complete), .7 * nrow(Hitters_complete))
train <- Hitters_complete[rows, ]
test <- Hitters_complete[-rows, ]

full_model <- lm(Salary ~., data = train)

select_model <- lm(Salary ~ AtBat + Hits + Walks + CRBI + Division + PutOuts, data = train)

rmse <- function(fitted, actual) {
    sqrt(mean((fitted - actual)^2))
}

Bootstrap <- function(model, data, n, B) {
    total_MSE <- 0
    for(i in 1:B) {
        train <- sample(1:nrow(data), n, replace=TRUE)
        test <- (1:nrow(data))[!((1:nrow(data)) %in% train)]
        new_model <- update(model, .~. , data=data[train, ])
        MSE <- rmse(predict(new_model, newdata = data[test,]), data[test,]$Salary)
        total_MSE <- total_MSE + MSE
    }
    total_MSE <- total_MSE/B
    print("Mean Error")
    print(total_MSE)
}

Bootstrap(full_model, train, 100, 25)
Bootstrap(full_model, train, 100, 50)
Bootstrap(full_model, train, 100, 100)
Bootstrap(select_model, train, 50, 25)
Bootstrap(select_model, train, 50, 50)
Bootstrap(select_model, train, 50, 100)
