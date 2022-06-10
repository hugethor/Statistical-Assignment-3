library(caret)

# Loading the predictors
x_2008 <- read.table('subset2008.txt')
x_2007 <- read.table('subset2007.txt')

# Loading the results
y_2008 <- read.table('TITER2008_centered.txt')
y_2007 <- read.table('TITER2007_centered.txt')
y_2007 <- t(y_2007)
y_2007 <- as.data.frame(y_2007)

# Change colname of one column
colnames(y_2008)[colnames(y_2008) == "V1"] <- "result"
colnames(y_2007)[colnames(y_2007) == "V1"] <- "result"

# Creating the matrices
data_2008 <- cbind(x_2008, y_2008)
data_2007 <- cbind(x_2007, y_2007)

# Creating the sequence & combinations
combinations_4 <- combn(seq(20), 4)
combinations_3 <- combn(seq(20), 3)
combinations_2 <- combn(seq(20), 2)

data_2008.result.lm <- train(result ~ V1 + V2, data = data_2008, method = "lm",trControl = ctrl)
summary(data_2008.result.lm)

variables <- list("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9")
sapply(data_2008, lm())
error = c()
for (i in colnames(x_2008)){
  print("for model")
  print(i)
  model <- lm(paste("result ~", i), data = data_2008)
  #error[y]<- model[["results"]][["RMSE"]]
  print(summary(model))
}

print(mean((data_2008$result - pred)^2))

variables <- list(V10, V11, V12, V13, V14, V15, V16, V17, V18, V19, V20)
#Getting RMSE
print(data_2008.result.lm[["results"]][["RMSE"]])

#measuring error
ctrl <- trainControl(method = "LOOCV")


