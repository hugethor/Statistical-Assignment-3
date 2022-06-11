library(caret)
# formula given to us
y_new[i] <- c(1, x[i,])%*%lm(y[-i] ~ x[-i,]$coef)
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
predictors <- list("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10", "V11", "V12", "V13", "V14", "V15", "V16", "V17", "V18", "V19", "V20")
combinations_4 <- combn(predictors, 4)
combinations_3 <- combn(predictors, 3)
combinations_2 <- combn(predictors, 2)

error = c()
y_new = c()
for (i in predictors){
  print("for predictor:")
  model <- lm(paste("result ~", i), data = data_2008)
  error[i] <- summary(model)$sigma
  print(error[i])
}

for (i in 1:ncol(combinations_2)){
  x <- noquote(combinations_2[1,i])
  y <- noquote(combinations_2[2,i])
  x <- unlist(x)
  y <- unlist(y)
  print(x)
  print(y)
  model <- lm(paste("result ~", as.character(x+y)), data = data_2008)
  print(summary(model)$sigma)
}

# stuff from package

data_2008.result.lm <- train(result ~ V1 + V2, data = data_2008, method = "lm",trControl = ctrl)
summary(data_2008.result.lm)
print(mean((data_2008$result - pred)^2))
#Getting RMSE
print(data_2008.result.lm[["results"]][["RMSE"]])
#error[y]<- model[["results"]][["RMSE"]]
#measuring error
ctrl <- trainControl(method = "LOOCV")


