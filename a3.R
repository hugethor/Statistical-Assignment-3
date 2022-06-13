# library(caret)
# formula given to us
# y_new[i] <- c(1, x[i,])%*%lm(y[-i] ~ x[-i,]$coef)

library(ggplot2)

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

# Creating the dataframes
data_2008 <- cbind(x_2008, y_2008)
data_2007 <- cbind(x_2007, y_2007)

# Creating the sequence & combinations
predictors <- list("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10", "V11", "V12", "V13", "V14", "V15", "V16", "V17", "V18", "V19", "V20")
combinations_4 <- combn(predictors, 4)
combinations_3 <- combn(predictors, 3)
combinations_2 <- combn(predictors, 2)

# Model for 1 variable
error1 = c()
for (i in predictors){
  model <- lm(paste("result ~", i), data = data_2008)
  error1[i] <- summary(model)$sigma
  error1[i] <- (error1[i]^2 * 22)/24
  print(error1[i])
}


# Model for 2 variables
error2 <- c()
predictor2 <- c()
for (i in 1:ncol(combinations_2)){
  x_var <- NULL
  x <- combinations_2[1,i]
  x <- unlist(x)
  y <- combinations_2[2,i]
  y <- unlist(y)
  x_var <- paste(x_var, x, "+")
  x_var <- paste(x_var, y)
  predictor2[i] <- x_var
  form <- paste("result ~", x_var)
  model <- lm(form, data = data_2008)
  error2[i] <- summary(model)$sigma
  error2[i] <- (error2[i]^2 * 21)/24
  print(error2[i])
}

# Model for 3 variables
error3 <- c()
predictor3 <- c()
for (i in 1:ncol(combinations_3)){
  x_var <- NULL
  x <- combinations_3[1,i]
  x <- unlist(x)
  y <- combinations_3[2,i]
  y <- unlist(y)
  z <- combinations_3[3,i]
  z <- unlist(z)
  x_var <- paste(x_var, x, "+")
  x_var <- paste(x_var, y, "+")
  x_var <- paste(x_var, z)
  predictor3[i] <- x_var
  form <- paste("result ~", x_var)
  model <- lm(form, data = data_2008)
  error3[i] <- summary(model)$sigma
  error3[i] <- (error3[i]^2 * 20)/24
  print(error3[i])
}

# Model for 4 variables
error4 <- c()
predictor4 <- c()
for (i in 1:ncol(combinations_4)){
  x_var <- NULL
  x <- combinations_4[1,i]
  x <- unlist(x)
  y <- combinations_4[2,i]
  y <- unlist(y)
  z <- combinations_4[3,i]
  z <- unlist(z)
  w <- combinations_4[4,i]
  w <- unlist(w)
  x_var <- paste(x_var, x, "+")
  x_var <- paste(x_var, y, "+")
  x_var <- paste(x_var, z, "+")
  x_var <- paste(x_var, w)
  predictor4[i] <- x_var
  form <- paste("result ~", x_var)
  model <- lm(form, data = data_2008)
  error4[i] <- summary(model)$sigma
  error4[i] <- (error4[i]^2 * 19)/24
  print(error4[i])
}

# Create histograms for each model - 1 variable per model
predictors <- c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10", "V11", "V12", "V13", "V14", "V15", "V16", "V17", "V18", "V19", "V20")
hist(error1, breaks = 20,
     main="Error per model - 1 variable as predictor",
     xlab="Errors per predictor",
     freq=FALSE)

# Create histograms for each model - 2 variables per model
hist(error2, breaks = 190,
     main="Error per model - 2 variables as predictors",
     xlab="Errors per combinations of 2 variables",
     freq=FALSE)

# Create histograms for each model - 3 variables per model
hist(error3, breaks = 1140,
     main="Error per model - 3 variables as predictors",
     xlab="Errors per combinations of 3 variables",
     freq=FALSE)

# Create histograms for each model - 4 variables per model
hist(error4, breaks = 4845,
     main="Error per model - 4 variables as predictors",
     xlab="Errors per combinations of 4 variables",
     freq=FALSE)

# Check the best model
min_1 <- min(error1)
min_2 <- min(error2)
min_3 <- min(error3)
min_4 <- min(error4)
min_error <- min(min_1,min_2,min_3,min_4)
if (min_error == min_1){
  index <- which(error1 == min_1)
  best_model <- predictors[index]
  print(best_model)
} else {
  if (min_error == min_2){
    index <- which(error2 == min_2)
    best_model <- predictor2[index]
    print(best_model)
  } else {
    if (min_error == min_3) {
      index <- which(error3 == min_3)
      best_model <- predictor3[index]
      print(best_model)
    } else {
      index <- which(error4 == min_4)
      best_model <- predictor4[index]
      print(best_model)
    }
  }
}

# Obtain coefficients for best model
form <- paste("result ~", best_model)
model <- lm(form, data = data_2008)
coef_model <- summary(model)$coef
# Y = beta0 + beta1*v10 + beta2*v13 + beta3*v17 + beta4*v20
beta0 <- coef_model["(Intercept)", "Estimate"]
beta1 <- coef_model["V10", "Estimate"]
beta2 <- coef_model["V13", "Estimate"]
beta3 <- coef_model["V17", "Estimate"]
beta4 <- coef_model["V20", "Estimate"]





