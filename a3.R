# library(caret)
# formula given to us
# y_new[i] <- c(1, x[i,])%*%lm(y[-i] ~ x[-i,]$coef)

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
predictor_list2 <- c()
for (i in 1:ncol(combinations_2)){
  x_var <- NULL
  x <- combinations_2[1,i]
  x <- unlist(x)
  y <- combinations_2[2,i]
  y <- unlist(y)
  x_var <- paste(x_var, x, "+")
  x_var <- paste(x_var, y)
  predictor_list2[i] <- x_var
  form <- paste("result ~", x_var)
  model <- lm(form, data = data_2008)
  error2[i] <- summary(model)$sigma
  error2[i] <- (error2[i]^2 * 21)/24
  print(error2[i])
}

# Model for 3 variables
error3 <- c()
predictor_list3 <- c()
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
  predictor_list3[i] <- x_var
  form <- paste("result ~", x_var)
  model <- lm(form, data = data_2008)
  error3[i] <- summary(model)$sigma
  error3[i] <- (error3[i]^2 * 20)/24
  print(error3[i])
}

# Model for 4 variables
error4 <- c()
predictor_list4 <- c()
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
  predictor_list4[i] <- x_var
  form <- paste("result ~", x_var)
  model <- lm(form, data = data_2008)
  error4[i] <- summary(model)$sigma
  error4[i] <- (error4[i]^2 * 19)/24
  print(error4[i])
}

# Check the best model

a <- min(error1)
b <- min(error2)
c <- min(error3)
d <- min(error4)

error <- min(a,b,c,d)

index <- which(error4 == d)
print(index)
best_model <- predictor_list4[index]
print(best_model)
print(d)

