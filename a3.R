# Loading the predictors
x_2008 <- read.table('~/sc/subset2008.txt')
x_2007 <- read.table('~/sc/subset2007.txt')

# Loading the results
y_2008 <- read.table('~/sc/TITER2008_centered.txt')
y_2007 <- read.table('~/sc/TITER2007_centered.txt')
y_2007 <- t(y_2007)

# Change colname of one column
colnames(y_2008)[colnames(y_2008) == "V1"] <- "result"
colnames(y_2007)[colnames(y_2007) == "V1"] <- "result"

# Turn into a vector
y_08 <- as.vector(y_2008)
y_07 <- as.vector(y_2007)

# Creating the matrices
matrix_2008 <- as.matrix(x_2008)
matrix_2007 <- as.matrix(x_2007)
matrix_2008 <- cbind(matrix_2008, y_08)
matrix_2008 <- as.matrix(matrix_2008)
matrix_2007 <- cbind(matrix_2007, y_07)

# Creating the sequence & combinations
seq <- seq(20)
combinations_4 <- combn(seq, 4)
combinations_3 <- combn(seq, 3)
combinations_2 <- combn(seq, 2)

