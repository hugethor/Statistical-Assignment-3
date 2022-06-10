t_2008 <- read.table('~/sc/subset2008.txt')
t_2007 <- read.table('~/sc/subset2007.txt')
matrix_2008 <- as.matrix(t_2008)
matrix_2007 <- as.matrix(t_2007)
y <- seq(20)
combinations <- combn(y, 4)
# combn --> function to generate all combinations of n elements