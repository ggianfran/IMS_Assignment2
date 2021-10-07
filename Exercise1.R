# Ex 1.1
data <- read.table("/cloud/project/data.txt", quote="\"", comment.char="")
X_bar = colMeans(data)  
u0 <- c(0, 0, 0)
S <- cov(data)

n <- 100
p <- 3
alpha <- 0.05

critical_value <- ((n-1)*p)/(n-p) * qf(1-alpha, df1 = p, df2 = n-p)

T2 = as.vector(n * t(X_bar - u0) %*% S %*% (X_bar - u0))

# If true, reject H_0
T2 > critical_value