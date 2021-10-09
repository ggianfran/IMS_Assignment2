# Ex 1.1 ------------------------------------------------------------------
data <- read.table("data.txt")

X_bar = colMeans(data)  
u0 <- c(0, 0, 0)
S <- cov(data)

n <- 100
p <- 3
alpha <- 0.05

critical_value <- ((n-1)*p)/(n-p) * qf(1-alpha, df1 = p, df2 = n-p)

T2 = as.vector(n * t(X_bar - u0) %*% solve(S) %*% (X_bar - u0))

# If true, reject H_0
(Reject_H_0 <- T2 > critical_value)


# Ex 1.2 ------------------------------------------------------------------
# Axes given by sqrt(eigenvalue_i * critical value / n) * eigenvector_i
eig <- eigen(S)
axes <- matrix(0,3,3)
colnames(axes) <- c("Axis 1", "Axis 2", "Axis 3")
for (i in 1:3) {
  axes[,i] <- sqrt(eig$values[i] * critical_value / n) * eig$vectors[,i]
}
axes


# Ex 1.3 ------------------------------------------------------------------
# mu_i = x_bar +/- sqrt( critical_value * S[i,i] / n)
simultaneously_valid_conf_intervals <- matrix(0, 2, 3)
colnames(simultaneously_valid_conf_intervals) <- c("mu_1", "mu_2", "mu_3")
rownames(simultaneously_valid_conf_intervals) <- c("Lower bound", "Upper bound")
for (i in 1:3) {
  simultaneously_valid_conf_intervals[1,i] <- X_bar[i] - abs(sqrt(critical_value * S[i,i] / n))
  simultaneously_valid_conf_intervals[2,i] <- X_bar[i] + abs(sqrt(critical_value * S[i,i] / n))
}
simultaneously_valid_conf_intervals

# Ex 1.4 ------------------------------------------------------------------
# mu_i = x_bar +/- t_{n-1}(alpha/2p) * sqrt(S[i,i] / n)
bonferroni_conf_intervals <- matrix(0, 2, 3)
colnames(bonferroni_conf_intervals) <- c("mu_1", "mu_2", "mu_3")
rownames(bonferroni_conf_intervals) <- c("Lower bound", "Upper bound")
for (i in 1:3) {
  bonferroni_conf_intervals[1,i] <- X_bar[i] - abs(qt(1 - alpha/(2*p), df = n-1) * sqrt(S[i,i] / n))
  bonferroni_conf_intervals[2,i] <- X_bar[i] + abs(qt(1 - alpha/(2*p), df = n-1) * sqrt(S[i,i] / n))
}
bonferroni_conf_intervals



