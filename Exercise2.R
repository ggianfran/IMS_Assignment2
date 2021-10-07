MarkVal <- read.delim("MarkVal.txt")
head(MarkVal)

# Ex 2.1 ------------------------------------------------------------------
par(mfrow = c(2,2))
qqnorm(MarkVal$Assets, main = "Assets Q-Q Plot")
qqnorm(MarkVal$Sales, main = "Sales Q-Q Plot")
qqnorm(MarkVal$Market_Value, main = "Market value Q-Q Plot")
qqnorm(MarkVal$Employees, main = "Employees Q-Q Plot")

# Asses joint multivariate normality
p <- 4
n <- 59
data <- as.matrix(MarkVal[,2:5])
deviations <- data - matrix(1,n,1)%*%colMeans(data)

d2 <- rep(0, n)
for (i in 1:n) {
  d2[i] <- t(deviations[i,])%*%solve(cov(data))%*%(deviations[i,]) # Calculate squared distances
}

d2 <- d2[order(d2)] # Order squared distances

theoretical_quantiles <-  qchisq(ppoints(n, a=0.5), df = p)
par(mfrow = c(1,1))
qqplot(theoretical_quantiles, d2,
       main = "Chi-square plot of Multivariate MarkVal Data",
       xlab = "Chi-square quantiles",
       ylab = "Squared distances")


# Ex 2.2 ------------------------------------------------------------------
LnMarkVal <- log(MarkVal[,2:5])
par(mfrow = c(2,2))
qqnorm(LnMarkVal$Assets, main = "log of Assets Q-Q Plot")
qqnorm(LnMarkVal$Sales, main = "log of Sales Q-Q Plot")
qqnorm(LnMarkVal$Market_Value, main = "log of Market value Q-Q Plot")
qqnorm(LnMarkVal$Employees, main = "log of Employees Q-Q Plot")

# Asses joint multivariate normality
p <- 4
n <- 59
lnData <- as.matrix(LnMarkVal)
lnDeviations <- lnData - matrix(1,n,1)%*%colMeans(lnData)

lnD2 <- rep(0, n)
for (i in 1:n) {
  lnD2[i] <- t(lnDeviations[i,])%*%solve(cov(lnData))%*%(lnDeviations[i,]) # Calculate squared distances
}

lnD2 <- lnD2[order(lnD2)] # Order squared distances

theoretical_quantiles <-  qchisq(ppoints(n, a=0.5), df = p)
par(mfrow = c(1,1))
qqplot(theoretical_quantiles, lnD2,
       main = "Chi-square plot of Log of Multivariate MarkVal Data",
       xlab = "Chi-square quantiles",
       ylab = "Squared distances")
qqline(theoretical_quantiles, lnD2)
