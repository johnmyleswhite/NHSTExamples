library("ggplot2")

set.seed(1)

total.simulations <- 10000

m <- 100
n <- 20

results <- rep(NA, total.simulations)

for (i in 1:total.simulations)
{
  x <- matrix(runif(m * n), nrow = m, ncol = n)
  y <- runif(m)
  
  p.values <- summary(lm(y ~ x))$coefficients[, 4]
  results[i] <- sum(p.values < 0.05)
}

qplot(results, binwidth = 1)

table(results) / total.simulations
