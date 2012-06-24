# Let eps = measured group mean
# Assume sigma = 1
# t = eps / (sigma / sqrt(n)) = sqrt(n) * eps / sigma = sqrt(n) * eps
# Thus, E[t] = E[sqrt(n) * eps] = sqrt(n) * E[eps]
# We want to find smallest n such that:
# CDF_t(sqrt(n) * E[eps], n - 1) > 0.975

find.minimum.n <- function(eps)
{
  n <- 2
  
  while (pt(eps * sqrt(n), n - 1) < 0.975)
  {
    n <- n + 1
  }
  
  return(n)
}

epsilons <- 10 ^ seq(-1, 1, by = 0.01)

df <- data.frame(Effect = epsilons, N = NA)

for (i in 1:nrow(df))
{
  eps <- df$Effect[i]
  df$N[i] <- find.minimum.n(eps)
}

ggplot(df, aes(x = Effect, y = N)) + geom_line() + scale_x_log10() + opts(title = "Minimum Sample Size Required to Pass p < .05 NHST")
ggsave("article5/sample_size.png")
