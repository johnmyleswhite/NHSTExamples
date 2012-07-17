# Assume mean of Group 1 known to be 0
# Therefore we only measure Group 2 data
# Let eps = true difference in means
# Let sigma = true variance of data
# We wish to find smallest sample size, N, such that
# E[p] < 0.05

find.minimum.n <- function(eps, sigma, n.sims = 10000)
{
  n <- 2

  n.too.small <- TRUE
  
  while (n.too.small)
  {
    p <- rep(NA, n.sims)
  
    for (i in 1:n.sims)
    {
      x <- rnorm(n, eps, sigma)
      p[i] <- t.test(x)$p.value
    }
    
    if (mean(p) < 0.05)
    {
      n.too.small <- FALSE
    }
    else
    {
      n <- n + 1      
    }
  }
  
  return(n)
}

set.seed(1)
epsilons <- 10 ^ seq(-1, 1, by = 0.01)

df <- data.frame(Effect = epsilons, N = NA)

for (i in 1:nrow(df))
{
  eps <- df$Effect[i]
  df$N[i] <- find.minimum.n(eps, 1, 2500)
}

ggplot(df, aes(x = Effect, y = N)) + geom_line() + scale_x_log10() + opts(title = "Minimum Sample Size Required to Pass p < .05 NHST in Expectation") + xlab("Effect Size") + ylab("Minimum N")
ggsave("article5/sample_size.png")

save(df, file = "cache.Rdata")
