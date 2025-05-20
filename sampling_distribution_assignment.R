
# ==========================================================
# SAMPLING DISTRIBUTION ASSIGNMENT – FULL SOLUTION IN R
# ==========================================================

# ==========================================================
# BINOMIAL DISTRIBUTION
# ==========================================================

# Q1: What is the expected value and variance of the sampling distribution 
# of the mean for a Binomial(n = 10, p = 0.4) population?

n <- 10
p <- 0.4

mu <- n * p
var <- n * p * (1 - p)

cat("Theoretical Mean =", mu, "\n")
cat("Theoretical Variance =", var, "\n")

# Q2: Simulate the sampling distribution of the mean for Binomial(n = 10, p = 0.8) 
# and observe how skewness changes with p.

set.seed(1)
sample_size <- 30
num_samples <- 1000

sample_means <- replicate(num_samples, {
  mean(rbinom(sample_size, size = 10, prob = 0.8))
})

hist(sample_means, main = "Sample Means from Binomial(10, 0.8)", col = "skyblue")
cat("Empirical Mean:", mean(sample_means), "\n")
cat("Empirical Variance:", var(sample_means), "\n")

# ==========================================================
# POISSON DISTRIBUTION
# ==========================================================

# Q1: If X ~ Poisson(λ = 4), simulate the sampling distribution of the mean 
# for sample size n = 50. Compare theoretical and empirical mean and variance.

set.seed(2)
lambda <- 4
n <- 50
num_samples <- 1000

sample_means <- replicate(num_samples, {
  mean(rpois(n, lambda))
})

cat("Empirical Mean:", mean(sample_means), "\n")
cat("Empirical Variance:", var(sample_means), "\n")

hist(sample_means, main = "Sample Means from Poisson(λ = 4)", col = "lightgreen")

cat("Theoretical Mean =", lambda, "\n")
cat("Theoretical Variance =", lambda / n, "\n")

# Q2: How does the distribution of sample means change as λ increases?

sample_means2 <- replicate(num_samples, mean(rpois(n, lambda = 20)))
hist(sample_means2, main = "Poisson(λ=20) Sample Means", col = "orange")

# ==========================================================
# EXPONENTIAL DISTRIBUTION
# ==========================================================

# Q1: For X ~ Exponential(λ = 1.5), simulate sample means and check normality 
# for n = 10 and n = 50.

set.seed(3)
lambda <- 1.5

sample_means_10 <- replicate(1000, mean(rexp(10, rate = lambda)))
hist(sample_means_10, main = "Exponential Sample Means (n = 10)", col = "lightblue")

sample_means_50 <- replicate(1000, mean(rexp(50, rate = lambda)))
hist(sample_means_50, main = "Exponential Sample Means (n = 50)", col = "lightcoral")

# ==========================================================
# NORMAL DISTRIBUTION
# ==========================================================

# Q1: Use X ~ N(70, 10^2). Simulate the sampling distribution of the mean 
# for different values of n and confirm that it remains normal.

set.seed(4)
mu <- 70
sigma <- 10
n_vals <- c(5, 30, 100)

par(mfrow = c(1, 3))
for (n in n_vals) {
  sample_means <- replicate(1000, mean(rnorm(n, mean = mu, sd = sigma)))
  hist(sample_means, main = paste("Normal (n =", n, ")"), col = "khaki", xlab = "Mean")
}

# ==========================================================
# GAMMA DISTRIBUTION
# ==========================================================

# Q1: Take X ~ Gamma(shape = 2, rate = 1). Simulate and compare the sampling 
# distribution for n = 10 and n = 100.

set.seed(5)
shape <- 2
rate <- 1

sample_means_10 <- replicate(1000, mean(rgamma(10, shape = shape, rate = rate)))
sample_means_100 <- replicate(1000, mean(rgamma(100, shape = shape, rate = rate)))

par(mfrow = c(1, 2))
hist(sample_means_10, main = "Gamma Means (n = 10)", col = "lightblue")
hist(sample_means_100, main = "Gamma Means (n = 100)", col = "salmon")

# ==========================================================
# BONUS: Probability Integral Transformation – Exponential Example
# ==========================================================

# Q: Use PIT to generate Exponential(λ=1.5) samples using inverse CDF.

set.seed(6)
U <- runif(1000)
lambda <- 1.5

X <- -log(U) / lambda
hist(X, breaks = 40, main = "Exponential from PIT", col = "violet")
