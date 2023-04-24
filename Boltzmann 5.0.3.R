library(ggplot2)

# Generate example p-values
p_values_uniform <- seq(0.001, 0.039, by = 0.001)
p_values_high_freq <- seq(0.04, 0.05, by = 0.0001)
p_values <- c(p_values_uniform, p_values_high_freq)

# Boltzmann-inspired model
T <- 0.1 # Temperature

boltzmann_redistribution_uniform_update <- function(p, T) {
  E <- p
  P <- exp(E / T)
  P_normalized <- P / sum(P)
  print(P_normalized)
  p_new <- numeric(length(p))
  for (i in 1:length(p)) {
    r <- runif(1, min(P_normalized), max(P_normalized))
    print(r)
    if (r < P_normalized[i]) {
      print("True")
      p_new[i] <- runif(1, 0, p[i])
    } else {
      p_new[i] <- p[i]
    }
  }
  
  p_new
}

new_p_values_boltzmann_uniform_update <- boltzmann_redistribution_uniform_update(p_values, T)

# Diffusion model
C <- function(p) {
  w1 <- 0.7; mu1 <- 0.045; sigma1 <- 0.002
  w2 <- 0.3; mu2 <- 0.0055; sigma2 <- 0.0005
  w1 * exp(-((p - mu1)^2) / (2 * sigma1^2)) + w2 * exp(-((p - mu2)^2) / (2 * sigma2^2))
}

D <- 0.001
delta_t <- 0.1
iterations <- 1000

diffusion_redistribution <- function(p, D, delta_t, iterations) {
  p_new <- p
  for (i in 1:iterations) {
    delta_p <- D * delta_t * rnorm(length(p), mean = 0, sd = 1)
    p_new <- p_new + delta_p
    p_new <- pmax(pmin(p_new, 0.005), 0) # Ensure p-values are within [0, 0.005]
  }
  p_new
}

new_p_values_diffusion <- diffusion_redistribution(p_values, D, delta_t, iterations)

# Plot histograms
data_original <- data.frame(p_value = p_values, Model = "Original")
data_boltzmann <- data.frame(p_value = new_p_values_boltzmann, Model = "Boltzmann")
data_diffusion <- data.frame(p_value = new_p_values_diffusion, Model = "Diffusion")

data_all <- rbind(data_original, data_boltzmann, data_diffusion)

ggplot(data_all, aes(x = p_value, fill = Model)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
  theme_minimal() +
  labs(title = "Histogram of p-values: Original vs Boltzmann vs Diffusion", x = "p-value", y = "Count") +
  scale_fill_manual(values = c("Original" = "blue", "Boltzmann" = "green", "Diffusion" = "red"))
