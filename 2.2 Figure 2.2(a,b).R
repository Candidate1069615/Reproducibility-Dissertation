## Modelling distribution of p-values
### Defining Phi

library(pbivnorm) # Load the package for inverse beta regularized function

# Define the function phi
phi <- function(p, pm, n) {
  lp <- qbeta(2*p, n/2, 1/2, lower.tail = TRUE, log.p = FALSE)
  lpm <- qbeta(1-2*pm, 1/2, n/2, lower.tail = TRUE, log.p = FALSE)
  return(lp^(0.5*(-n-1)) * sqrt((-lp*(lpm-1))/((lp-1)*lpm - 2*sqrt((1-lp)*lp)*sqrt((1-lpm)*lpm) + 1)) * (1/((1/lp) - (2*sqrt(1-lp)*sqrt(lpm)/(sqrt(lp)*sqrt(1-lpm))) + (1/(1-lpm)) - 1))^(n/2))
}

### Fig 2.2(a): Taleb Distribution over Histogram Distribution

n_reps <- 5000 # Set the number of repetitions
n <- 30 # Set the sample size
mu <- 0.425 # Set the mean of the normal distribution
sigma <- 1 # Set the standard deviation of the normal distribution

p_values <- numeric(n_reps) # Initialize a vector to store the p-values

for (i in 1:n_reps) {
  # Generate a random sample of size n from a normal distribution with mean mu and standard deviation sigma
  sample <- rnorm(n, mean = mu, sd = sigma)
  
  # Compute the test statistic
  t_stat <- (mean(sample) - 0) / (sd(sample) / sqrt(n))
  
  # Compute the p-value using a one-sided t-test to the right
  p_value <- pt(t_stat, df = n, lower.tail = FALSE)
  
  # Store the p-value in the p_values vector
  p_values[i] <- p_value
}

# Compute the median and mean p-values
median_p <- median(p_values)
mean_p <- mean(p_values)

# Print the median and mean p-values
cat("Median p-value:", round(median_p, 4))
cat("\nMean p-value:", round(mean_p, 4))

# Generate a sequence of values for p
p_seq <- seq(0, 0.4, length.out = 100)

# Compute the values of Phi(p, pm, n) for the given values of p
Phi_seq <- sapply(p_seq, function(p) Phi(p, median_p, n_reps))

# Set the file name for the PNG file
png_file <- "histogram_Taleb.png"
# Set the size of the PNG file in inches
png_width <- 5.5
png_height <- 4
# Set the resolution for the PNG file in pixels per inch (dpi)
dpi <- 300
# Create a new PNG graphics device
png(png_file, width = png_width, height = png_height, units = "in", res = dpi)
# Create a histogram of p_values with y-axis scaled by n_reps
hist(p_values, xaxt="n", breaks = 100, col = "#EFEFEF", freq = FALSE, xlab = "p-Value", xlim = c(0, 0.4), main = "")
# Add a plot of Phi(p, pm, n) 
lines(p_seq, Phi_seq, col = "blue", lwd = 2)
axis(side = 1, at =c(0,0.05,0.10,0.15,0.20,0.25,0.30,0.35,0.40))
# Add a vertical line at the median p-value in red
abline(v = median_p, col = "red", lty = 2)
# Add a vertical line at the mean p-value in orange
abline(v = mean_p, col = "orange", lty = 2)
# Save the PNG file
dev.off()

median_p
mean_p
sum(p_values<=0.05)/5000

### Fig 2.2(b): Taleb Distribution for varying p_M

# Define the function phi
phi <- function(p, pm, n) {
  lp <- qbeta(2*p, n/2, 1/2, lower.tail = TRUE, log.p = FALSE)
  lpm <- qbeta(1-2*pm, 1/2, n/2, lower.tail = TRUE, log.p = FALSE)
  return(lp^(0.5*(-n-1)) * sqrt((-lp*(lpm-1))/((lp-1)*lpm - 2*sqrt((1-lp)*lp)*sqrt((1-lpm)*lpm) + 1)) * (1/((1/lp) - (2*sqrt(1-lp)*sqrt(lpm)/(sqrt(lp)*sqrt(1-lpm))) + (1/(1-lpm)) - 1))^(n/2))
}

# Set the range of values for p
p_values <- seq(0, 1, 0.01)

# Set the values for pm
pm_values <- c(0.01, 0.05, 0.25, 0.5)

# Set the value for n
n <- 100

# Create a data frame to store the results
results <- data.frame()

# Compute the values of phi for each value of pm
for (pm in pm_values) {
  values <- sapply(p_values, function(p) phi(p, pm, n))
  df <- data.frame(p = p_values, pm = pm, values = values)
  results <- rbind(results, df)
}

# Plot the results
ggplot(results, aes(x = p, y = values, color = factor(pm))) +
  geom_line() +
  scale_color_discrete(name = expression(p[M])) +  # Set the title of the legend
  xlab("p") +
  ylab(expression(phi[L](p))) +
  ylim(c(0, 12.5)) +
  xlim(c(0, 0.5)) +
  theme_classic() +
  theme(text = element_text(size = 12)) +
  labs(title = NULL)

ggsave(filename = "compare4.png", dpi = 300, width = 5.5, height = 4)
