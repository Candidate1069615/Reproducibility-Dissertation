library(ggplot2)

n_reps <- 5000 # Set the number of repetitions
n <- 30 # Set the sample size
sigma <- 1 # Set the standard deviation of the normal distribution

mu_vals <- seq(0, 1, length.out = 250) # Create a sequence of 500 mu values from 0 to 1
medians <- numeric(length(mu_vals)) # Initialize a vector to store the median p-values
random_p_values <- numeric(length(mu_vals)) # Initialize a vector to store the randomly selected p-values

for (j in 1:length(mu_vals)) {
  mu <- mu_vals[j]
  p_values <- numeric(n_reps) # Initialize a vector to store the p-values
  
  for (i in 1:n_reps) {
    # Generate a random sample of size n from a normal distribution with mean mu and standard deviation sigma
    sample <- rnorm(n, mean = mu, sd = sigma)
    
    # Compute the test statistic
    t_stat <- (mean(sample) - 0) / (sd(sample) / sqrt(n))
    
    # Compute the p-value using a one-sided t-test to the right
    p_value <- pt(t_stat, df = n-1, lower.tail = FALSE)
    
    # Store the p-value in the p_values vector
    p_values[i] <- p_value
  }
  
  # Compute the median p-value
  medians[j] <- median(p_values)
  
  # Randomly select a p-value and store it in the random_p_values vector
  random_p_values[j] <- sample(p_values, 1)
}

# Create a data frame for plotting
df <- data.frame(mu = mu_vals, median_p = medians, random_p = random_p_values)

# Calculate how many p-values are above and below the line
above_line <- sum(random_p_values > medians)
below_line <- sum(random_p_values < medians)

cat("Number of p-values above the line:", above_line, "\n")
cat("Number of p-values below the line:", below_line, "\n")

# Calculate the proportion of scatters smaller than all previous examples
smaller_than_all_previous <- sum(diff(random_p_values) < 0)
prop_smaller_than_all_previous <- smaller_than_all_previous / (length(random_p_values) - 1)

# Calculate the proportion of scatters smaller than the previous example
smaller_than_previous <- sum(diff(random_p_values) < 0 & diff(mu_vals) > 0)
prop_smaller_than_previous <- smaller_than_previous / (length(random_p_values) - 1)

cat("Proportion of scatters smaller than all previous examples:", prop_smaller_than_all_previous, "\n")
cat("Proportion of scatters smaller than the previous example:", prop_smaller_than_previous, "\n")

# Plot the graph
ggplot(df, aes(x = mu)) +
  geom_line(aes(y = median_p), color = "black", size = 1) +
  geom_point(aes(y = random_p), color = "red") +
  labs(x = expression(paste("True mean difference, ", m[v])), y = "p-value") +
  theme_minimal()
