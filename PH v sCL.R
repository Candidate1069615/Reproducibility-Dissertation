ph_data <- simulate_ph(n = 10000, lower = 20, upper = 80, theta0 = 0, tau = 0.1, alpha = c(0, 0.05,0.1, 1), eta = c(0.4,0.3,0.3))
yi <- ph_data$yi
vi <- ph_data$vi
ph_p_values <- get_p_values_right(ph_data$yi,ph_data$vi)
hist(ph_p_values, breaks = 100, xlim = c(0, 0.1), 
     main = "Histogram of P-values", xlab = "P-values", 
     freq = FALSE, col = "lightblue", border = "black")
lines(density(ph_p_values, from = 0, to = max(ph_p_values)), col = "red")
mean(ph_p_values)

c_data <- simulate_classical(n = 10000, lower = 20, upper = 80, theta0 = 0, tau = 0.1)
yi <- ph_data$yi
vi <- ph_data$vi
c_p_values <- get_p_values_right(c_data$yi,c_data$vi)
hist(c_p_values, breaks = 100, xlim = c(0, 0.1), 
     main = "Histogram of P-values", xlab = "P-values", 
     freq = FALSE, col = "lightblue", border = "black")
lines(density(c_p_values, from = 0, to = max(ph_p_values)), col = "red")
mean(c_p_values)

sum(ph_p_values<=0.05) - sum(c_p_values<=0.05)
2194/10000


library(ggplot2)

ph_data <- simulate_ph(n = 10000, lower = 20, upper = 80, theta0 = 0.3, tau = 0.1, alpha = c(0, 0.05, 1), eta = c(0.9, 0.1))
yi <- ph_data$yi
vi <- ph_data$vi
ph_p_values <- get_p_values_right(ph_data$yi,ph_data$vi)

c_data <- simulate_classical(n = 10000, lower = 20, upper = 80, theta0 = 0.3, tau = 0.1)
yi <- ph_data$yi
vi <- ph_data$vi
c_p_values <- get_p_values_right(c_data$yi,c_data$vi)

# Combine the p-values and create a data frame for plotting
df <- data.frame(p_values = c(ph_p_values, c_p_values),
                 type = factor(rep(c("PH", "Classical"), each = length(ph_p_values))))

# Define custom color and transparency values
my_colors <- c("darkslategray3", "tan1")
my_alpha <- 0.7

# Plot the overlaid histogram with density plot and same y-axis scale
ggplot(df, aes(x = p_values, fill = type)) +
  geom_histogram(alpha = my_alpha, position = "identity", bins = 100, aes(y = ..density..)) +
  geom_density(alpha = my_alpha) +
  scale_fill_manual(values = my_colors) +
  labs(title = "Histogram and Density Plot of P-values", x = "P-values", y = "Density") +
  theme_bw() +
  ylim(0, 7) +
  coord_cartesian(xlim = c(0, 0.1)) +
  scale_y_continuous(expand = c(0,0))