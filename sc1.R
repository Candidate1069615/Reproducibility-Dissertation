library(ggplot2)

# Define the FPR function
FPR <- function(alpha, phi, beta) {
  num <- alpha * phi
  denom <- alpha * phi + (1 - beta) * (1 - phi)
  return(num / denom)
}
FPR(0.05,phi,0.6)
FPR(0.005,phi,0.6)
# Set the values for your parameters
phi <- 0.9 # Adjust this value if necessary

# Generate data
power_values <- seq(0, 1, 0.01) # Generate power values from 0 to 1
FPR_alpha_005 <- sapply(power_values, function(power) FPR(0.05, phi, 1 - power))
FPR_alpha_0005 <- sapply(power_values, function(power) FPR(0.005, phi, 1 - power))

# Create a data frame
df <- data.frame(
  "Power" = c(power_values, power_values),
  "FPR" = c(FPR_alpha_005, FPR_alpha_0005),
  "Alpha" = factor(c(rep(0.05, length(power_values)), rep(0.005, length(power_values))))
)

# Create the plot
plot<- ggplot(df, aes(x = Power, y = FPR, group = Alpha, linetype = Alpha, color = Alpha)) +
  geom_line(size = 1, aes(linetype = Alpha, color = Alpha)) +
  scale_linetype_manual(values = c("0.05" = "solid", "0.005" = "dotted")) +
  scale_color_manual(values = c("0.05" = "red", "0.005" = "blue"), labels = c("0.05", "0.005")) +
  theme_bw() +
  labs(title = "FPR vs Power", x = "Power (1 - Beta)", y = "FPR") +
  guides(
    linetype = guide_legend(title = "Alpha", override.aes = list(color = c("red", "blue"))),
    color = "none"
  ) +
  theme(legend.position = "bottom")


# Save the plot
ggsave("scen1.png", plot = plot, width = 8, height = 6, dpi = 300)