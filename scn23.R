library(ggplot2)
# Set the values for your parameters
c <- 10
phi <- 0.9
psi_alpha_c <- 0.3
hh_prime_pairs <- list(c(0, 0), c(0.05, 0.03), c(0.15, 0.1))

# Generate data
power_values <- seq(0, 1, 0.01) # Generate power values from 0 to 1
df <- data.frame()

for (hh_prime in hh_prime_pairs) {
  h <- hh_prime[1]
  h_prime <- hh_prime[2]
  FPR_scen2_values <- sapply(power_values, function(power) FPR_scen2(0.05, h, phi, 1 - power))
  FPR_scen3_values <- sapply(power_values, function(power) FPR_scen3(0.05, c, h, h_prime, phi, psi_alpha_c, 1 - power))
  
  temp_df <- data.frame(
    "Power" = c(power_values, power_values),
    "FPR" = c(FPR_scen2_values, FPR_scen3_values),
    "Scenario" = factor(c(rep("Scenario 2", length(power_values)), rep("Scenario 3", length(power_values)))),
    "HHprime" = paste0("h=", h, ", h'=", h_prime)
  )
  
  df <- rbind(df, temp_df)
}
plot <- ggplot(df, aes(x = Power, y = FPR, group = interaction(Scenario, HHprime), linetype = Scenario, color = HHprime)) +
  geom_line(size = 1) +
  scale_linetype_manual(values = c("solid", "dotted")) +
  theme_bw() +
  labs(title = "FPR vs Power", x = expression(paste("Power (", italic("1 - \u03B2"), ")")), y = "FPR") +
  scale_color_discrete(name = "(h,h')", labels = c("h=0, h'=0", "h=0.05, h'=0.03", "h=0.15, h'=0.1")) +
  theme(legend.position = "bottom")

# Save the plot
ggsave("scen23.png", plot = plot, width = 8, height = 6, dpi = 300)