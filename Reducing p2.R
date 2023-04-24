# Define variables
type1 = 0.005
type1Power = 0.05
type2 = 0.25
p = 1 - (9000:9990) / 10000
xbar = qnorm(1 - p / 2)

# Calculate muPower
muPower = qnorm(1 - type2) + qnorm(1 - type1Power / 2)

# Calculate Bayes factors
bfPow = 0.5 * (dnorm(xbar, muPower, 1) + dnorm(xbar, -muPower, 1)) / dnorm(xbar, 0, 1)
muUMPBT = qnorm(0.9975)
bfUMPBT = 0.5 * (dnorm(xbar, muUMPBT, 1) + dnorm(xbar, -muUMPBT, 1)) / dnorm(xbar, 0, 1)
bfLR = 0.5 / exp(-0.5 * xbar ^ 2)
bfLocal = -1 / (2.71 * p * log(p))

# Create a data frame with the calculated Bayes factors
data = data.frame(p, bfLocal, bfLR, bfPow, bfUMPBT)

# Find the values for U_005, L_005, U_05, and L_05
U_005 = max(data$bfLR[data$p == 0.005])
L_005 = min(data$bfLocal[data$p == 0.005])
U_05 = max(data$bfLR[data$p == 0.05])
L_05 = min(data$bfUMPBT[data$p == 0.05])

# Set plot margins
par(mai = c(0.8, 0.8, .1, 0.4))
par(mgp = c(2, 1, 0))

# Plot the Bayes factors
matplot(p, cbind(bfLR, -1 / (2.71 * p * log(p))), type = 'n', log = 'xy', 
        xlab = expression(paste(italic(P), "-value")), ylab = "Bayes Factor",
        ylim = c(0.3, 100), bty = "n", xaxt = "n", yaxt = "n")
lines(p, bfPow, col = "red", lwd = 2.5)
lines(p, bfLR, col = "black", lwd = 2.5)
lines(p, bfUMPBT, col = "blue", lwd = 2.5)
lines(p, bfLocal, col = "green", lwd = 2.5)

# Add a legend
legend(0.015, 100, c(expression(paste("Power")), "Likelihood Ratio Bound", "UMPBT",
                     expression(paste("Local-", italic(H)[1], " Bound"))),
       lty = c(1, 1, 1, 1), lwd = c(2.5, 2.5, 2.5, 2.5), col = c("red", "black", "blue", "green"), cex = 0.8)

# Add custom axis labels
axis(side = 1, at = c(-2, 0.001, 0.0025, 0.005, 0.010, 0.025, 0.050, 0.100, 0.14),
     labels = c("", "0.0010", "0.0025", "0.0050", "0.0100", "0.0250", "0.0500", "0.1000", ""), 
     lwd = 1, tck = -0.01, padj = -1.1, cex.axis = 0.8)
axis(side = 2, at = c(-0.2, 0.3, 0.5, 1, 2, 5, 10, 20, 50, 100), 
     labels = c("", "0.3", "0.5", "1.0", "2.0", "5.0", "10.0", "20.0", "50.0", "100.0"), 
     lwd = 1, las = 1, tck = -0.01, hadj = 0.6, cex.axis = 0.8)
axis(side = 2, at = c(L_005, U_005), labels = c(13.9, 25.7), 
     lwd = 1, las = 1, tck = -0.01, hadj = 0.6, cex.axis = 0.6, col.axis = "red")
axis(side = 4, at = c(-0.2, 0.3, 0.5, 1, 2, 5, 10, 20, 50, 100), 
     labels = c("", "0.3", "0.5", "1.0", "2.0", "5.0", "10.0", "20.0", "50.0", "100.0"), 
     lwd = 1, las = 1, tck = -0.01, hadj = 0.4, cex.axis = 0.8)
axis(side = 4, at = c(L_05, U_05), labels = c(2.4, 3.4), 
     lwd = 1, las = 1, tck = -0.01, hadj = 0.4, cex.axis = 0.6, col.axis = "red")

# Add dashed lines
segments(x0 = 0.000011, y0 = U_005, x1 = 0.005, y1 = U_005, col = "gray40", lty = 2)
segments(x0 = 0.000011, y0 = L_005, x1 = 0.005, y1 = L_005, col = "gray40", lty = 2)
segments(x0 = 0.005, y0 = 0.00000001, x1 = 0.005, y1 = U_005, col = "gray40", lty = 2)
segments(x0 = 0.05, y0 = U_05, x1 = 0.14, y1 = U_05, col = "gray40", lty = 2)
segments(x0 = 0.05, y0 = L_05, x1 = 0.14, y1 = L_05, col = "gray40", lty = 2)
segments(x0 = 0.05, y0 = 0.00000001, x1 = 0.05, y1 = U_05, col = "gray40", lty = 2)
