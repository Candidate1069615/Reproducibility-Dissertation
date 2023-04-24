# Code for Table 5.2
# Replace alpha, eta, and type = "ph" to generate Table 5.1

# Define the parameters to vary
theta0_values <- c(0, 0.1, 0.3, 0.5)
tau_values <- c(0.1, 0.5, 0.8)

# Initialize an empty data frame to store the results
results <- data.frame()

# Iterate through each combination of theta0 and tau
for (t0 in theta0_values) {
  for (t in tau_values) {
    pb_data <- simulate_pb(n = 10000, lower = 20, upper = 80, theta0 = t0, tau = t, alpha = c(0, 0.025, 0.05, 1), eta = c(1,0.7,0.1))
    pb_p_values <- get_p_values_right(pb_data$yi, pb_data$vi)
    
    # Add the estimates function call
    est_pb3 <- estimates(N = 100, n = 50, lower = 20, upper = 80, theta0 = t0, tau = t, alpha = c(0, 0.025, 0.05, 1), eta = c(1,0.7,0.1), type = "pb")
    
    # Extract the new variables
    pb_mean = est_pb3[1,1]
    pb_sd = est_pb3[1,2]
    pb_tau_mean = est_pb3[2,1]
    pb_tau_sd = est_pb3[2,2]
    
    cl_mean = est_pb3[5,1]
    cl_sd = est_pb3[5,2]
    cl_tau_mean = est_pb3[6,1]
    cl_sd = est_pb3[6,2]
    
    pb_mean = est_pb3[3,1]
    pb_sd = est_pb3[3,1]
    pb_tau_mean = est_pb3[4,1]
    pb_tau_sd = est_pb3[4,2]
    
    #Discontinuity <- Discontinuity_test(pb_p_values, d_point)
    LCM_sup <- LCM(pb_p_values, min(pb_p_values), max(pb_p_values))
    CS_1 <- CoxShi(pb_p_values, id, min(pb_p_values), max(pb_p_values), J, 1, 0, test_type = "one_sided")
    CS_2B <- CoxShi(pb_p_values, id, min(pb_p_values), max(pb_p_values), J, 2, 1, test_type = "one_sided")
    
    # Combine the results into a single row and include the new variables
    row <- data.frame(theta0 = t0, tau = t, LCM_sup = LCM_sup, CS_1 = CS_1, CS_2B = CS_2B, pb_mean = pb_mean,pb_tau_mean=pb_tau_mean,cl_mean=cl_mean,cl_tau_mean=cl_tau_mean,pb_mean=pb_mean,pb_tau_mean=pb_tau_mean)
    
    # Append the row to the results data frame
    results <- rbind(results, row)
  }
}

results
