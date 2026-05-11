source("C:/Users/arpad/Documents/University/Master HU-Berlin/2026_Sose/SQE/Topic_3/Ports/R/ab2022_model.R")

# 1. Setup Parameters
params <- list(
  theta_C = 0.5,
  theta_R = 0.55,
  omega_C = 0.03,
  omega_R = 0.07,
  beta_C = 0.03,
  beta_R = 0.0,
  a_bar_C = 2,
  a_bar_R = 1,
  tau_C = 0.01,
  tau_R = 0.005,
  c_C = 1.3,
  c_R = 1.3,
  r_a = 50,
  S_bar_C = 999,
  S_bar_R = 999,
  alpha_C = 0.85,
  alpha_R = 0.66,
  L = 1000000,
  y = 2.5
)

# 2. Setup Grid
# 10001 points from -50 to 50 (step 0.01)
x_vals <- seq(-50, 50, by = 0.01)
dt <- data.table(x = x_vals)
dt[, D := abs(x)]

# 3. Run Equilibrium Solver
message("Solving for General Equilibrium...")
final_params <- FINDEQ(dt, params)

# 4. Calculate Radii
# CBD Radius: Max distance where land use is Commercial (U=1)
cbd_radius <- max(dt[U == 1, D], na.rm = TRUE)
if (is.infinite(cbd_radius)) cbd_radius <- 0

# Urban Radius: Max distance where land use is Commercial or Residential (U < 3)
urban_radius <- max(dt[U < 3, D], na.rm = TRUE)
if (is.infinite(urban_radius)) urban_radius <- 0

# 5. Print Results
cat("\n--- Baseline Model Results ---\n")
cat(sprintf("Total Employment: %.2f\n", final_params$L))
cat(sprintf("Wage:             %.4f\n", final_params$y))
cat(sprintf("CBD Radius:       %.2f km\n", cbd_radius))
cat(sprintf("Urban Radius:     %.2f km\n", urban_radius))
cat("------------------------------\n")
