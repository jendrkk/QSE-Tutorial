library(data.table)

#' SOLVER function: Computes spatial equilibrium for a given wage (y) and total employment (L)
#' @param dt data.table with location data (x, D)
#' @param params list of parameters
#' @return list(L_hat_demand, L_hat_supply)
SOLVER <- function(dt, params) {
  # Current values of L and y are stored in params or passed via dt columns
  L_val <- params$L
  y_val <- params$y

  # 1.1 Amenity and Productivity Shifters
  # Use epsilon_C/R if they exist in the data.table, otherwise default to 1.0
  eps_C <- if ("epsilon_C" %in% names(dt)) dt$epsilon_C else 1.0
  eps_R <- if ("epsilon_R" %in% names(dt)) dt$epsilon_R else 1.0

  dt[, A_tilde_x_C := params$a_bar_C * eps_C * (L_val^params$beta_C) * exp(-params$tau_C * D)]
  dt[, A_tilde_x_R := params$a_bar_R * eps_R * (L_val^params$beta_R) * exp(-params$tau_R * D)]
  
  # Floor Space Shifters
  dt[, a_x_C := A_tilde_x_C^(1/(1-params$alpha_C)) * y_val^(params$alpha_C/(params$alpha_C-1))]
  dt[, a_x_R := A_tilde_x_R^(1/(1-params$alpha_R)) * y_val^(1/(1-params$alpha_R))]

  # 1.2 Land Rents
  # Commercial
  dt[, r_x_C := (a_x_C / (1 + params$omega_C)) * (a_x_C / (params$c_C * (1 + params$theta_C)))^((1 + params$omega_C) / (params$theta_C - params$omega_C)) - 
        params$c_C * (a_x_C / (params$c_C * (1 + params$theta_C)))^((1 + params$theta_C) / (params$theta_C - params$omega_C))]
  
  # Residential
  dt[, r_x_R := (a_x_R / (1 + params$omega_R)) * (a_x_R / (params$c_R * (1 + params$theta_R)))^((1 + params$omega_R) / (params$theta_R - params$omega_R)) - 
        params$c_R * (a_x_R / (params$c_R * (1 + params$theta_R)))^((1 + params$theta_R) / (params$theta_R - params$omega_R))]

  # Land Use Decision (U)
  # 1: Commercial, 2: Residential, 3: Agricultural
  dt[, U := 3L] # Default to Agricultural
  dt[r_x_R > params$r_a & r_x_R >= r_x_C, U := 2L]
  dt[r_x_C > params$r_a & r_x_C > r_x_R, U := 1L]
  
  # Handle S_bar constraints and compute Realized Height (S_x,i)
  dt[, S_x_C := 0.0]
  dt[, S_x_R := 0.0]
  
  dt[U == 1, S_x_C := pmin(params$S_bar_C, (a_x_C / (params$c_C * (1 + params$theta_C)))^(1 / (params$theta_C - params$omega_C)))]
  dt[U == 2, S_x_R := pmin(params$S_bar_R, (a_x_R / (params$c_R * (1 + params$theta_R)))^(1 / (params$theta_R - params$omega_R)))]

  # Bid Rents (p_x,i)
  dt[, p_x_C := 0.0]
  dt[, p_x_R := 0.0]
  dt[U == 1, p_x_C := a_x_C * (1 / (1 - params$omega_C)) * S_x_C^params$omega_C]
  dt[U == 2, p_x_R := a_x_R * (1 / (1 - params$omega_R)) * S_x_R^params$omega_R]

  # Labor Demand (L_x,C) and Supply (n_x)
  dt[, L_x_C := 0.0]
  dt[, n_x := 0.0]
  
  dt[U == 1, L_x_C := (params$alpha_C / (1 - params$alpha_C)) * (p_x_C / y_val) * S_x_C]
  dt[U == 2, n_x := S_x_R / ((1 - params$alpha_R) * y_val / p_x_R)]

  # Aggregate values
  L_hat_demand <- sum(dt$L_x_C, na.rm = TRUE)
  L_hat_supply <- sum(dt$n_x, na.rm = TRUE)
  
  return(list(L_hat_demand = L_hat_demand, L_hat_supply = L_hat_supply))
}

#' WAGE function: Finds the wage y that clears the labor market for a fixed total employment L
WAGE <- function(dt, params, tolerance = 0.001, max_iter = 100) {
  iter <- 0
  res <- SOLVER(dt, params)
  
  # Handle zero cases to avoid premature termination
  if (res$L_hat_demand == 0 && res$L_hat_supply == 0) {
    obj_int <- 1.0 # Force at least one iteration
  } else {
    obj_int <- abs((res$L_hat_demand + 1e-6) / (res$L_hat_supply + 1e-6) - 1)
  }
  
  while (obj_int > tolerance && iter < max_iter) {
    if (res$L_hat_supply == 0) {
      y_factor <- 1.2
    } else if (res$L_hat_demand == 0) {
      y_factor <- 0.8
    } else {
      y_factor <- (res$L_hat_demand / res$L_hat_supply)^0.01
    }
    
    params$y <- 0.5 * params$y + 0.5 * params$y * y_factor
    res <- SOLVER(dt, params)
    
    if (res$L_hat_demand == 0 && res$L_hat_supply == 0) {
      obj_int <- 1.0
    } else {
      obj_int <- abs((res$L_hat_demand + 1e-6) / (res$L_hat_supply + 1e-6) - 1)
    }
    iter <- iter + 1
    # cat(sprintf("  WAGE iter %d: y=%.4f, Ld=%.2f, Ls=%.2f, obj=%.4f\n", iter, params$y, res$L_hat_demand, res$L_hat_supply, obj_int))
  }
  return(params)
}

#' FINDEQ function: Finds the equilibrium total employment L
FINDEQ <- function(dt, params, tolerance = 0.001, max_iter = 100) {
  iter <- 0
  params <- WAGE(dt, params)
  res <- SOLVER(dt, params)
  
  if (res$L_hat_demand == 0 && res$L_hat_supply == 0) {
    obj_ext <- 1.0 # Force iteration
  } else {
    obj_ext <- abs(params$L / (0.5 * (res$L_hat_demand + res$L_hat_supply)) - 1)
  }
  
  while (obj_ext > tolerance && iter < max_iter) {
    # If city is empty, we need to be careful about L shrinking to 0
    if (res$L_hat_demand == 0 && res$L_hat_supply == 0) {
      params$y <- params$y * 0.8
    }
    
    params$L <- 0.5 * params$L + 0.25 * (res$L_hat_demand + res$L_hat_supply)
    params <- WAGE(dt, params)
    res <- SOLVER(dt, params)
    
    if (res$L_hat_demand == 0 && res$L_hat_supply == 0) {
      obj_ext <- 1.0
    } else {
      obj_ext <- abs(params$L / (0.5 * (res$L_hat_demand + res$L_hat_supply)) - 1)
    }
    iter <- iter + 1
    cat(sprintf("FINDEQ iter %d: L=%.2f, y=%.4f, Ld=%.2f, Ls=%.2f, obj=%.4f\n", iter, params$L, params$y, res$L_hat_demand, res$L_hat_supply, obj_ext))
  }
  return(params)
}

#' INVERT function: Adjusts local amenities to match observed building heights
#' For simplicity, this implementation follows the pseudo-code logic
INVERT <- function(dt, params, observed_heights_C, observed_heights_R, tolerance = 0.001, max_iter = 50) {
  # Initialize epsilons if not present
  if (!"epsilon_C" %in% names(dt)) dt[, epsilon_C := 1.0]
  if (!"epsilon_R" %in% names(dt)) dt[, epsilon_R := 1.0]
  
  iter <- 0
  params <- FINDEQ(dt, params)
  
  # Current height in model
  current_S_C <- dt$S_x_C
  current_S_R <- dt$S_x_R
  
  # Simple correlation check for commercial height as example
  corr_C <- cor(observed_heights_C[dt$U == 1], current_S_C[dt$U == 1])
  
  while (iter < max_iter) {
    # Update epsilons based on height ratio (avoiding division by zero)
    dt[U == 1, epsilon_C := epsilon_C * (observed_heights_C / pmax(S_x_C, 0.001))]
    dt[U == 2, epsilon_R := epsilon_R * (observed_heights_R / pmax(S_x_R, 0.001))]
    
    params <- FINDEQ(dt, params)
    
    # Re-calculate correlation or objective
    # This is a simplified version of the inversion logic
    iter <- iter + 1
  }
  return(params)
}
