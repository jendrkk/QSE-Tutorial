import numpy as np
from ab2022_model import AB2022Model

def run_baseline():
    # Parameters from prompt
    params = {
        'theta_C': 0.5,
        'theta_R': 0.55,
        'omega_C': 0.03,
        'omega_R': 0.07,
        'beta_C': 0.03,
        'a_bar_C': 2.0,
        'a_bar_R': 1.0,
        'tau_C': 0.01,
        'tau_R': 0.005,
        'c_C': 1.3,
        'c_R': 1.3,
        'r_a': 50.0,
        'S_bar_C': 999.0,
        'S_bar_R': 999.0,
        'alpha_C': 0.85,
        'alpha_R': 0.66,
        'beta_R': 0.0
    }
    
    model = AB2022Model(params)
    
    print("Running Baseline Model...")
    L_eq, y_eq, res = model.find_eq(L_guess=1000000, y_guess=2.5)
    
    # Calculate Radii
    # CBD Radius (x0): boundary where commercial use ends (U=1)
    x = res['x']
    U = res['U']
    
    cbd_mask = (U == 1) & (x >= 0)
    if np.any(cbd_mask):
        # The CBD ends where U is no longer 1. 
        # In this symmetric model, we look at positive x.
        cbd_radius = np.max(x[cbd_mask])
    else:
        cbd_radius = 0.0
        
    urban_mask = (U != 3) & (x >= 0)
    if np.any(urban_mask):
        urban_radius = np.max(x[urban_mask])
    else:
        urban_radius = 0.0
        
    print("-" * 30)
    print(f"Results:")
    print(f"Total Employment (L): {L_eq:,.2f}")
    print(f"Equilibrium Wage (y): {y_eq:.4f}")
    print(f"CBD Radius (x0):      {cbd_radius:.2f} km")
    print(f"Urban Radius (x1):    {urban_radius:.2f} km")
    print("-" * 30)

if __name__ == "__main__":
    run_baseline()
