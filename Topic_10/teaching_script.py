import numpy as np
from scipy.spatial.distance import cdist
import importlib.util
import sys
import os

# Import solver from RRH-Solver.py dynamically
current_dir = os.path.dirname(os.path.abspath(__file__))
solver_path = os.path.join(current_dir, "RRH-Solver.py")
spec = importlib.util.spec_from_file_location("rrh_model", solver_path)
rrh = importlib.util.module_from_spec(spec)
sys.modules["rrh_model"] = rrh
spec.loader.exec_module(rrh)
solver = rrh.solver

import matplotlib.pyplot as plt

def main():
    # 1. Parameterization
    alpha = 0.75
    sigma = 5
    LL = 153889  # Total US civilian labor force 2010 proxy
    params = np.array([sigma, alpha])
    
    N = 30
    NN = N * N  # 900 locations

    # 2. Setup Grid and Distance Matrix
    print("Setting up grid and distances...")
    ltd = np.linspace(0, 4, N)
    lgd = np.linspace(0, 4, N)
    ltd_grid, lgd_grid = np.meshgrid(ltd, lgd, indexing='ij')
    coords = np.column_stack((ltd_grid.ravel(), lgd_grid.ravel()))

    # Calculate pairwise Euclidean distance
    dist = cdist(coords, coords, metric='euclidean')
    
    # Own iceberg transport costs are one
    np.fill_diagonal(dist, 1.0)
    
    # Trade costs are a power function of effective distance
    dist = dist ** 0.33

    # 3. Country Matrix (country_mat) and LL_country
    print("Setting up countries...")
    # Left half is West (Country 0), Right half is East (Country 1)
    is_west = coords[:, 1] < 2.0  # Midpoint is 2.0
    is_east = ~is_west

    country_mat = np.zeros((NN, 2))
    country_mat[is_west, 0] = 1
    country_mat[is_east, 1] = 1

    # Total population distributed 50/50 initially based on land area
    LL_west = (np.sum(is_west) / NN) * LL
    LL_east = (np.sum(is_east) / NN) * LL
    LL_country = np.array([LL_west, LL_east])

    # 4. Border Frictions
    print("Setting up border penalties...")
    bord = np.ones((NN, NN)) * 2
    np.fill_diagonal(bord, 1.0)

    bordcty = np.ones((NN, NN))
    # West to East and East to West gets penalty 2
    is_w_2d = is_west[:, None]
    is_e_2d = is_east[:, None]
    bordcty[is_w_2d & is_e_2d.T] = 2
    bordcty[is_e_2d & is_w_2d.T] = 2

    border_penalty = bord * bordcty

    # 5. Productivity (A) and Housing (H)
    print("Generating fundamentals...")
    np.random.seed(1)
    a = np.random.normal(0, 0.01, NN)
    A = np.exp(a)
    
    # Normalize productivities within each country (Geometric Mean to 1)
    A[is_west] = A[is_west] / np.exp(np.mean(np.log(A[is_west])))
    A[is_east] = A[is_east] / np.exp(np.mean(np.log(A[is_east])))

    H = np.ones(NN) * 100

    # 6. Run Solver
    print("\n>>>> Start Wage and Population Convergence <<<<")
    W, L, pi = solver(A, H, dist, border_penalty, country_mat, LL_country, params)
    print(">>>> Wage and Population System Finished <<<<\n")
    print(f"Sample Wages (First 5): \n{W[:5]}\n")
    print(f"Sample Populations (First 5): \n{L[:5]}\n")
    
    print(f"Total Population Check (Should be {LL}): {np.sum(L):.2f}\n")

    # 7. Calculate Variables for Plotting
    print("Calculating plot variables...")
    pi_nn = np.diag(pi)
    
    # Panel A: Log Population
    log_L = np.log(L).reshape((N, N))
    
    # Panel B: Own trade share
    own_trade_share = pi_nn.reshape((N, N))
    
    # Panel C: Price index
    # Formula: P_n = (sigma / (sigma-1)) * (L_n / (sigma * F * pi_nn))**(1/(1-sigma)) * w_n / A_n
    F_val = 1.0
    term1 = sigma / (sigma - 1)
    term2 = (L / (sigma * F_val * pi_nn)) ** (1 / (1 - sigma))
    term3 = W / A
    P_n = term1 * term2 * term3
    price_index = P_n.reshape((N, N))
    
    # Panel D: Average trade cost (within countries)
    avg_trade_cost = np.zeros(NN)
    for i in range(NN):
        # Determine which country this region is in
        country_idx = np.argmax(country_mat[i])
        # Find all regions in the same country
        same_country = (country_mat[:, country_idx] == 1)
        # Exclude self
        mask = same_country.copy()
        mask[i] = False
        # Average distance to other regions in same country
        if np.any(mask):
            avg_trade_cost[i] = np.mean(dist[i, mask])
    
    avg_trade_cost_grid = avg_trade_cost.reshape((N, N))

    # 8. Generate Plot
    print("Generating Slide 30 plots...")
    fig, axes = plt.subplots(2, 2, figsize=(12, 10))
    
    im1 = axes[0, 0].imshow(log_L, origin='lower', cmap='viridis', aspect='auto', extent=[0, 4, 0, 4])
    axes[0, 0].set_title('Panel A : Log Population')
    fig.colorbar(im1, ax=axes[0, 0])
    
    im2 = axes[0, 1].imshow(own_trade_share, origin='lower', cmap='viridis', aspect='auto', extent=[0, 4, 0, 4])
    axes[0, 1].set_title('Panel B : Own trade share')
    fig.colorbar(im2, ax=axes[0, 1])
    
    im3 = axes[1, 0].imshow(price_index, origin='lower', cmap='viridis', aspect='auto', extent=[0, 4, 0, 4])
    axes[1, 0].set_title('Panel C : Price index')
    fig.colorbar(im3, ax=axes[1, 0])
    
    im4 = axes[1, 1].imshow(avg_trade_cost_grid, origin='lower', cmap='viridis', aspect='auto', extent=[0, 4, 0, 4])
    axes[1, 1].set_title('Panel D : Average trade cost (within countries)')
    fig.colorbar(im4, ax=axes[1, 1])
    
    for ax in axes.flat:
        ax.set_xlabel('Longitude')
        ax.set_ylabel('Latitude')
        
    plt.tight_layout()
    plot_path = os.path.join(current_dir, 'slide30_replication.png')
    plt.savefig(plot_path, dpi=300)
    print(f"Plot saved to: {plot_path}")

if __name__ == '__main__':
    main()
