import matplotlib.pyplot as plt
import numpy as np
from MODEL_JIT import MODEL_JIT

def plot_results(res, r_a):
    """
    Generates plots matching the provided styles for:
    1. Floor space rent
    2. Building height
    3. Land rent
    """
    x = res['x']
    U = res['U']

    # Identify CBD and Urban Area boundaries from the model results
    cbd_max = res['x_0']
    urban_max = res['x_1']

    # Colors and line styles matching the paper
    c_comm = 'tab:red'
    c_res = 'tab:blue'
    c_agri = 'black'

    # To show discontinuities in Floor Space Rent and Building Height, 
    # we mask the arrays so we only plot the values for the "winning" land use.
    p_C_realized = np.where(U == 1, res['p_C_bid'], np.nan)
    p_R_realized = np.where(U == 2, res['p_R_bid'], np.nan)
    S_C_realized = np.where(U == 1, res['S_star_C'], np.nan)
    S_R_realized = np.where(U == 2, res['S_star_R'], np.nan)

    # Create Subplots
    fig, axes = plt.subplots(1, 3, figsize=(15, 5))

    # Panel 1: Floor space rent
    ax = axes[0]
    ax.plot(x, p_C_realized, color=c_comm, linestyle=(0, (5, 5)), linewidth=2, label='Commercial')
    ax.plot(x, p_R_realized, color=c_res, linestyle=(0, (3, 3)), linewidth=2, label='Residential')
    ax.set_title('Floor space rent', fontweight='bold')
    ax.set_ylabel('Floor space rent')

    # Panel 2: Building height
    ax = axes[1]
    ax.plot(x, S_C_realized, color=c_comm, linestyle=(0, (5, 5)), linewidth=2, label='Commercial')
    ax.plot(x, S_R_realized, color=c_res, linestyle=(0, (3, 3)), linewidth=2, label='Residential')
    ax.set_title('Building height', fontweight='bold')
    ax.set_ylabel('Building height (floors)')

    # Panel 3: Land rent
    ax = axes[2]
    # For land rent, plot the full bid rent curves to show where they intersect
    ax.plot(x, res['r_C'], color=c_comm, linestyle=(0, (5, 5)), linewidth=2, label='Commercial')
    ax.plot(x, res['r_R'], color=c_res, linestyle=(0, (3, 3)), linewidth=2, label='Residential')
    ax.plot(x, np.full_like(x, r_a), color=c_agri, linestyle='-', linewidth=2, label='Agricultural')
    ax.set_title('Land rent', fontweight='bold')
    ax.set_ylabel('Land rent')

    # Apply common formatting to all axes
    for ax in axes:
        ax.set_xlim(-50, 50)
        ax.set_xlabel('Distance from Center (km)')
        # Highlight regions
        if cbd_max != np.inf:
            ax.axvspan(-cbd_max, cbd_max, color='gray', alpha=0.1, label='CBD')
        if urban_max != np.inf and cbd_max != np.inf:
            ax.axvspan(-urban_max, -cbd_max, color='lightgray', alpha=0.1, label='Urban area')
            ax.axvspan(cbd_max, urban_max, color='lightgray', alpha=0.1)
        
        ax.grid(True, alpha=0.3)
        ax.set_xticks(np.arange(-50, 60, 10))

    # Custom Legend at the bottom
    handles, labels = axes[2].get_legend_handles_labels()
    by_label = dict(zip(labels, handles))
    order = ['Commercial', 'Residential', 'Agricultural', 'Urban area', 'CBD']
    ordered_handles = [by_label[k] for k in order if k in by_label]
    ordered_labels = [k for k in order if k in by_label]

    fig.legend(ordered_handles, ordered_labels, loc='lower center', ncol=5, bbox_to_anchor=(0.5, -0.05), frameon=False)

    plt.tight_layout(rect=[0, 0.05, 1, 1])
    plt.savefig('model_plots_paper_style.png', dpi=300, bbox_inches='tight')
    print("Plots saved to 'model_plots_paper_style.png'")
    plt.show()

if __name__ == "__main__":
    # Initialize the model and solve for equilibrium
    model = MODEL_JIT(city_size=100_001)
    print("Solving for equilibrium...")
    L, y, res = model.find_eq_bisection_jit()
    print(f"Equilibrium found: L={L:.2f}, y={y:.4f}")
    
    plot_results(res, model.params['r_a'])
