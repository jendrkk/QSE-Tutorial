import numpy as np
from scipy.optimize import root_scalar

class AB2022Model:
    def __init__(self, params=None):
        """
        Initializes the model with the given parameters.
        """
        # Default baseline parameters from prompt and Stata toolkit
        self.params = {
            'alpha_C': 0.85,
            'alpha_R': 0.66,
            'theta_C': 0.5,
            'theta_R': 0.55,
            'omega_C': 0.03,
            'omega_R': 0.07,
            'beta_C': 0.03,
            'beta_R': 0.0,
            'a_bar_C': 2.0,
            'a_bar_R': 1.0,
            'tau_C': 0.01,
            'tau_R': 0.005,
            'c_C': 1.4,
            'c_R': 1.4,
            'r_a': 150.0,
            'S_bar_C': 999.0,
            'S_bar_R': 999.0,
            'x_1_bar': 999.0,
        }
        if params:
            self.params.update(params)
        
        # Grid setup: 10001 points from -50 to 50
        self.x = np.linspace(-50, 50, 10001)
        self.dist = np.abs(self.x)
        self.eps_C = np.ones_like(self.x)
        self.eps_R = np.ones_like(self.x)

    def solver(self, L, y):
        """
        SOLVER procedure: Calculates the spatial equilibrium for a given wage (y) and total employment (L).
        """
        p = self.params
        
        # 1.1 Amenity and Productivity Shifters
        A_tilde_C = p['a_bar_C'] * self.eps_C * (L**p['beta_C']) * np.exp(-p['tau_C'] * self.dist)
        A_tilde_R = p['a_bar_R'] * self.eps_R * (L**p['beta_R']) * np.exp(-p['tau_R'] * self.dist)
        
        # Floor space shifters
        # a_x_C = A_tilde_x_C^(1/(1-alpha_C)) * y^(alpha_C/(alpha_C-1))
        # a_x_R = A_tilde_x_R^(1/(1-alpha_R)) * y^(1/(1-alpha_R))
        a_C = A_tilde_C**(1/(1-p['alpha_C'])) * (y**(p['alpha_C']/(p['alpha_C']-1)))
        a_R = A_tilde_R**(1/(1-p['alpha_R'])) * (y**(1/(1-p['alpha_R'])))
        
        # 1.2 Land Rents
        # Formula: r = a/(1+omega) * (a/(c(1+theta)))^((1+omega)/(theta-omega)) - c * (a/(c(1+theta)))^((1+theta)/(theta-omega))
        def calc_rent(a, omega, theta, c):
            # Optimal height S* = (a / (c(1+theta)))^(1/(theta-omega))
            S_star = (a / (c * (1 + theta)))**(1 / (theta - omega))
            rent = (a / (1 + omega)) * (S_star**(1 + omega)) - c * (S_star**(1 + theta))
            return rent

        r_C = calc_rent(a_C, p['omega_C'], p['theta_C'], p['c_C'])
        r_R = calc_rent(a_R, p['omega_R'], p['theta_R'], p['c_R'])
        
        # Land Use Decision
        # 1: Commercial, 2: Residential, 3: Agricultural
        U = np.ones_like(self.x) * 3
        U[(r_R > r_C) & (r_R > p['r_a'])] = 2
        U[(r_C >= r_R) & (r_C > p['r_a'])] = 1
        
        # Urban Growth Boundary (x_1_bar)
        U[self.dist > p['x_1_bar']] = 3
        
        # 1.3 Endogenous Variables
        # Profit-maximizing height
        S_star_C = (a_C / (p['c_C'] * (1 + p['theta_C'])))**(1 / (p['theta_C'] - p['omega_C']))
        S_star_R = (a_R / (p['c_R'] * (1 + p['theta_R'])))**(1 / (p['theta_R'] - p['omega_R']))
        
        # Realized height
        S_C = np.minimum(p['S_bar_C'], S_star_C)
        S_R = np.minimum(p['S_bar_R'], S_star_R)
        
        # Bid rents (p_bar)
        p_C_bid = a_C * (1 / (1 - p['omega_C'])) * (S_C**p['omega_C'])
        p_R_bid = a_R * (1 / (1 - p['omega_R'])) * (S_R**p['omega_R'])
        
        # Labor Demand and Supply
        # L_D = sum( L_x_C ) where L_x_C = alpha_C/(1-alpha_C) * p_C / y * S_C
        L_D_loc = np.zeros_like(self.x)
        mask_C = (U == 1)
        L_D_loc[mask_C] = (p['alpha_C'] / (1 - p['alpha_C'])) * (p_C_bid[mask_C] / y) * S_C[mask_C]
        
        # L_S = sum( n_x ) where n_x = S_R / f_R and f_R = (1-alpha_R) * y / p_R
        L_S_loc = np.zeros_like(self.x)
        mask_R = (U == 2)
        L_S_loc[mask_R] = (S_R[mask_R] * p_R_bid[mask_R]) / ((1 - p['alpha_R']) * y)
        
        L_D_total = np.sum(L_D_loc)
        L_S_total = np.sum(L_S_loc)
        
        results = {
            'L_D_total': L_D_total,
            'L_S_total': L_S_total,
            'U': U,
            'S_C': S_C,
            'S_R': S_R,
            'S_x': np.where(U == 1, S_C, np.where(U == 2, S_R, 0)),
            'p_C_bid': p_C_bid,
            'p_R_bid': p_R_bid,
            'r_C': r_C,
            'r_R': r_R,
            'S_star_C': S_star_C,
            'S_star_R': S_star_R,
            'x': self.x,
            'dist': self.dist
        }
        return results

    def wage_objective(self, y, L):
        """
        Objective function for wage market clearing: L_D / L_S - 1 = 0
        """
        res = self.solver(L, y)
        if res['L_S_total'] <= 1e-6:
            return 1e10 if res['L_D_total'] > 1e-6 else 0.0
        return res['L_D_total'] / res['L_S_total'] - 1

    def find_wage(self, L, y_guess):
        """
        WAGE: Finds the market-clearing wage y for a fixed total employment L.
        """
        try:
            # Use Brent's method for efficiency
            sol = root_scalar(self.wage_objective, args=(L,), bracket=[0.1, 100], method='brentq', xtol=1e-6)
            return sol.root
        except ValueError:
            # Fallback to dampened fixed-point iteration as in Stata
            y = y_guess
            for _ in range(100):
                res = self.solver(L, y)
                if res['L_S_total'] <= 1e-6:
                    if res['L_D_total'] <= 1e-6: break
                    y *= 1.2
                elif res['L_D_total'] <= 1e-6:
                    y *= 0.8
                else:
                    y_factor = (res['L_D_total'] / res['L_S_total'])**0.01
                    y_new = 0.5 * y + 0.5 * y * y_factor
                    if abs(y_new / y - 1) < 1e-7: break
                    y = y_new
            return y

    def find_eq(self, L_guess, y_guess):
        """
        FINDEQ: Finds the general equilibrium total employment L and wage y.
        """
        L = L_guess
        y = y_guess
        for i in range(100):
            y = self.find_wage(L, y)
            res = self.solver(L, y)
            avg_L_model = 0.5 * (res['L_D_total'] + res['L_S_total'])
            
            if avg_L_model <= 1e-6:
                # City collapsed
                return 0.0, y, res
                
            if abs(L / avg_L_model - 1) < 1e-4:
                break
            
            # Update L using dampened iteration
            L = 0.5 * L + 0.5 * avg_L_model
        
        return L, y, res

    def invert(self, observed_heights, target_L, conv_param=0.05, tolerance=0.001):
        """
        INVERT: Adjusts local amenities (epsilon) to match observed building heights 
        and total population target.
        """
        L, y, res = self.find_eq(target_L, 2.5)
        
        for i in range(200):
            # 1. Update amenities based on height ratio (CONV logic)
            # S_star is the profit-maximizing height without the limit
            S_model = res['S_x']
            mask_urban = (res['U'] < 3)
            
            # Adjustment factor: Observed / Model
            adj = np.ones_like(self.x)
            valid = (S_model > 1e-3) & (observed_heights > 1e-3)
            adj[valid] = observed_heights[valid] / S_model[valid]
            
            # Apply update to both C and R (dampened)
            self.eps_C = (1 - conv_param) * self.eps_C + conv_param * adj * self.eps_C
            self.eps_R = (1 - conv_param) * self.eps_R + conv_param * adj * self.eps_R
            
            # 2. Update L and y
            L, y, res = self.find_eq(L, y)
            
            # 3. Adjust population target (EMP logic)
            if L > 1e-6:
                pop_adj = (target_L / L)**0.01
                self.eps_R *= pop_adj
                L, y, res = self.find_eq(L, y)
            
            # Check convergence (correlation > 0.999 and pop gap < tolerance)
            if L > 1e-6:
                corr = np.corrcoef(observed_heights[mask_urban], S_model[mask_urban])[0, 1]
                pop_gap = abs(L / target_L - 1)
                if corr > 0.999 and pop_gap < tolerance:
                    break
                    
        return L, y, res

if __name__ == "__main__":
    # Baseline run
    model = AB2022Model()
    L_eq, y_eq, res = model.find_eq(1000000, 2.5)
    print(f"Equilibrium Results:")
    print(f"L: {L_eq:,.2f}")
    print(f"y: {y_eq:.4f}")
