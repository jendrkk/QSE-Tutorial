import numpy as np
import pandas as pd
import time


class MODEL:
    def __init__(self, params=None, city_size=10001):
        """
        Initializes the model with the given parameters.
        """
        if params is None:
            params = {}

        # Allow city_size passed inside params for convenience
        if 'city_size' in params:
            city_size = int(params.pop('city_size'))

        self.params = {
            'alpha_C': 0.85, 'alpha_R': 0.66,
            'theta_C': 0.5, 'theta_R': 0.55,
            'omega_C': 0.03, 'omega_R': 0.07,
            'beta_C': 0.03, 'beta_R': 0.0,
            'a_bar_C': 2.0, 'a_bar_R': 1.0,
            'tau_C': 0.01, 'tau_R': 0.005,
            'c_C': 1.4, 'c_R': 1.4,
            'r_a': 30.0,
            'S_bar_C': 999.0, 'S_bar_R': 999.0,
            'x_1_bar': 999.0,
        }
        self.params.update(params)

        self.city_size = int(city_size)
        self.x = np.linspace(-50, 50, self.city_size)
        self.dist = np.abs(self.x)
        self.eps_C = np.ones_like(self.x)
        self.eps_R = np.ones_like(self.x)

        # Pre-calculate distance decay
        self._dist_decay_C = np.exp(-self.params['tau_C'] * self.dist)
        self._dist_decay_R = np.exp(-self.params['tau_R'] * self.dist)

    def solver(self, L, y):
        p = self.params
        
        # 1.1 Amenity and Productivity Shifters
        A_tilde_C = p['a_bar_C'] * self.eps_C * (L**p['beta_C']) * self._dist_decay_C
        A_tilde_R = p['a_bar_R'] * self.eps_R * (L**p['beta_R']) * self._dist_decay_R
        
        # Floor space shifters
        # Use max(y, eps) to avoid div by zero
        y_safe = max(y, 1e-10)
        a_C = A_tilde_C**(1/(1-p['alpha_C'])) * (y_safe**(p['alpha_C']/(p['alpha_C']-1)))
        a_R = A_tilde_R**(1/(1-p['alpha_R'])) * (y_safe**(1/(1-p['alpha_R'])))
        
        # 1.2 Land Rents
        def calc_rent(a, omega, theta, c):
            S_star = (a / (c * (1 + theta)))**(1 / (theta - omega))
            rent = (a / (1 + omega)) * (S_star**(1 + omega)) - c * (S_star**(1 + theta))
            return rent, S_star

        r_C, S_star_C = calc_rent(a_C, p['omega_C'], p['theta_C'], p['c_C'])
        r_R, S_star_R = calc_rent(a_R, p['omega_R'], p['theta_R'], p['c_R'])
        
        # Land Use Decision
        U = np.full_like(self.x, 3, dtype=int)
        mask_R = (r_R > r_C) & (r_R > p['r_a'])
        mask_C = (r_C >= r_R) & (r_C > p['r_a'])
        U[mask_R] = 2
        U[mask_C] = 1
        U[self.dist > p['x_1_bar']] = 3

        # Efficient boundary finding
        pos_x = self.x >= 0
        x_1 = np.min(self.x[(U == 3) & pos_x]) if np.any((U == 3) & pos_x) else np.inf
        x_0 = np.min(self.x[(U != 1) & pos_x]) if np.any((U != 1) & pos_x) else np.inf
        
        # 1.3 Endogenous Variables
        S_C = np.minimum(p['S_bar_C'], S_star_C)
        S_R = np.minimum(p['S_bar_R'], S_star_R)
        
        p_C_bid = a_C * (1 / (1 - p['omega_C'])) * (S_C**p['omega_C'])
        p_R_bid = a_R * (1 / (1 - p['omega_R'])) * (S_R**p['omega_R'])
        
        L_D_loc = np.zeros_like(self.x)
        mask_U1 = (U == 1)
        L_D_loc[mask_U1] = (p['alpha_C'] / (1 - p['alpha_C'])) * (p_C_bid[mask_U1] / y_safe) * S_C[mask_U1]
        
        L_S_loc = np.zeros_like(self.x)
        mask_U2 = (U == 2)
        L_S_loc[mask_U2] = (S_R[mask_U2] * p_R_bid[mask_U2]) / ((1 - p['alpha_R']) * y_safe)
        
        L_D_total = np.sum(L_D_loc)
        L_S_total = np.sum(L_S_loc)
        
        return {
            'L_D_total': L_D_total, 'L_S_total': L_S_total,
            'U': U, 'S_C': S_C, 'S_R': S_R,
            'S_x': np.where(U == 1, S_C, np.where(U == 2, S_R, 0)),
            'p_C_bid': p_C_bid, 'p_R_bid': p_R_bid,
            'r_C': r_C, 'r_R': r_R,
            'S_star_C': S_star_C, 'S_star_R': S_star_R,
            'x_0': x_0, 'x_1': x_1, 'x': self.x, 'dist': self.dist
        }
    def advanced_solver(self, L, y):
        """Implements Algorithm 4: BETTERSOLVER (Slide 17)"""
        p = self.params
        r = self.solver(L, y)

        # Step 11: Compute implied residential population (N_hat)
        N_hat = r['L_S_total']

        # Step 12: Compute implied market-clearing wage (y_hat)
        # Formula: (alpha_C / (1 - alpha_C)) * sum(p_C * S_C) / N_hat
        mask_C = (r['U'] == 1)
        wage_bill_integral = np.sum(r['p_C_bid'][mask_C] * r['S_C'][mask_C])
        
        if N_hat < 1e-10:
            y_hat = 0.0
        else:
            y_hat = (p['alpha_C'] / (1 - p['alpha_C'])) * wage_bill_integral / N_hat

        return r, N_hat, y_hat

    def find_eq_adv(self, L_guess, y_guess, tol=1e-4, max_iter=2000):
        """Implements Algorithm: BETTERFINDEQ """
        y = y_guess
        L = L_guess
        omega_N = 0.05
        omega_y = 0.01
        
        for _ in range(max_iter):
            res, N_hat, y_hat = self.advanced_solver(L, y)

            # Convergence check
            if abs(L/max(N_hat, 1e-10) - 1) < tol and abs(y/max(y_hat, 1e-10) - 1) < tol: 
                return N_hat, y_hat, res
            
            # Update BOTH guesses simultaneously using weighted combination (Slide 19)
            y = omega_y * y_hat + (1 - omega_y) * y
            L = omega_N * N_hat + (1 - omega_N) * L

            # Safeguards to prevent catastrophic negative values
            y = max(1e-5, min(y, 100.0))
            L = max(1e-5, L)

        print("The algorithm did not converge")
        return L, y, res


    def wage_solver(self, L, y, tol=1e-7, max_iter=500):
        """Simple wage solver with improved convergence."""
        y_prev = y * 1.05

        for i in range(max_iter):
            res = self.solver(L, y)
            if res['L_S_total'] < 1e-6 and res['L_D_total'] < 1e-6:
                return y
            
            diff = res['L_D_total'] - res['L_S_total']
            if np.abs(diff) / max(1e-6, res['L_S_total']) < tol:
                return y
            
            y_fac = (res['L_D_total'] / res['L_S_total']) ** 0.01 if res['L_S_total'] > 1e-6 else 1.2
            y_new = 0.5 * y + 0.5 * y * y_fac
            
            y_prev = y
            y = max(1e-5, min(y_new, 100.0)) # Bounds for stability

            if np.abs(y/y_prev - 1) < tol:
                return y
        return y

    def find_eq_simple(self, y_guess, L_guess):
        return self._find_eq_outer(y_guess, L_guess)

    def _find_eq_outer(self, y_guess, L_guess, tol=1e-4, max_iter=100):
        y, L = y_guess, L_guess
        for _ in range(max_iter):
            y = self.wage_solver(L, y)
            res = self.solver(L, y)
            avg_L_model = 0.5 * (res['L_D_total'] + res['L_S_total'])

            if avg_L_model < 1e-6: return 0.0, y, res
            if abs(L / avg_L_model - 1) < tol: return L, y, res
            
            L = 0.5 * L + 0.5 * avg_L_model
        return L, y, res

    def _default_bisection_bounds(self):
        scale = self.city_size / 10001.0
        return 10_000.0 * scale, 10_000_000.0 * scale

    def wage_bisection(self, L, y_min=0.1, y_max=None, tol=1e-5, max_iter=100):
        if y_max is None: y_min, y_max = 0.1, 20.0
        for _ in range(max_iter):
            y_mid = (y_min + y_max) / 2.0
            res = self.solver(L, y_mid)
            if res['L_S_total'] < 1e-6 and res['L_D_total'] < 1e-6: return y_mid
            diff = res['L_D_total'] - res['L_S_total']
            if abs(diff) / max(1e-6, res['L_S_total']) < tol or (y_max - y_min) < 1e-7: return y_mid
            if diff > 0: y_min = y_mid
            else: y_max = y_mid
        return (y_min + y_max) / 2.0

    def find_eq_bisection(self, L_min=None, L_max=None, tol=1e-4, max_iter=100):
        if L_min is None or L_max is None: L_min, L_max = self._default_bisection_bounds()
        y_eq = 2.5
        for _ in range(max_iter):
            L_mid = (L_min + L_max) / 2.0
            y_eq = self.wage_bisection(L_mid)
            res = self.solver(L_mid, y_eq)
            avg_L_model = 0.5 * (res['L_D_total'] + res['L_S_total'])
            if avg_L_model < 1e-6:
                L_max = L_mid
                continue
            diff = avg_L_model - L_mid
            if abs(diff) / L_mid < tol: return L_mid, y_eq, res
            if diff > 0: L_min = L_mid
            else: L_max = L_mid
        return (L_min + L_max) / 2.0, y_eq, self.solver((L_min + L_max) / 2.0, y_eq)


def main():
    """Main execution function - compare all four methods."""
    model = MODEL(city_size=10_001)

    y_guess = 2.5
    L_guess = 1_000_000
    
    # 1. Test SIMPLE method
    print("\n" + "="*60)
    print("1. Testing SIMPLE method: (L_D/L_S)^0.01")
    print("="*60)
    start_simple = time.time()
    L_simple, y_simple, res_simple = model.find_eq_simple(y_guess, L_guess)
    time_simple = time.time() - start_simple
    
    print(f"Time elapsed: {time_simple:.3f} seconds")
    print(f"Labour Demand: {res_simple['L_D_total']:.2f}")
    print(f"Labour Supply: {res_simple['L_S_total']:.2f}")
    print(f"Equilibrium Wage: {y_simple:.4f}")
    print(f"Equilibrium Employment: {L_simple:.2f}")

    # 2. Test BISECTION method
    print("\n" + "="*60)
    print("2. Testing BISECTION method: Binary Search")
    print("="*60)
    start_bisection = time.time()
    L_bisect, y_bisect, res_bisect = model.find_eq_bisection()
    time_bisection = time.time() - start_bisection
    
    print(f"Time elapsed: {time_bisection:.3f} seconds")
    print(f"Labour Demand: {res_bisect['L_D_total']:.2f}")
    print(f"Labour Supply: {res_bisect['L_S_total']:.2f}")
    print(f"Equilibrium Wage: {y_bisect:.4f}")
    print(f"Equilibrium Employment: {L_bisect:.2f}")

    # 3. Test Advanced Revised Method:
    print("\n" + "="*60)
    print("3. Testing Advanced method: Binary Search")
    print("="*60)
    start_adv = time.time()
    L_adv, y_adv, res_adv = model.find_eq_adv(L_guess, y_guess)
    time_adv = time.time() - start_adv
    
    print(f"Time elapsed: {time_adv:.3f} seconds")
    print(f"Labour Demand: {res_adv['L_D_total']:.2f}")
    print(f"Labour Supply: {res_adv['L_S_total']:.2f}")
    print(f"Equilibrium Wage: {y_adv:.4f}")
    print(f"Equilibrium Employment: {L_adv:.2f}")

    # Final Comparison
    print("\n" + '='*60)
    print("PERFORMANCE COMPARISON")
    print("="*60)
    print(f"Simple time:           {time_simple:.3f} s")
    print(f"Bisection time:        {time_bisection:.3f} s")
    print(f"Advanced time:         {time_adv:.3f} s")

if __name__ == "__main__":
    main()