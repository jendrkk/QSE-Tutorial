import numpy as np
from numba import njit, prange

@njit
def jit_solver(dist, eps_C, eps_R, L, y, params_arr):
    """
    JIT-compiled spatial equilibrium solver.
    params_arr: [alpha_C, alpha_R, theta_C, theta_R, omega_C, omega_R, beta_C, beta_R, a_bar_C, a_bar_R, tau_C, tau_R, c_C, c_R, r_a, S_bar_C, S_bar_R, x_1_bar]
    """
    (alpha_C, alpha_R, theta_C, theta_R, omega_C, omega_R, beta_C, beta_R, 
     a_bar_C, a_bar_R, tau_C, tau_R, c_C, c_R, r_a, S_bar_C, S_bar_R, x_1_bar) = params_arr

    # 1.1 Shifters
    A_tilde_C = a_bar_C * eps_C * (L**beta_C) * np.exp(-tau_C * dist)
    A_tilde_R = a_bar_R * eps_R * (L**beta_R) * np.exp(-tau_R * dist)
    
    a_C = A_tilde_C**(1/(1-alpha_C)) * (y**(alpha_C/(alpha_C-1)))
    a_R = A_tilde_R**(1/(1-alpha_R)) * (y**(1/(1-alpha_R)))
    
    # 1.2 Land Rents
    # S* = (a / (c(1+theta)))^(1/(theta-omega))
    # r = a/(1+omega) * S*^(1+omega) - c * S*^(1+theta)
    
    S_star_C = (a_C / (c_C * (1 + theta_C)))**(1 / (theta_C - omega_C))
    r_C = (a_C / (1 + omega_C)) * (S_star_C**(1 + omega_C)) - c_C * (S_star_C**(1 + theta_C))
    
    S_star_R = (a_R / (c_R * (1 + theta_R)))**(1 / (theta_R - omega_R))
    r_R = (a_R / (1 + omega_R)) * (S_star_R**(1 + omega_R)) - c_R * (S_star_R**(1 + theta_R))
    
    # Land Use Decision
    n = len(dist)
    L_D_total = 0.0
    L_S_total = 0.0
    
    for i in range(n):
        if dist[i] > x_1_bar:
            continue
            
        # Determine winning use
        if r_C[i] >= r_R[i] and r_C[i] > r_a:
            # Commercial
            S = min(S_bar_C, S_star_C[i])
            p_bid = a_C[i] * (1 / (1 - omega_C)) * (S**omega_C)
            L_D_total += (alpha_C / (1 - alpha_C)) * (p_bid / y) * S
        elif r_R[i] > r_C[i] and r_R[i] > r_a:
            # Residential
            S = min(S_bar_R, S_star_R[i])
            p_bid = a_R[i] * (1 / (1 - omega_R)) * (S**omega_R)
            L_S_total += (S * p_bid) / ((1 - alpha_R) * y)
            
    return L_D_total, L_S_total

@njit
def jit_find_wage(dist, eps_C, eps_R, L, y_guess, params_arr, tolerance=1e-6):
    """
    JIT-compiled wage clearing loop using dampened fixed-point iteration.
    """
    y = y_guess
    for _ in range(200):
        L_D, L_S = jit_solver(dist, eps_C, eps_R, L, y, params_arr)
        
        if L_S <= 1e-9:
            if L_D <= 1e-9: return y
            y *= 1.1 # Increase wage to reduce demand/attract supply
        elif L_D <= 1e-9:
            y *= 0.9 # Decrease wage
        else:
            ratio = L_D / L_S
            if abs(ratio - 1.0) < tolerance:
                return y
            y_factor = ratio**0.01
            y = 0.5 * y + 0.5 * y * y_factor
            
    return y

@njit
def jit_find_eq(dist, eps_C, eps_R, L_guess, y_guess, params_arr, tolerance=1e-4):
    """
    JIT-compiled general equilibrium loop.
    """
    L = L_guess
    y = y_guess
    
    for _ in range(100):
        y = jit_find_wage(dist, eps_C, eps_R, L, y, params_arr)
        L_D, L_S = jit_solver(dist, eps_C, eps_R, L, y, params_arr)
        
        avg_L = 0.5 * (L_D + L_S)
        if avg_L <= 1e-6:
            return 0.0, y
            
        if abs(L / avg_L - 1.0) < tolerance:
            return avg_L, y
            
        L = 0.5 * L + 0.5 * avg_L
        
    return L, y

from concurrent.futures import ProcessPoolExecutor
import functools

def _sweep_worker(args):
    """Internal worker for parallel sweeps."""
    param_name, value, base_params = args
    current_params = base_params.copy()
    current_params[param_name] = value
    
    # Heuristic for residential cost if only commercial is varied
    if param_name == 'theta_C' and 'theta_R' not in base_params:
        current_params['theta_R'] = value + 0.05
        
    try:
        model = AB2022JITModel(current_params)
        # We check if the user wants an Open City or Closed City sweep. 
        # For now, default to Open City as in find_eq
        L_eq, y_eq, res = model.find_eq(L_guess=1000000)
        
        U = res['U']
        dist = res['dist']
        S_x = res['S_x']
        
        # Radii
        cbd_radius = np.max(dist[U == 1]) if np.any(U == 1) else 0.0
        urban_radius = np.max(dist[U < 3]) if np.any(U < 3) else 0.0
        
        # Max Heights per use
        max_S_C = np.max(res['S_C'][U == 1]) if np.any(U == 1) else 0.0
        max_S_R = np.max(res['S_R'][U == 2]) if np.any(U == 2) else 0.0
        
        # Max Floor Space Rents per use
        max_p_C = np.max(res['p_C_bid'][U == 1]) if np.any(U == 1) else 0.0
        max_p_R = np.max(res['p_R_bid'][U == 2]) if np.any(U == 2) else 0.0
        
        # Max Land Rents per use
        max_r_C = np.max(res['r_C'][U == 1]) if np.any(U == 1) else 0.0
        max_r_R = np.max(res['r_R'][U == 2]) if np.any(U == 2) else 0.0
        
        # Land Rent and Floor Rent Envelopes
        r_env = np.maximum(np.maximum(res['r_C'], res['r_R']), current_params['r_a'])
        p_env = np.where(U == 1, res['p_C_bid'], np.where(U == 2, res['p_R_bid'], 0.0))
        
        return {
            'value': value, 'L': L_eq, 'y': y_eq,
            'cbd_radius': cbd_radius, 'urban_radius': urban_radius,
            'max_S_C': max_S_C, 'max_S_R': max_S_R,
            'max_p_C': max_p_C, 'max_p_R': max_p_R,
            'max_r_C': max_r_C, 'max_r_R': max_r_R,
            'heights': S_x, 'land_rents': r_env, 'floor_rents': p_env,
            'success': True
        }
    except Exception as e:
        return {'value': value, 'success': False, 'error': str(e)}

class AB2022JITModel:
    @staticmethod
    def run_parallel_sweep(param_name, param_values, base_params, max_workers=None):
        """
        Runs a parallel parameter sweep using all available CPU cores.
        Returns a dictionary of results.
        """
        tasks = [(param_name, v, base_params) for v in param_values]
        with ProcessPoolExecutor(max_workers=max_workers) as executor:
            raw_results = list(executor.map(_sweep_worker, tasks))
        
        valid = [r for r in raw_results if r['success']]
        if not valid:
            return None
            
        # Compile into structured dictionary
        return {
            'param_values': np.array([r['value'] for r in valid]),
            'L': np.array([r['L'] for r in valid]),
            'y': np.array([r['y'] for r in valid]),
            'cbd_radius': np.array([r['cbd_radius'] for r in valid]),
            'urban_radius': np.array([r['urban_radius'] for r in valid]),
            'max_S_C': np.array([r['max_S_C'] for r in valid]),
            'max_S_R': np.array([r['max_S_R'] for r in valid]),
            'max_p_C': np.array([r['max_p_C'] for r in valid]),
            'max_p_R': np.array([r['max_p_R'] for r in valid]),
            'max_r_C': np.array([r['max_r_C'] for r in valid]),
            'max_r_R': np.array([r['max_r_R'] for r in valid]),
            'height_matrix': np.array([r['heights'] for r in valid]),
            'land_rent_matrix': np.array([r['land_rents'] for r in valid]),
            'floor_rent_matrix': np.array([r['floor_rents'] for r in valid])
        }

    def __init__(self, params=None):
        self.params = {
            'alpha_C': 0.85, 'alpha_R': 0.66, 'theta_C': 0.5, 'theta_R': 0.55,
            'omega_C': 0.03, 'omega_R': 0.07, 'beta_C': 0.03, 'beta_R': 0.0,
            'a_bar_C': 2.0, 'a_bar_R': 1.0, 'tau_C': 0.01, 'tau_R': 0.005,
            'c_C': 1.4, 'c_R': 1.4, 'r_a': 150.0, 'S_bar_C': 999.0, 'S_bar_R': 999.0, 'x_1_bar': 999.0
        }
        if params: self.params.update(params)
        
        self.x = np.linspace(-50, 50, 10001)
        self.dist = np.abs(self.x)
        self.eps_C = np.ones_like(self.x)
        self.eps_R = np.ones_like(self.x)
        
        self.update_params_arr()

    def update_params_arr(self):
        p = self.params
        self.params_arr = np.array([
            p['alpha_C'], p['alpha_R'], p['theta_C'], p['theta_R'], 
            p['omega_C'], p['omega_R'], p['beta_C'], p['beta_R'],
            p['a_bar_C'], p['a_bar_R'], p['tau_C'], p['tau_R'],
            p['c_C'], p['c_R'], p['r_a'], p['S_bar_C'], p['S_bar_R'], p['x_1_bar']
        ], dtype=np.float64)

    def find_wage(self, L, y_guess=2.5):
        return jit_find_wage(self.dist, self.eps_C, self.eps_R, L, y_guess, self.params_arr)

    def find_eq(self, L_guess=1000000, y_guess=2.5):
        L, y = jit_find_eq(self.dist, self.eps_C, self.eps_R, L_guess, y_guess, self.params_arr)
        # Return L, y, and the full results from the solver for the equilibrium state
        return L, y, self.solver(L, y)

    def solver(self, L, y):
        # To support plotting, we provide a full results dictionary.
        # We'll use a standard numpy calculation for the full spatial arrays
        # so we don't have to manage complex dictionary returns in Numba.
        p = self.params
        dist = self.dist
        
        A_tilde_C = p['a_bar_C'] * self.eps_C * (L**p['beta_C']) * np.exp(-p['tau_C'] * dist)
        A_tilde_R = p['a_bar_R'] * self.eps_R * (L**p['beta_R']) * np.exp(-p['tau_R'] * dist)
        
        a_C = A_tilde_C**(1/(1-p['alpha_C'])) * (y**(p['alpha_C']/(p['alpha_C']-1)))
        a_R = A_tilde_R**(1/(1-p['alpha_R'])) * (y**(1/(1-p['alpha_R'])))
        
        S_star_C = (a_C / (p['c_C'] * (1 + p['theta_C'])))**(1 / (p['theta_C'] - p['omega_C']))
        r_C = (a_C / (1 + p['omega_C'])) * (S_star_C**(1 + p['omega_C'])) - p['c_C'] * (S_star_C**(1 + p['theta_C']))
        
        S_star_R = (a_R / (p['c_R'] * (1 + p['theta_R'])))**(1 / (p['theta_R'] - p['omega_R']))
        r_R = (a_R / (1 + p['omega_R'])) * (S_star_R**(1 + p['omega_R'])) - p['c_R'] * (S_star_R**(1 + p['theta_R']))
        
        U = np.ones_like(dist) * 3
        U[(r_R > r_C) & (r_R > p['r_a'])] = 2
        U[(r_C >= r_R) & (r_C > p['r_a'])] = 1
        U[dist > p['x_1_bar']] = 3
        
        S_C = np.minimum(p['S_bar_C'], S_star_C)
        S_R = np.minimum(p['S_bar_R'], S_star_R)
        
        p_C_bid = a_C * (1 / (1 - p['omega_C'])) * (S_C**p['omega_C'])
        p_R_bid = a_R * (1 / (1 - p['omega_R'])) * (S_R**p['omega_R'])
        
        L_D_loc = np.zeros_like(dist)
        L_D_loc[U == 1] = (p['alpha_C'] / (1 - p['alpha_C'])) * (p_C_bid[U == 1] / y) * S_C[U == 1]
        
        L_S_loc = np.zeros_like(dist)
        L_S_loc[U == 2] = (S_R[U == 2] * p_R_bid[U == 2]) / ((1 - p['alpha_R']) * y)
        
        results = {
            'L_D_total': np.sum(L_D_loc),
            'L_S_total': np.sum(L_S_loc),
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
            'dist': dist
        }
        return results
