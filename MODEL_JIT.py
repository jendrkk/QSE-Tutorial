import numpy as np
import time
from numba import njit, float64, int64

# JIT-friendly parameter layout
PARAM_ORDER = (
    'alpha_C', 'alpha_R', 'theta_C', 'theta_R',
    'omega_C', 'omega_R', 'beta_C', 'beta_R',
    'a_bar_C', 'a_bar_R', 'tau_C', 'tau_R',
    'c_C', 'c_R', 'r_a', 'S_bar_C',
    'S_bar_R', 'x_1_bar'
)

@njit
def solver_jit(params, x, dist, precalc, L, y, 
               U, S_C, S_R, p_C_bid, p_R_bid, r_C_arr, r_R_arr, S_star_C_arr, S_star_R_arr):
    alpha_C, alpha_R, theta_C, theta_R, omega_C, omega_R, beta_C, beta_R, \
    a_bar_C, a_bar_R, tau_C, tau_R, c_C, c_R, r_a, S_bar_C, S_bar_R, x_1_bar = params

    y_safe = max(y, 1e-10)
    
    # Constants for this (L, y)
    H_C = (a_bar_C * (L**beta_C))**(1.0/(1.0-alpha_C)) * (y_safe**(alpha_C/(alpha_C-1.0)))
    H_R = (a_bar_R * (L**beta_R))**(1.0/(1.0-alpha_R)) * (y_safe**(1.0/(1.0-alpha_R)))

    K_C = (H_C / (c_C * (1.0 + theta_C)))**(1.0 / (theta_C - omega_C))
    K_R = (H_R / (c_R * (1.0 + theta_R)))**(1.0 / (theta_R - omega_R))

    # Factor for rent when S_star < S_bar
    R_base_C = H_C * (1.0 / (1.0 + omega_C)) * (K_C**(1.0 + omega_C)) - c_C * (K_C**(1.0 + theta_C))
    R_base_R = H_R * (1.0 / (1.0 + omega_R)) * (K_R**(1.0 + omega_R)) - c_R * (K_R**(1.0 + theta_R))

    # Factor for bid when S_star < S_bar
    P_base_C = H_C * (1.0 / (1.0 - omega_C)) * (K_C**omega_C)
    P_base_R = H_R * (1.0 / (1.0 - omega_R)) * (K_R**omega_R)

    # Pre-calculated spatial arrays (V_C, V_R, W_C, W_R, r_spatial_C, r_spatial_R, p_spatial_C, p_spatial_R)
    V_C, V_R, W_C, W_R, RS_C, RS_R, PS_C, PS_R = precalc

    L_D_total = 0.0
    L_S_total = 0.0
    x_0, x_1 = np.inf, np.inf
    n = len(x)

    for i in range(n):
        s_st_C = K_C * W_C[i]
        s_st_R = K_R * W_R[i]
        
        if s_st_C <= S_bar_C:
            r_C = R_base_C * RS_C[i]
        else:
            r_C = (H_C * V_C[i]) * (1.0/(1.0+omega_C)) * (S_bar_C**(1.0+omega_C)) - c_C * (S_bar_C**(1.0+theta_C))
            
        if s_st_R <= S_bar_R:
            r_R = R_base_R * RS_R[i]
        else:
            r_R = (H_R * V_R[i]) * (1.0/(1.0+omega_R)) * (S_bar_R**(1.0+omega_R)) - c_R * (S_bar_R**(1.0+theta_R))

        use = 3
        if dist[i] <= x_1_bar:
            if r_C >= r_R and r_C > r_a: use = 1
            elif r_R > r_C and r_R > r_a: use = 2
        
        U[i], r_C_arr[i], r_R_arr[i] = use, r_C, r_R
        S_star_C_arr[i], S_star_R_arr[i] = s_st_C, s_st_R

        sc, sr = min(S_bar_C, s_st_C), min(S_bar_R, s_st_R)
        S_C[i], S_R[i] = sc, sr
        
        if sc == s_st_C: pc = P_base_C * PS_C[i]
        else: pc = (H_C * V_C[i]) * (1.0 / (1.0 - omega_C)) * (S_bar_C**omega_C)
        
        if sr == s_st_R: pr = P_base_R * PS_R[i]
        else: pr = (H_R * V_R[i]) * (1.0 / (1.0 - omega_R)) * (S_bar_R**omega_R)
        
        p_C_bid[i], p_R_bid[i] = pc, pr

        if use == 1: L_D_total += (alpha_C / (1.0 - alpha_C)) * (pc / y_safe) * sc
        elif use == 2: L_S_total += (sr * pr) / ((1.0 - alpha_R) * y_safe)

        if x[i] >= 0:
            if use == 3 and x[i] < x_1: x_1 = x[i]
            if use != 1 and x[i] < x_0: x_0 = x[i]

    return L_D_total, L_S_total, x_0, x_1

@njit
def wage_bisection_jit(params, x, dist, precalc, L, y_min, y_max, buffers, tol=1e-7, max_iter=100):
    for _ in range(max_iter):
        y_mid = 0.5 * (y_min + y_max)
        ld, ls, x0, x1 = solver_jit(params, x, dist, precalc, L, y_mid, *buffers)
        if ls < 1e-10 and ld < 1e-10: return y_mid
        diff = ld - ls
        if np.abs(diff) / max(1e-6, ls) < tol or (y_max - y_min) < 1e-9: return y_mid
        if diff > 0: y_min = y_mid
        else: y_max = y_mid
    return 0.5 * (y_min + y_max)

@njit
def find_eq_bisection_jit_npy(params, x, dist, precalc, L_min, L_max, buffers, tol=1e-4, max_iter=100):
    y_eq = 2.5
    for _ in range(max_iter):
        L_mid = 0.5 * (L_min + L_max)
        y_eq = wage_bisection_jit(params, x, dist, precalc, L_mid, 0.1, 50.0, buffers)
        ld, ls, x0, x1 = solver_jit(params, x, dist, precalc, L_mid, y_eq, *buffers)
        avg_L = 0.5 * (ld + ls)
        if avg_L < 1e-6:
            L_max = L_mid
            continue
        diff = avg_L - L_mid
        if np.abs(diff) / L_mid < tol: return L_mid, y_eq, ld, ls, x0, x1
        if diff > 0: L_min = L_mid
        else: L_max = L_mid
    return (L_min + L_max)/2.0, y_eq, ld, ls, x0, x1

@njit
def wage_solver_jit(params, x, dist, precalc, L, y, method, buffers, tol=1e-7, max_iter=500):
    if method == 2: # Bisection
        return wage_bisection_jit(params, x, dist, precalc, L, 0.1, 50.0, buffers, tol)
    
    y_prev = y * 1.05
    for _ in range(max_iter):
        ld, ls, x0, x1 = solver_jit(params, x, dist, precalc, L, y, *buffers)
        if ls < 1e-6 and ld < 1e-6: return y
        diff = ld - ls
        if np.abs(diff) / max(1e-6, ls) < tol: return y
        
        # Simple method
        y_fac = (ld / ls) ** 0.01 if ls > 1e-6 else 1.2
        y_new = 0.5 * y + 0.5 * y * y_fac
        
        y_prev = y
        y = max(1e-5, min(y_new, 100.0))
        if np.abs(y/y_prev - 1.0) < tol: return y
    return y

@njit
def find_eq_jit_npy(params, x, dist, precalc, y_guess, L_guess, wage_method, buffers):
    y, L = y_guess, L_guess
    for _ in range(100):
        y = wage_solver_jit(params, x, dist, precalc, L, y, wage_method, buffers)
        ld, ls, x0, x1 = solver_jit(params, x, dist, precalc, L, y, *buffers)
        avg_L = 0.5 * (ld + ls)
        if avg_L < 1e-6: return 0.0, y, ld, ls, x0, x1
        if np.abs(L / avg_L - 1.0) < 1e-4: return L, y, ld, ls, x0, x1
        L = 0.5 * L + 0.5 * avg_L
    ld, ls, x0, x1 = solver_jit(params, x, dist, precalc, L, y, *buffers)
    return L, y, ld, ls, x0, x1

@njit
def find_eq_adv_jit_npy(params, x, dist, precalc, L_guess, y_guess, buffers, tol=1e-4, max_iter=2000):
    alpha_C = params[0]
    y, L = y_guess, L_guess
    omega_N, omega_y = 0.05, 0.01
    
    ld, ls, x0, x1 = 0.0, 0.0, 0.0, 0.0
    
    for _ in range(max_iter):
        ld, ls, x0, x1 = solver_jit(params, x, dist, precalc, L, y, *buffers)
        N_hat = ls
        
        U, S_C, p_C_bid = buffers[0], buffers[1], buffers[3]
        wage_bill_integral = 0.0
        for i in range(len(U)):
            if U[i] == 1:
                wage_bill_integral += p_C_bid[i] * S_C[i]
        
        y_hat = (alpha_C / (1.0 - alpha_C)) * wage_bill_integral / N_hat if N_hat > 1e-10 else 0.0
            
        if np.abs(L/max(N_hat, 1e-10) - 1.0) < tol and np.abs(y/max(y_hat, 1e-10) - 1.0) < tol:
            return N_hat, y_hat, ld, ls, x0, x1
            
        y = omega_y * y_hat + (1.0 - omega_y) * y
        L = omega_N * N_hat + (1.0 - omega_N) * L
        y = max(1e-5, min(y, 100.0))
        L = max(1e-5, L)
        
    return L, y, ld, ls, x0, x1

class MODEL_JIT:
    def __init__(self, params=None, city_size=10001):
        if params is None: params = {}
        if 'city_size' in params: city_size = int(params.pop('city_size'))

        self.params = {
            'alpha_C': 0.85, 'alpha_R': 0.66, 'theta_C': 0.5, 'theta_R': 0.55,
            'omega_C': 0.03, 'omega_R': 0.07, 'beta_C': 0.03, 'beta_R': 0.0,
            'a_bar_C': 2.0, 'a_bar_R': 1.0, 'tau_C': 0.01, 'tau_R': 0.005,
            'c_C': 1.4, 'c_R': 1.4, 'r_a': 30.0, 'S_bar_C': 999.0,
            'S_bar_R': 999.0, 'x_1_bar': 999.0,
        }
        self.params.update(params)
        self.city_size = int(city_size)
        p = self.params
        self.params_tuple = tuple(p[name] for name in PARAM_ORDER)

        self.x = np.linspace(-50.0, 50.0, self.city_size)
        self.dist = np.abs(self.x)
        
        # Pre-calculate spatial power components
        V_C = np.exp(-p['tau_C'] * self.dist / (1.0 - p['alpha_C']))
        V_R = np.exp(-p['tau_R'] * self.dist / (1.0 - p['alpha_R']))
        W_C = V_C**(1.0 / (p['theta_C'] - p['omega_C']))
        W_R = V_R**(1.0 / (p['theta_R'] - p['omega_R']))
        RS_C = W_C**(1.0 + p['theta_C'])
        RS_R = W_R**(1.0 + p['theta_R'])
        PS_C = V_C * (W_C**p['omega_C'])
        PS_R = V_R * (W_R**p['omega_R'])

        self._precalc = (V_C, V_R, W_C, W_R, RS_C, RS_R, PS_C, PS_R)

        # Pre-allocate buffers
        self._buffers = (
            np.zeros(self.city_size, dtype=np.int64),   # U
            np.zeros(self.city_size, dtype=np.float64), # S_C
            np.zeros(self.city_size, dtype=np.float64), # S_R
            np.zeros(self.city_size, dtype=np.float64), # p_C_bid
            np.zeros(self.city_size, dtype=np.float64), # p_R_bid
            np.zeros(self.city_size, dtype=np.float64), # r_C_arr
            np.zeros(self.city_size, dtype=np.float64), # r_R_arr
            np.zeros(self.city_size, dtype=np.float64), # S_star_C_arr
            np.zeros(self.city_size, dtype=np.float64)  # S_star_R_arr
        )

    def _unpack(self, ld, ls, x0, x1):
        U, S_C, S_R = self._buffers[0], self._buffers[1], self._buffers[2]
        return {
            'L_D_total': ld, 'L_S_total': ls, 'U': U.copy(),
            'S_C': S_C.copy(), 'S_R': S_R.copy(),
            'S_x': np.where(U == 1, S_C, np.where(U == 2, S_R, 0.0)),
            'p_C_bid': self._buffers[3].copy(), 'p_R_bid': self._buffers[4].copy(),
            'r_C': self._buffers[5].copy(), 'r_R': self._buffers[6].copy(),
            'S_star_C': self._buffers[7].copy(), 'S_star_R': self._buffers[8].copy(),
            'x_0': x0, 'x_1': x1, 'x': self.x, 'dist': self.dist,
        }

    def solver(self, L, y):
        ld, ls, x0, x1 = solver_jit(self.params_tuple, self.x, self.dist, self._precalc, L, y, *self._buffers)
        return self._unpack(ld, ls, x0, x1)

    def find_eq_simple_jit(self, y_guess, L_guess):
        L, y, ld, ls, x0, x1 = find_eq_jit_npy(self.params_tuple, self.x, self.dist, self._precalc, y_guess, L_guess, 0, self._buffers)
        return L, y, self._unpack(ld, ls, x0, x1)

    def find_eq_adv_jit(self, L_guess, y_guess):
        L, y, ld, ls, x0, x1 = find_eq_adv_jit_npy(self.params_tuple, self.x, self.dist, self._precalc, L_guess, y_guess, self._buffers)
        return L, y, self._unpack(ld, ls, x0, x1)

    def find_eq_bisection_jit(self, L_min=None, L_max=None, tol=1e-4, max_iter=100):
        if L_min is None or L_max is None:
            scale = self.city_size / 10001.0
            L_min, L_max = 10000.0 * scale, 10000000.0 * scale
        L_mid, y_eq, ld, ls, x0, x1 = find_eq_bisection_jit_npy(self.params_tuple, self.x, self.dist, self._precalc, L_min, L_max, self._buffers, tol, max_iter)
        return L_mid, y_eq, self._unpack(ld, ls, x0, x1)

def main():
    model = MODEL_JIT(city_size=100_001)
    model.find_eq_simple_jit(2.5, 1_000_000) # Warmup
    model.find_eq_adv_jit(1_000_000, 2.5) # Warmup
    
    start = time.time()
    L, y, res = model.find_eq_simple_jit(2.5, 1_000_000)
    print(f"JIT Optimized (Simple): {time.time()-start:.4f}s, L={L:.2f}, y={y:.4f}")

    start = time.time()
    L, y, res = model.find_eq_adv_jit(1_000_000, 2.5)
    print(f"JIT Optimized (Advanced): {time.time()-start:.4f}s, L={L:.2f}, y={y:.4f}")

if __name__ == '__main__':
    main()
