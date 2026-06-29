import numpy as np
from numba import njit

@njit
def geo_mean(arr):
    # use log's to manage compute
    return np.exp(np.mean(np.log(arr))) 
    
@njit
def solver(A, H, dist, border_penalty,C_m,LL_c, params, tolerance = 1e-3):
    """
    A: (N x 1) productivity of each municipality
    H: (N x 1) inelastic housing stock of each municipality
    dist: (N X N) Distances between all municipalties
    C_m: (N x C) Matrix containg information to which country the municiaplity belongs
    LL_c: (C x 1) population in each country
    params: [sigma, alpha] list of parameters
    tolerance: parametr that determine when we reached the equilibrium
    """
    N = len(A) # number of observations
    C = len(LL_c)

    # unpacking the paramter list
    sigma   = params[0] 
    alpha   = params[1]

    # calulate constants done for efficency
    exp_1 = (-alpha)/(sigma-1)
    exp_2 = (sigma-1)/(sigma*(1-alpha)-1)
    exp_3 = 1/(sigma-1) 

    # init the endogenours variables
    W,L = np.ones(N), np.dot(C_m,LL_c/np.sum(C_m,axis=0))

    trade_cost = (dist*border_penalty)**(1-sigma)
    for _ in range(200_000): # set maximum number of iterations
        
        # calulate the Tradeshare: pi using (Slide 26)
        num = trade_cost*(L * (W/A)**(1-sigma))
        denom = np.sum(num, axis=1).reshape((N, 1)) 
        pi = num / denom

        # calculate income and expenditure Slide(27)
        income = W*L
        expenditure = np.dot(income,pi) 

        # calcualte population mobility: pop_mob equation (Slide 28)
        num = (A**alpha * H**(1-alpha) * np.diag(pi)**exp_1)**exp_2
        # note (C_m @ C_m.T) @ numerator takes longer than C_m @ (numerator @ ).t 
        L_new = (num/ np.dot(C_m,np.dot(num,C_m))) * np.dot(C_m,LL_c)

        if (np.abs( (L_new/L) - 1) < tolerance).all() \
            and (np.abs( (income/expenditure) - 1) < tolerance).all():
            return W, L, pi
        else:
            # update L
            L = 0.75* L + 0.25*L_new

            # update W
            W_new = W*(expenditure / income)**exp_3 
            W = 0.75* W + 0.25*W_new
            # Normalize all wages by the geometric mean of first Country
            W = W / geo_mean(W[C_m[:, 0] == 1])
    print('Values did not converge')
    return W, L, pi
        

if __name__ == '__main__':
    solver()