from MODEL import MODEL

m = MODEL()
p = m.params
y = 2.5
L = 1_000_000
r = m.solver(L, y)

L_x = p['alpha_C'] / (1- p['alpha_C']) * r['p_C_bid'] * r['S_star_C'] / y
n_x = 1 / (1- p['alpha_R']) * r['S_star_R'] * r['p_R_bid'] 

x_0 = r['x_0']
x_1 = r['x_1']
x = r['x']

out_l = (x>=-x_1)&(x<=-x_0) 
out_r = (x>= x_0)&(x<= x_1) 
in_c = (x>=-x_0)&(x<=x_0)

print(f"Sum L_x in C (Real Demand): {sum(L_x[in_c])}")
print(f"Sum L_x in R (Virtual Demand): {sum(L_x[out_l]) + sum(L_x[out_r])}")
print(f"Sum n_x in R (Real Supply * y): {sum(n_x[out_l]) + sum(n_x[out_r])}")
print(f"Sum n_x in C (Virtual Supply * y): {sum(n_x[in_c])}")
