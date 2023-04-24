###############################################################################
# -*- coding: utf-8 -*-
###############################################################################
# Senior Thesis - Optimization
###############################################################################
"""
This code runs the simulation in my thesis---The Implications of Carbon 
Pricing for Environmental Inequality.
"""
###############################################################################
# Contributor(s): Evan Perry
# Last Revised: 2023-04-03
# Version = 1
###############################################################################


###############################################################################
# Import Packages & Setup
###############################################################################

import numpy as np
import pandas as pd
from scipy.optimize import minimize
from itertools import combinations
from os import chdir

# Set working directory
chdir("C:/Users/eaper/Senior Thesis/seniorThesis")

###############################################################################


###############################################################################
# Model Data Setup
###############################################################################

# Read in the data
plants = pd.read_csv("Results/kmeans-plants.csv")
demand = pd.read_csv("Results/kmeans-demand.csv")
lines = pd.read_csv("Data/main/transmission/tlines.csv")
power_fac = pd.read_csv("Data/main/transmission/factors.csv")

# Define the model
N = 30
R = 3

# Create a dictionary mappying generators to their region
plants_loc = {i : plants["region"][i] for i in range(len(plants.index))}

# Create a vector with the added operating costs per MWh
# $3/MWh for gas and oil, $5/MWh for coal
op_cost = {"Oil": 3, "Gas": 3, "Coal": 5}
op_cost = [op_cost[i] for i in plants["fuel_cat"]]

# Lines: Cal-NW, Cal-SW, NW-SW
trans_cap = [lines["tcap"][0],
             lines["tcap"][1],
             lines["tcap"][3] + lines["tcap"][4]]
    
# Power transfer distribution factors (same line order)
trans_fac = power_fac.drop(columns = ["24", "34"])
trans_fac = trans_fac.drop([2], axis=0)

dis_fctr = (1/(1 + 0.0149162234))**(1/365)

###############################################################################


###############################################################################
# Generation Phase
###############################################################################

def generation_sim(U, D, j):
    
    # U : Unit cost matrix ($/mmBTU); associated with the BCA scenario
    # D : 1-d numpy array of the demand needed at period t in each region
    # j : Investment scenario
    
    ###########################################################################
    ## Preliminaries
    
    # Supply
    supply0 = np.array([1]*N + [0]*(2*N))
    supply1 = np.array([0]*N + [1]*N + [0]*N)
    supply2 = np.array([0]*(2*N) + [1]*N)
    
    # Capacity
    caps = plants["plant_count"]
    
    # Export vectors
    export_ind = []
    
    for r in range(R):
        temp = np.zeros(shape = (N, R))
        
        for i in range(N):
            if plants_loc[i] == r:
                temp[i,] = np.ones(shape = R)
        
        temp[:,r] = np.zeros(shape = N)    
        export_ind.append(temp.reshape(N*R, order = "F"))
        
    ###########################################################################
    ## Marginal Cost Construction
    
    j = np.multiply(np.array(j), np.array(plants["j_bar"]))
    D_rho_j = np.array(plants["hrate"] - j)
    D_rho_j = np.diagflat(D_rho_j)
    
    mc = np.matmul(D_rho_j, U)
    mc = mc.reshape(N*R, order = "F") + np.array(op_cost*3)
    
    ###########################################################################
    ## Q Bar
    
    q_bar = 0.9 * np.array(list(plants["mean_capacity"])*R)
    
    ###########################################################################
    ## Cost Function (MC \cdot Q)
    
    def costFunction(A):
        Q = np.multiply(q_bar, A)
        return mc.dot(Q)/10000
    
    ###########################################################################
    ## Bounds (Supply >= 0)
    
    bnds = ((0, None), (0, None), (0, None))*N
    
    ###########################################################################
    ## Demand Constraints (Suppply - Demand >= 0)
    
    def conD_0(A):
        Q = np.multiply(q_bar, A)
        return supply0.dot(Q) - D[0]
        
    def conD_1(A):
        Q = np.multiply(q_bar, A)
        return supply1.dot(Q) - D[1]
    
    def conD_2(A):
        Q = np.multiply(q_bar, A)
        return supply2.dot(Q) - D[2]
    
    demand_cons = [
            {'type': 'ineq', 'fun': conD_0},
            {'type': 'ineq', 'fun': conD_1},
            {'type': 'ineq', 'fun': conD_2}
        ]
    
    ###########################################################################
    ## Capacity Constraints (Capacity - Generation >= 0)
    
    def write_conC(i):
        
        def conC(A):
            prod_i = np.zeros(N)
            prod_i[i] = 1
            prod_i = np.tile(prod_i, R)
            return caps[i] - prod_i.dot(A)
        
        return conC
    
    capacity_cons = [{'type': 'ineq', 'fun': write_conC(i)} for i in range(N)]
    
    ###########################################################################
    ## Transmission Constraints
    
    # A : MaxCap - Sum over region 2 & 3 {PTDF_(line, region) * Exports }
    # B : Sum over region 2 & 3 {PTDF_(line, region) * Exports } - MaxCap
    
    def conT_0A(A):
        Q = np.multiply(q_bar, A)
        p1 = trans_fac.iloc[0,1] * export_ind[1].dot(Q)
        p2 = trans_fac.iloc[1,1] * export_ind[2].dot(Q)
        return trans_cap[0] - p1 - p2
    
    def conT_0B(A):
        Q = np.multiply(q_bar, A)
        p1 = trans_fac.iloc[0,1] * export_ind[1].dot(Q)
        p2 = trans_fac.iloc[1,1] * export_ind[2].dot(Q)
        return  p1 + p2 + trans_cap[0]
    
    def conT_1A(A):
        Q = np.multiply(q_bar, A)
        p1 = trans_fac.iloc[0,2] * export_ind[1].dot(Q)
        p2 = trans_fac.iloc[1,2] * export_ind[2].dot(Q)
        return trans_cap[1] - p1 - p2
    
    def conT_1B(A):
        Q = np.multiply(q_bar, A)
        p1 = trans_fac.iloc[0,2] * export_ind[1].dot(Q)
        p2 = trans_fac.iloc[1,2] * export_ind[2].dot(Q)
        return  p1 + p2 + trans_cap[1]
    
    def conT_2A(A):
        Q = np.multiply(q_bar, A)
        p1 = trans_fac.iloc[0,3] * export_ind[1].dot(Q)
        p2 = trans_fac.iloc[1,3] * export_ind[2].dot(Q)
        return trans_cap[2] - p1 - p2
    
    def conT_2B(A):
        Q = np.multiply(q_bar, A)
        p1 = trans_fac.iloc[0,3] * export_ind[1].dot(Q)
        p2 = trans_fac.iloc[1,3] * export_ind[2].dot(Q)
        return  p1 + p2 + trans_cap[2]
    
    trans_cons = [
            {'type': 'ineq', 'fun': conT_0A},
            {'type': 'ineq', 'fun': conT_0B},
            {'type': 'ineq', 'fun': conT_1A},
            {'type': 'ineq', 'fun': conT_1B},
            {'type': 'ineq', 'fun': conT_2A},
            {'type': 'ineq', 'fun': conT_2B}
        ]
    
    ###########################################################################
    ## Intial Guess
    
    A0 = np.zeros(shape = (N, R))
    
    for i in range(N):
        for r in range(R):
            if plants_loc[i] == r:
                A0[i,r] = caps[i]
            
    A0 = A0.reshape(N*R, order="F")
    
    ###########################################################################
    ## Optimize
    
    all_cons = demand_cons + capacity_cons + trans_cons
    
    mod_results = minimize(
            costFunction, A0, constraints = all_cons, bounds= bnds, 
            method='SLSQP', tol = 0.25
        )
    
    return mod_results

###############################################################################


###############################################################################
# Investment Phase
###############################################################################

# Investment Scenarios
inv_types = [1, 2, 3, 4]
inv_scenarios = list()

for n in range(len(inv_types) + 1):
    inv_scenarios += list(combinations(inv_types, n))

for i in range(len(inv_scenarios)):
    my_scen = inv_scenarios[i]
    inv_scenarios[i] = [
        1 if (plants["inv_int"][i] in my_scen) else 0 for i in range(N)
    ]

inv_scen_out = {str(i) : inv_scenarios[i] for i in range(16)}
inv_scen_out = pd.DataFrame(inv_scen_out)
inv_scen_out.to_csv("Results/simulation-results/inv_scenarios.csv", index=False)

def investment_sim(U, sim_name, file_path):
    
    total_costs = []
    inv_costs = []
    generation_sch = []
    success = []
    
    for k in range(len(inv_scenarios)):
        j = np.array(inv_scenarios[k])
        
        inv_cost = j.dot(np.array(plants["inv_cost"]))/10000
        inv_costs.append(inv_cost)
        
        gen_phase_results = []
        
        for t in range(len(demand.index)):
            D = demand.iloc[t, 1:]
            gen_t = generation_sim(U, D, j)
            gen_phase_results.append(gen_t)
        
        # Total costs under the given investment scenario
        daily_gen_costs = sum([
            gen_phase_results[i]["fun"] for i in range(len(gen_phase_results))
        ])
        gen_phase_costs = sum([
            (dis_fctr**d) * daily_gen_costs for d in range(3*365 + 1)     
        ])
        total_costs.append(inv_cost + gen_phase_costs)
        
        # Make sure that the optimizations terminated successfully for each 
        # investment scenario
        success.append([
            gen_phase_results[i]["success"] for i in range(len(gen_phase_results))
        ])
        
        # Generation matrix over the 24-hour period for the given scenario
        # Size is 720 x 3
        gen_mat = [gen_phase_results[a]["x"].reshape((N,R), order="F") 
                   for a in range(len(gen_phase_results))]
        gen_mat = np.concatenate(gen_mat, axis=0).round(decimals=5)
        generation_sch.append(gen_mat)
        
    total_costs = np.array(total_costs)
    opt_inv_scenario = np.argmin(total_costs)
    
    total_costs = pd.DataFrame(total_costs)
    total_costs["inv_costs"] = inv_costs
    total_costs["sim_name"] = sim_name
    total_costs.to_csv("Results/simulation-results/" + file_path + "/costs.csv")
    
    my_success = pd.DataFrame(success[opt_inv_scenario])
    my_success["sim_name"] = sim_name
    my_success.to_csv("Results/simulation-results/" + file_path + "/success.csv")
    
    my_A = pd.DataFrame(generation_sch[opt_inv_scenario])
    my_A["sim_name"] = sim_name
    my_A.to_csv("Results/simulation-results/" + file_path + "/gen.csv")
    
    return opt_inv_scenario, total_costs, my_success, my_A


def investment_sim_high(U, sim_name, file_path):
    
    total_costs = []
    inv_costs = []
    generation_sch = []
    success = []
    
    for k in range(len(inv_scenarios)):
        j = np.array(inv_scenarios[k])
        
        inv_cost = j.dot(np.array(plants["inv_cost"]))/1000
        inv_costs.append(inv_cost)
        
        gen_phase_results = []
        
        for t in range(len(demand.index)):
            D = demand.iloc[t, 1:]
            gen_t = generation_sim(U, D, j)
            gen_phase_results.append(gen_t)
        
        # Total costs under the given investment scenario
        daily_gen_costs = sum([
            gen_phase_results[i]["fun"] for i in range(len(gen_phase_results))
        ])
        gen_phase_costs = sum([
            (dis_fctr**d) * daily_gen_costs for d in range(3*365 + 1)     
        ])
        total_costs.append(inv_cost + gen_phase_costs)
        
        # Make sure that the optimizations terminated successfully for each 
        # investment scenario
        success.append([
            gen_phase_results[i]["success"] for i in range(len(gen_phase_results))
        ])
        
        # Generation matrix over the 24-hour period for the given scenario
        # Size is 720 x 3
        gen_mat = [gen_phase_results[a]["x"].reshape((N,R), order="F") 
                   for a in range(len(gen_phase_results))]
        gen_mat = np.concatenate(gen_mat, axis=0).round(decimals=5)
        generation_sch.append(gen_mat)
        
    total_costs = np.array(total_costs)
    opt_inv_scenario = np.argmin(total_costs)
    
    total_costs = pd.DataFrame(total_costs)
    total_costs["inv_costs"] = inv_costs
    total_costs["sim_name"] = sim_name
    total_costs.to_csv("Results/simulation-results/" + file_path + "/costs.csv")
    
    my_success = pd.DataFrame(success[opt_inv_scenario])
    my_success["sim_name"] = sim_name
    my_success.to_csv("Results/simulation-results/" + file_path + "/success.csv")
    
    my_A = pd.DataFrame(generation_sch[opt_inv_scenario])
    my_A["sim_name"] = sim_name
    my_A.to_csv("Results/simulation-results/" + file_path + "/gen.csv")
    
    return opt_inv_scenario, total_costs, my_success, my_A

###############################################################################


###############################################################################
# Simulation Scenarios
###############################################################################

def make_U_no_bca(tau):
    
    U = np.zeros(shape=(N,R))
    
    for i in range(N):
        for r in range(R):
            # If the plant is in California --> u + tau*e
            if (plants_loc[i] == 0):
                U[i,r] = plants["fuel_price"][i] + tau*plants["co2e_mmbtu"][i]
            # If the plant is not in California --> u
            elif (plants_loc[i] != 0):
                U[i,r] = plants["fuel_price"][i]
    
    return U


def make_U_bca(tau):
    
    e_not_cal = 284134780/6734463962.555
    # 0.907185? 
    
    U = np.zeros(shape=(N,R))

    for i in range(N):
        for r in range(R):
            # If the plant is in California --> u + tau*e
            if (plants_loc[i] == 0):
                U[i,r] = plants["fuel_price"][i] + tau*plants["co2e_mmbtu"][i]
            # If the plant not in Cal. but sells to Cal. --> u + tau*e_region
            elif (plants_loc[i] != 0 & r == 0):
                U[i,r] = plants["fuel_price"][i] + tau*e_not_cal
            # If the plant not in Cal. and doesn't sell to Cal. --> u
            elif (plants_loc[i] != 0 & r != 0):
                U[i,r] = plants["fuel_price"][i]
                
    return U

###############################################################################
## Scenario a: tau = 0 & No BCA

U_a = make_U_no_bca(0)
        
###############################################################################
## Scenario b: tau = 20 & No BCA

U_b = make_U_no_bca(20)

###############################################################################
## Scenario c: tau = 20 & No BCA

U_c = make_U_no_bca(40)

###############################################################################
## Scenario d: tau = 20 & No BCA

U_d = make_U_no_bca(60)

###############################################################################
## Scenario e: tau = 20 & No BCA

U_e = make_U_no_bca(80)

###############################################################################
## Scenario f: tau = 0 & Uniform BCA

U_f = make_U_bca(0)

###############################################################################
## Scenario g: tau = 20 & Uniform BCA

U_g = make_U_bca(20)

###############################################################################
## Scenario h: tau = 40 & Uniform BCA

U_h = make_U_bca(40)

###############################################################################
## Scenario i: tau = 60 & Uniform BCA

U_i = make_U_bca(60)

###############################################################################
## Scenario j: tau = 80 & Uniform BCA

U_j = make_U_bca(80)

###############################################################################


###############################################################################
# Running the Simulations
###############################################################################

inv_a, cost_a, succ_a, gen_a = investment_sim(U_a, "a", "scenario-a")
inv_b, cost_b, succ_b, gen_b = investment_sim(U_b, "b", "scenario-b")
inv_c, cost_c, succ_c, gen_c = investment_sim(U_c, "c", "scenario-c")
inv_d, cost_d, succ_d, gen_d = investment_sim(U_d, "d", "scenario-d")
inv_e, cost_e, succ_e, gen_e = investment_sim(U_e, "e", "scenario-e")
inv_f, cost_f, succ_f, gen_f = investment_sim(U_f, "f", "scenario-f")
inv_g, cost_g, succ_g, gen_g = investment_sim(U_g, "g", "scenario-g")
inv_h, cost_h, succ_h, gen_h = investment_sim(U_h, "h", "scenario-h")
inv_i, cost_i, succ_i, gen_i = investment_sim(U_i, "i", "scenario-i")
inv_j, cost_j, succ_j, gen_j = investment_sim(U_j, "j", "scenario-j")


# High Cost Investment Scenario
hinv_a, hcost_a, hsucc_a, hgen_a = investment_sim_high(U_a, "a", "high-cost/scenario-a")
hinv_b, hcost_b, hsucc_b, hgen_b = investment_sim_high(U_b, "b", "high-cost/scenario-b")
hinv_c, hcost_c, hsucc_c, hgen_c = investment_sim_high(U_c, "c", "high-cost/scenario-c")
hinv_d, hcost_d, hsucc_d, hgen_d = investment_sim_high(U_d, "d", "high-cost/scenario-d")
hinv_e, hcost_e, hsucc_e, hgen_e = investment_sim_high(U_e, "e", "high-cost/scenario-e")
hinv_f, hcost_f, hsucc_f, hgen_f = investment_sim_high(U_f, "f", "high-cost/scenario-f")
hinv_g, hcost_g, hsucc_g, hgen_g = investment_sim_high(U_g, "g", "high-cost/scenario-g")
hinv_h, hcost_h, hsucc_h, hgen_h = investment_sim_high(U_h, "h", "high-cost/scenario-h")
hinv_i, hcost_i, hsucc_i, hgen_i = investment_sim_high(U_i, "i", "high-cost/scenario-i")
hinv_j, hcost_j, hsucc_j, hgen_j = investment_sim_high(U_j, "j", "high-cost/scenario-j")

 
###############################################################################

