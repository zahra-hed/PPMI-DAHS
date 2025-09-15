import mdptoolbox, mdptoolbox.example
import pandas as pd
import numpy as np
import random
from math import sqrt

#divide the data for cross-validation
def divide_CV(input, CV): 
    data_list = list(input)
    random.shuffle(data_list)

    subset_size = len(data_list) // CV
    remainder = len(data_list) % CV

    subsets = []
    start_idx = 0
    for i in range(CV):
        end_idx = start_idx + subset_size + (1 if i < remainder else 0)
        subsets.append(data_list[start_idx:end_idx])
        start_idx = end_idx
    return subsets

#define the reward function
def Reward(a, b, mode):
    R = np.zeros(12).reshape(4,3)
    if mode == "reward":
        R[0,:] += 1
    else:
        R[3,:] -= 1
    R[:,1] -= a
    R[:,2] -= b

    return R

def run():
    # divide the data into 1:9
    # calculate P
    # solve the MDP (for 2)
    # 
    CV = 5

    patients = ts_df.iloc[:,0] # Extract patient IDs
    patients = set(patients) # Remove duplicates
    subsets = divide_CV(patients, CV) # Split into 5 CV folds

    tot_sim, tot_const, tot_random, tot_sim_w, tot_real = [[],[]], [[],[]], [[],[]], [[],[]], [[],[]]
    LEDD_total = [0 for _ in range(7)] #sim_r, sim_w, worst_p, worst_p, constant, random, actual 
    LEDD_total = {
        "optimal": [[],[]],  # For optimal policy (reward & penalty models)
        "random": [[],[]],   # For random actions
        "const": [[],[]],   # For constant (no change) treatment
        "worst": [[],[]],    # For worst-case policy
        "real": [[],[]]      # For real-world observed treatment (Actual policy)
    }
    tests = ["optimal", "random", "const", "worst", "real"]

    for i in range(CV+1):
        if i < CV:
            # Use fold `i` for validation, rest for training
            valid_idx = subsets[i]
            train_idx = []
            for j in range(CV):
                if i != j:
                    train_idx += subsets[j]

            valid = ts_df[ts_df['PATNO'].isin(valid_idx)]
            train = ts_df[ts_df['PATNO'].isin(train_idx)]
        else:
            # On final iteration, use full dataset (no CV)
            valid = ts_df

        #create probablity matrix P
        P_t = np.zeros(4*3*4).reshape(3,4,4) # Training transition matrix, calculated just from the train set
        P_v = np.zeros(4*3*4).reshape(3,4,4) # Validation transition matrix, 
        #calculated from the whole dataset to avoid unfairly favoring the trained policy

        for a in range(3):
            for s in range(4):
                for s_ in range(3):
                    #Computes P(s' | s, a) by counting transitions in the data
                    criteria = ((train['cluster'] == s) & (train['action'] == a))
                    criteria_2 = ((train['cluster'] == s) & (train['action'] == a) & (train['cluster_n'] == s_))
                    P_t[a,s,s_] = criteria_2.sum()/criteria.sum() 

                P_t[a,s,3] = 1- sum(P_t[a,s,:]) 

        for a in range(3):
            for s in range(4):
                for s_ in range(3):
                    criteria = ((ts_df['cluster'] == s) & (ts_df['action'] == a))
                    criteria_2 = ((ts_df['cluster'] == s) & (ts_df['action'] == a) & (ts_df['cluster_n'] == s_))
                    P_v[a,s,s_] = criteria_2.sum()/criteria.sum()

                P_v[a,s,3] = 1 - sum(P_v[a,s,:])


        a, b = 0.01, 0.025 # Penalty weights for actions
        R_r = Reward(a, b,"reward") #Reward matrix
        mdp_model_r = mdptoolbox.mdp.PolicyIteration(P_t, R_r, 0.99)
        mdp_model_r.run()
        # if i < CV:
        #     print(f'CV = {i+1} / Reward Model / {mdp_model_r.policy}' , end = "")
        # else:
        #     print(f'Full D / Reward Model / {mdp_model_r.policy}' , end = "")

        R_p = Reward(a, b,"penalty") #Penalty matrix
        mdp_model_p = mdptoolbox.mdp.PolicyIteration(P_t, R_p, 0.99)
        mdp_model_p.run()
        #print(f' / Penalty Model / {mdp_model_p.policy}')

        #Worst-case policies
        R_r_w = - R_r
        mdp_model_r_w = mdptoolbox.mdp.PolicyIteration(P_t, R_r_w, 0.99)
        mdp_model_r_w.run()

        R_p_w = - R_p
        mdp_model_p_w = mdptoolbox.mdp.PolicyIteration(P_t, R_p_w, 0.99)
        mdp_model_p_w.run()

        #policy = {"r":mdp_model_r.policy, "p":mdp_model_p.policy, "r_w" : mdp_model_r_w.policy, "p_w" : mdp_model_r_w.policy}
        policy = {"r":mdp_model_r.policy, "p":mdp_model_p.policy, "r_w" : mdp_model_r_w.policy, "p_w" : mdp_model_r_w.policy}

        sim_reward, random_reward, const_reward, sim_reward_w, real_reward = [0,0], [0,0], [0,0], [0,0], [0,0]

        tot_n = 0
        for pat in valid_idx:
            pat_d = valid[valid['PATNO'] == pat] # Get patient trajectory
            n = len(pat_d) # Number of time steps
            init_LEDD = list(pat_d['LEDD'])[0] # Initial LEDD value

            # Initialize LEDD tracking for each strategy
            LEDD = {"optimal":[init_LEDD,init_LEDD],
                    "random":[init_LEDD,init_LEDD],
                    "const":[init_LEDD,init_LEDD],
                    "worst":[init_LEDD,init_LEDD],
                    "real":[0,0]} #sim_r, sim_p, worst_r, worst_p, constant, random
            
            LEDD_traj = {"optimal":[init_LEDD,init_LEDD],
                    "random":[init_LEDD,init_LEDD],
                    "const":[init_LEDD,init_LEDD],
                    "worst":[init_LEDD,init_LEDD],
                    "real":[0,0]}

            state = pat_d['cluster']  # Initial state
            state = [list(state)[0],list(state)[0]]
            # state = ['state for the reward policy', 'state for the penalty policy']
            
            LEDD_total["real"].append(pat_d["LEDD"].mean())

            # Simulate each strategy
            for t in tests:
                if state[0] == 0:
                    if t == "optimal":
                        sim_reward[0] += 1
                    elif t == "const":
                        const_reward[0] += 1
                    elif t == "worst":
                        sim_reward_w[0] += 1
                    elif t == "real":
                        real_reward[0] += 1
                    else:
                        random_reward[0] += 1
                elif state[1] == 3:
                    if t == "optimal":
                        sim_reward[1] -= 1
                    elif t == "const":
                        const_reward[1] -= 1
                    elif t == "worst":
                        sim_reward_w[1] -= 1
                    elif t == "real":
                        real_reward[1] -= 1
                    else:
                        random_reward[1] -= 1

                for nn in range(n-1): 
                
                    if t == "optimal":
                        action = [policy['r'][state[0]], policy['p'][state[1]]]
                    elif t == "const":
                        action = [0,0]
                    elif t == "worst":
                        action = [policy["r_w"][state[1]], policy["p_w"][state[1]]]
                    elif t == "real":
                        action = [list(pat_d["action"])[nn],list(pat_d["action"])[nn]]
                    else:
                        action = [random.choice(range(3)), random.choice(range(3))]    
                    if action[0] == 1:
                        LEDD[t][0] += 35
                    elif action[0] == 2:
                        LEDD[t][0] += 180
                    else:
                        if t != "const":
                            LEDD[t][0] -= 28

                    if action[1] == 1:
                        LEDD[t][1] += 35
                    elif action[1] == 2:
                        LEDD[t][1] += 180
                    else:
                        if t != "const":
                            LEDD[t][1] -= 28
                    
                    
                    LEDD_traj[t][0] += LEDD[t][0]
                    LEDD_traj[t][1] += LEDD[t][1]

                    state_ = [random.choices(range(4), weights = P_v[action[0],state[0],:])[0], random.choices(range(4), weights = P_v[action[1],state[1],:])[0]]

                    for k in range(2):
                        if action[k] == 1:
                            if t == "optimal":
                                sim_reward[k] -= 0.01
                            elif t == "const":
                                const_reward[k] -= 0.01
                            elif t == "worst":
                                sim_reward_w[k] -= 0.01
                            elif t == "real":
                                real_reward[k] -= 0.025
                            else:
                                random_reward[k] -= 0.01
                        elif action[k] == 2:
                            if t == "optimal":
                                sim_reward[k] -= 0.025
                            elif t == "const":
                                const_reward[k] -= 0.025
                            elif t == "worst":
                                sim_reward_w[k] -= 0.025
                            elif t == "real":
                                real_reward[k] -= 0.025
                            else:
                                random_reward[k] -= 0.025
                    if state[0] == 0:
                        if t == "optimal":
                            sim_reward[0] += 1
                        elif t == "const":
                            const_reward[0] += 1
                        elif t == "worst":
                            sim_reward_w[0] += 1
                        elif t == "real":
                            real_reward[0] += 0.025
                        else:
                            random_reward[0] += 1
                    elif state[1] == 3:
                        if t == "optimal":
                            sim_reward[1] -= 1
                        elif t == "const":
                            const_reward[1] -= 1
                        elif t == "worst":
                            sim_reward_w[1] -= 1
                        elif t == "real":
                            real_reward[1] -= 1
                        else:
                            random_reward[1] -= 1
                    state = state_
                
                LEDD_traj[t][0] /= n
                LEDD_traj[t][1] /= n

                LEDD_total[t][0].append(LEDD_traj[t][0])
                LEDD_total[t][1].append(LEDD_traj[t][1])

            tot_n += n


        sim_reward = [s/tot_n for s in sim_reward]
        sim_reward_w = [s/tot_n for s in sim_reward_w]
        const_reward = [s/tot_n for s in const_reward]
        random_reward = [s/tot_n for s in random_reward]
        real_rewards = [s/tot_n for s in real_reward]

        for kk in range(2):
            tot_sim[kk].append(sim_reward[kk])
            tot_const[kk].append(const_reward[kk])
            tot_random[kk].append(random_reward[kk])
            tot_sim_w[kk].append(sim_reward_w[kk])
            tot_real[kk].append(real_rewards[kk])

    tot1, tot2 = [], []
    #Real total

    #LEDD_total["real"] = np.mean(LEDD_total["real"])

    for t in tests:
        LEDD_total[t][0] = np.mean(LEDD_total[t][0])
        LEDD_total[t][1] = np.mean(LEDD_total[t][1])

    modes = ["reward", "penalty"]
    for aa in range(2):
        print(modes[aa])
        print("opti",  "\t", np.round(np.mean(tot_sim[aa]),4), "\t", np.round(np.std(tot_sim[aa])/sqrt(CV),4))
        print("worst", "\t", np.round(np.mean(tot_sim_w[aa]),4), "\t", np.round(np.std(tot_sim_w[aa])/sqrt(CV),4))
        print("const",  "\t", np.round(np.mean(tot_const[aa]),4),  "\t", np.round(np.std(tot_const[aa])/sqrt(CV),4))
        print("random",  "\t", np.round(np.mean(tot_random[aa]),4), "\t", np.round(np.std(tot_random[aa])/sqrt(CV),4))
        print("real \t", np.round(np.mean(tot_real[aa]),4))

