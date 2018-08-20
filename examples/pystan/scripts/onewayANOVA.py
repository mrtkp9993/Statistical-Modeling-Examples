import pickle

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import pystan

from helper import stan_utility

model_file = "../models/onewayANOVA.stan"
# http://staff.bath.ac.uk/pssiw/stats2/page16/page16.html
data_file = "../data/iqdata.csv"

data = pd.read_csv(data_file)

data = pd.get_dummies(data, columns=['group'])
del data['group_1']

data = {'N': 43,
        'x1': data['group_2'],
        'x2': data['group_3'],
        'y': data['iq']}

sm = pystan.StanModel(file=model_file)
fit = sm.sampling(data=data, control=dict(adapt_delta=0.95))
print(fit)
fit.plot()
plt.show()

# model diagnostics
# https://github.com/betanalpha/jupyter_case_studies/blob/master/pystan_workflow/stan_utility.py
stan_utility.check_all_diagnostics(fit)

# extract coefficents
fit_dict = fit.extract()
alpha = fit_dict['alpha']
beta_x1 = fit_dict['beta_x1']
beta_x2 = fit_dict['beta_x2']
# calculate group means from coefficents
mean_1 = alpha.mean(axis=0)
mean_2 = alpha.mean(axis=0) + beta_x1.mean(axis=0)
mean_3 = alpha.mean(axis=0) + beta_x2.mean(axis=0)
print(
    f'Mean of group 1: {mean_1},\nMean of group 2: {mean_2},\nMean of group 3: {mean_3}\n')
# calculate the posterior distribution of the difference between the means of group 1 and 3
diffs13 = alpha - (alpha + beta_x2)
# 95% credible intervals
diffs13_ci = np.percentile(diffs13, [2.5, 97.5], axis=0)
print(
    f"Estimated difference between the means of group 1 and 3: {diffs13.mean(axis=0)}\n")
print(f"\t95% credible interval: ({diffs13_ci[0]}, {diffs13_ci[1]})\n")
# How strongly do the data support the hypothesis that the mean of group 3 is larger than the mean of group 1?
print(f"{np.sum(alpha + beta_x2 > alpha) / np.size(alpha)}")
# Because probabilities are never exactly 1, we write >0.999

# Save model for later use
with open('../models/saved/onewayANOVA.pkl', 'wb') as f:
    pickle.dump(sm, f)
