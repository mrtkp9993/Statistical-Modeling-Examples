import pickle

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import pystan

from helper import stan_utility

model_file = "../models/twowayANOVA.stan"
# http://staff.bath.ac.uk/pssiw/stats2/page16/page16.html
data_file = "../data/drugtrial.csv"

data = pd.read_csv(data_file, index_col=0)

data = pd.get_dummies(data, columns=['gender', 'dose'])
data.drop(columns=['gender_1', 'dose_1'], inplace=True)

data = {'N': 48,
        'x1': data['gender_2'],
        'x2': data['dose_2'],
        'y': data['score']}

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
beta_x3 = fit_dict['beta_x3']
# calculate group means from coefficents
mean_11 = alpha.mean(axis=0)
mean_12 = alpha.mean(axis=0) + beta_x1.mean(axis=0)
mean_21 = alpha.mean(axis=0) + beta_x2.mean(axis=0)
mean_22 = alpha.mean(axis=0) + beta_x1.mean(axis=0) + \
    beta_x2.mean(axis=0) + beta_x3.mean(axis=0)
print(
    f'Mean of gender=1, dose=1: {mean_11},\n'
    f'Mean of gender=1, dose=2: {mean_12},\n'
    f'Mean of gender=2, dose=1: {mean_21},\n'
    f'Mean of gender=2, dose=2: {mean_22}\n')

# Save model for later use
with open('../models/saved/twowayANOVA.pkl', 'wb') as f:
    pickle.dump(sm, f)
