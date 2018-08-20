import pickle

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import pystan
import seaborn as sns

from helper import stan_utility

model_file = "../models/robustRegression.stan"
# http://vincentarelbundock.github.io/Rdatasets/datasets.html
data_file = "../data/aircraft.csv"

data = pd.read_csv(data_file)

data = {'N': 23,
        'X1': data['X1'],
        'X2': data['X2'],
        'X3': (data['X3'] - np.mean(data['X3'])) / np.std(data['X3']),
        'X4': (data['X4'] - np.mean(data['X4'])) / np.std(data['X4']),
        'Y': data['Y'],
        }

sm = pystan.StanModel(file=model_file)
fit = sm.sampling(data=data, control=dict(adapt_delta=0.95))
print(fit)
fit.plot(['b_X1', 'b_X2', 'b_X3', 'b_X4'])
plt.show()

# model diagnostics
# https://github.com/betanalpha/jupyter_case_studies/blob/master/pystan_workflow/stan_utility.py
stan_utility.check_all_diagnostics(fit)

# visualize model
# we'll plot histogram of our errors
Y_pred = fit.extract()['Y_pred'].mean(axis=0)
Y = data['Y'].values
abs_err = np.abs(Y - Y_pred)
sns.distplot(abs_err)
plt.title("Histogram of absolute errors")
plt.xlabel("Errors")
plt.ylabel("Frequency")
plt.show()

# Save model for later use
with open('../models/saved/robustRegression.pkl', 'wb') as f:
    pickle.dump(sm, f)
