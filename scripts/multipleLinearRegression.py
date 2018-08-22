import pickle

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import pystan
import seaborn as sns

from helper import psis, stan_utility

model_file = "../models/multipleLinearRegression.stan"
# http://lib.stat.cmu.edu/DASL/Datafiles/Cereals.html
data_file = "../data/cereals.txt"
data = pd.read_table(data_file)

data = data[['fat', 'weight', 'cups', 'rating']]
data = {'N': 77,
        'fat': data['fat'],
        'weight': data['weight'],
        'cups': data['cups'],
        'rating': data['rating']}

sm = pystan.StanModel(file=model_file)
fit = sm.sampling(data=data, control=dict(adapt_delta=0.95))
print(fit)
fit.plot(['b_fat', 'b_weight', 'b_cups'])
plt.show()

# model diagnostics
# https://github.com/betanalpha/jupyter_case_studies/blob/master/pystan_workflow/stan_utility.py
stan_utility.check_all_diagnostics(fit)

# visualize model
# we'll plot histogram of our errors
rating_pred = fit.extract()['rating_pred'].mean(axis=0)
rating = data['rating'].values
abs_err = np.abs(rating - rating_pred)
sns.distplot(abs_err)
plt.title("Histogram of absolute errors")
plt.xlabel("Errors")
plt.ylabel("Frequency")
plt.show()

# Log-likelihood
log_lik = fit.extract()['log_lik']
print(psis.psisloo(log_lik)[0])

# Save model for later use
with open('../models/saved/multipleLinearRegression.pkl', 'wb') as f:
    pickle.dump(sm, f)
