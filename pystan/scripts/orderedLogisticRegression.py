import pickle

import matplotlib.pyplot as plt
import pandas as pd
import pystan

from helper import stan_utility

model_file = "../models/orderedLogisticRegression.stan"
# https://stats.idre.ucla.edu/stata/dae/ordered-logistic-regression/
data_file = "../data/ologit.dta"

data = pd.read_stata(data_file)

x = data[['pared', 'public', 'gpa']]
y = data['apply'].map({'unlikely': 1, 'somewhat likely': 2, 'very likely': 3})

data = {'N': 400,
        'D': 3,
        'K': 3,
        'x': x,
        'y': y}

sm = pystan.StanModel(file=model_file)
fit = sm.sampling(data=data, control=dict(adapt_delta=0.95))
print(fit)

fit.plot()
plt.show()

# model diagnostics
# https://github.com/betanalpha/jupyter_case_studies/blob/master/pystan_workflow/stan_utility.py
stan_utility.check_all_diagnostics(fit)

# Save model for later use
with open('../models/saved/orderedLogisticRegression.pkl', 'wb') as f:
    pickle.dump(sm, f)
