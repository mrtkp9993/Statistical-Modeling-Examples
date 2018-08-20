import pickle

import matplotlib.pyplot as plt
import numpy as np
import pystan

from helper import psis, stan_utility

model_file = "../models/linearRegression.stan"
# Data from http://www.openbugs.net/Examples/Ratsdata.html
data = {'N': 5,
        'x': [8.0, 15.0, 22.0, 29.0, 36.0],
        'y': [160, 207, 248, 288, 324]
        }

sm = pystan.StanModel(file=model_file)
fit = sm.sampling(data=data, control=dict(adapt_delta=0.95))
print(fit)
fit.plot(['alpha', 'beta', 'sigma'])
plt.show()

# model diagnostics
# https://github.com/betanalpha/jupyter_case_studies/blob/master/pystan_workflow/stan_utility.py
stan_utility.check_all_diagnostics(fit)

# visualize model
fit_dict = fit.extract()
m_alpha = np.mean(fit_dict['alpha'])
m_beta = np.mean(fit_dict['beta'])
x = np.linspace(min(data['x']), max(data['x']))
y = m_alpha + m_beta * x
plt.scatter(data['x'], data['y'], c="#1f77b4", label="Observed Data")
plt.plot(x, y, c='#7f7f7f', label="Our Model")
plt.title("Rat weights")
plt.xlabel("Days")
plt.ylabel("Weigths in grams")
plt.legend()
plt.show()

# Log-likelihood
log_lik = fit.extract()['log_lik']
print(psis.psisloo(log_lik)[0])

# Save model for later use
with open('../models/saved/linearRegression.pkl', 'wb') as f:
    pickle.dump(sm, f)
