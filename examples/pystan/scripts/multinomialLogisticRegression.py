import pickle

import numpy as np
import pandas as pd
import pystan

from helper import stan_utility

model_file = "../models/multinomialLogisticRegression.stan"
# https://stats.idre.ucla.edu/stata/dae/multinomiallogistic-regression/
data_file = "../data/hsbdemo.dta"

data = pd.read_stata(data_file)

data = pd.get_dummies(data=data, columns=['ses', 'schtyp', 'honors'])

map_prog = {'general': 1,
            'academic': 2,
            'vocation': 3}
data['prog'] = data['prog'].map(map_prog)

data['read'] = (data['read'] - np.mean(data['read'])) / np.mean(data['read'])
data['write'] = (data['write'] - np.mean(data['write'])) / \
    np.mean(data['write'])

data = {'N': 200,
        'K': 3,
        'D': 6,
        'x': data[['ses_low', 'ses_middle', 'schtyp_public',
                   'honors_enrolled', 'read', 'write']],
        'y': data['prog']
        }

sm = pystan.StanModel(file=model_file)
fit = sm.sampling(data=data, control=dict(adapt_delta=0.95))
print(fit)

# model diagnostics
# https://github.com/betanalpha/jupyter_case_studies/blob/master/pystan_workflow/stan_utility.py
stan_utility.check_all_diagnostics(fit)

# Save model for later use
with open('../models/saved/multinomialLogisticRegression.pkl', 'wb') as f:
    pickle.dump(sm, f)
