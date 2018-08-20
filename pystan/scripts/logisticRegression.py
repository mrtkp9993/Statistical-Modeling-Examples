import pickle

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import pystan
import seaborn as sns
from sklearn.metrics import confusion_matrix
from sklearn.model_selection import train_test_split

from helper import stan_utility

model_file = "../models/logisticRegression.stan"
# https://stats.idre.ucla.edu/stata/dae/logistic-regression/
data_file = "../data/binary.dta"

data = pd.read_stata(data_file)

# Data preprocessing
# Convert rank categorial variable to dummy
data = pd.get_dummies(data=data, columns=['rank'])
del data['rank_1.0']  # avoid dummy variable trap

# Rescale gpa and gre variables
data['gre'] = (data['gre'] - np.mean(data['gre'])) / np.std(data['gre'])
data['gpa'] = (data['gpa'] - np.mean(data['gpa'])) / np.std(data['gpa'])

# Split data as train/test
data_train, data_test = train_test_split(data, test_size=0.2)

model_data = {'N_train': 320,
              'N_test': 80,
              'D': 5,
              'x_train': data_train[['gre', 'gpa', 'rank_2.0',
                                     'rank_3.0', 'rank_4.0']].astype(np.int32),
              'x_test': data_test[['gre', 'gpa', 'rank_2.0',
                                   'rank_3.0', 'rank_4.0']].astype(np.int32),
              'y_train': data_train['admit'].astype(np.int32)}

sm = pystan.StanModel(file=model_file)
fit = sm.sampling(data=model_data, control=dict(adapt_delta=0.95))
print(fit)
fit.plot(['alpha', 'beta'])
plt.show()

sns.pairplot(pd.DataFrame(fit.extract()['beta']))
plt.show()

# model diagnostics
# https://github.com/betanalpha/jupyter_case_studies/blob/master/pystan_workflow/stan_utility.py
stan_utility.check_all_diagnostics(fit)

# Confusion matrix
y_pred = fit.extract()['y_pred']
y_pred = np.median(y_pred, axis=0)
print(confusion_matrix(data_test['admit'], y_pred))

# Save model for later use
with open('../models/saved/logisticRegression.pkl', 'wb') as f:
    pickle.dump(sm, f)
