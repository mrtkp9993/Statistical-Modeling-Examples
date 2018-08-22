import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import seaborn as sns
import statsmodels.api as sm

# http://staff.bath.ac.uk/pssiw/stats2/page16/page16.html
df = pd.read_csv("../data/child_data.csv")
print(df.head())

sns.set(style="white", palette="muted", color_codes=True)

f, axes = plt.subplots(2, 2, figsize=(7, 7))

sns.distplot(df.age, ax=axes[0, 0])
sns.distplot(df.mem_span, ax=axes[0, 1])
sns.distplot(df.iq, ax=axes[1, 0])
sns.distplot(df.read_ab, ax=axes[1, 1])
plt.show()

sns.pairplot(df, vars=['age', 'mem_span', 'iq'])
plt.show()

# Rescale all variables
for col in df.columns.values:
    df[col] = (df[col] - np.mean(df[col]))/(2 * np.std(df[col]))

print(df.head())

# Ordinary multiple linear regression
# Mem_span and age seems correlated, so I'll use one of them
mod1 = sm.formula.ols('read_ab ~ age + iq', data=df).fit()
print(mod1.summary())

mod2 = sm.formula.ols('read_ab ~ age + mem_span', data=df).fit()
print(mod2.summary())

# Now, add interaction term
mod1 = sm.formula.ols('read_ab ~ age + iq + age:iq', data=df).fit()
print(mod1.summary())
