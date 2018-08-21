import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import seaborn as sns
import statsmodels.api as sm

# http://staff.bath.ac.uk/pssiw/stats2/page16/page16.html
df = pd.read_csv("data/awards.csv", index_col=0)
df.head()

df.describe()

df = pd.get_dummies(df, columns=["prog"])
del df['prog_1']
df.head()

df['math'] = (df['math'] - np.mean(df['math']))/(2 * np.std(df['math']))
df.head()

X = np.column_stack(
    (np.ones((df.shape[0], 1)), df[['math', 'prog_2', 'prog_3']]))
y = df['num_awards']

mod = sm.formula.GLM(y, X, family=sm.families.Poisson()).fit()
mod.summary()

model_fitted_y = mod.fittedvalues
model_residuals = mod.df_resid
model_abs_resid = np.abs(model_residuals)

# https://medium.com/@emredjan/emulating-r-regression-plots-in-python-43741952c034
plot_lm_1 = plt.figure(1)
plot_lm_1.set_figheight(8)
plot_lm_1.set_figwidth(12)

plot_lm_1.axes[0] = sns.residplot(model_fitted_y, 'num_awards', data=df,
                                  lowess=True,
                                  scatter_kws={'alpha': 0.5},
                                  line_kws={'color': 'red', 'lw': 2, 'alpha': 0.8})

plot_lm_1.axes[0].set_title('Residuals vs Fitted')
plot_lm_1.axes[0].set_xlabel('Fitted values')
plot_lm_1.axes[0].set_ylabel('Residuals')
