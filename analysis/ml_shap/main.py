# all these imports mean you can run this data on a linux machine with no monitor and it won't crash
import matplotlib as mpl
mpl.use('Agg')
import matplotlib.pyplot as plt
plt.ioff()

# the rest of the imports
import seaborn as sns
import pandas as pd
import numpy as np
import warnings
import pickle
from os.path import join

import shap
from gbdt_utils import lightgbmExperiment, get_cv

# some settings
sns.set(context='poster')
warnings.filterwarnings("ignore")

# variant determines outcome variable:
# 0: 'brthwt_z', 1: 'gest_weeks', 2: 'deliv_type'
variant = 1
data_dir = join('..', 'data', 'ml_data')

# whether to run the full hyperopt or just to load a model
hyperopt_model = True
compute_shap = True

# lightgbm, hyperopt, shap_params params
hyperopt_evals = 25  # for hyperparam evaluation. each one takes a while
n_splits = 1  # just keep at 1 for the huge dataset
n_estimators = 2500  # number of trees for lightgbm
n_subset_for_shap = 25000  # shap takes a long time to return importances, this is how many samples to do it for

# set up ML problem types with lightgbm terminology
if variant == 0:
    problem = 'regression'
    metric = 'rmse'
    eval_metric = 'rmse'
elif variant == 1:
    problem = 'regression'
    metric = 'rmse'
    eval_metric = 'rmse'
elif variant == 2:
    problem = 'classification'
    metric = 'binary'
    eval_metric = 'auc'

# define various variable types
social_vars = ['m_age_yrs', 'marital_status', 'm_educ', 'n_live_child',
               'n_dead_child', 'race', 'birth_year', 'birth_state_code',
               'm_state_code', 'm_educ_grade', 'm_race', 'n_prev_preg',
               'n_vag_deliv', 'n_ces_deliv', 'f_age_yrs', 'm_educ_2010',
               'm_educ_2010agg', 'hosp_pct_ces', 'm_muni_mean_inc',
               'm_muni_prop_2mw', 'm_muni_pop', 'gest_month_precare',
               'preg_type', 'sex']

birth_vars = ['apgar1', 'apgar5', 'birth_assist', 'birth_qtr', 'ces_pre_labor',
              'cong_anom', 'gest_method', 'hosp_deliv_type', 'labor_induced',
              'presentation', 'n_prenat_visit_cat', 'n_prenat_visit']

outcome_vars = ['brthwt_z', 'gest_weeks', 'deliv_type']

categorical = {'marital_status', 'm_educ', 'preg_type', 'deliv_type', 'n_prenat_visit_cat',
               'sex', 'race', 'cong_anom', 'birth_state_code', 'm_state_code',
               'm_race', 'gest_method', 'presentation', 'labor_induced',
               'ces_pre_labor', 'm_educ_2010', 'birth_assist', 'm_educ_2010agg',
               'hosp_deliv_type', 'birth_qtr'}

if variant == 0:
    Xcols = social_vars + birth_vars + outcome_vars[1:]
    ycol = outcome_vars[:1]
else:
    Xcols = social_vars
    ycol = outcome_vars[variant:variant + 1]

# have to remove conflated delivery types when predicting delivery type
if variant == 2:
    for var in ['hosp_pct_ces', 'n_ces_deliv', 'n_vag_deliv']:
        if var in Xcols:
            Xcols.remove(var)

# import from disk with only variables we need
df = pd.read_csv(join(data_dir, 'snsc_ml.csv'),
                 usecols=Xcols + ycol,
                 dtype={i: 'category' for i in categorical
                        if i in Xcols or i in ycol})

# convert strings to codes
# -1 means missing, as lightgbm expects
categorical_map_col = {}
for i, col in enumerate(categorical):
    if col in df.columns:
        categorical_map_col[col] = dict(enumerate(df[col].cat.categories))
        df[col] = df[col].cat.codes

X = df[Xcols].values
y = df[ycol].values[:, 0]

if problem == 'regression':
    flag = np.isfinite(y)  # no infinities and no nans
if problem == 'classification':
    flag = y > -1  # -1 means nan for categorical variables

# keep only reasonable values
if variant == 0:
    flag = flag & (y >= -6) & (y <= 6)
elif variant == 1:
    flag = flag & (y >= 21)

X = X[flag, :]
y = y[flag]

# split data
splitter = get_cv(problem, X, y, None, n_splits=n_splits, random_state=0)
train_inds, test_inds = next(splitter)
X_train, y_train = X[train_inds], y[train_inds]
X_test, y_test = X[test_inds], y[test_inds]

# get the model either from disk or from hyperparam optimization
model_name = 'lightgbm_' + str(n_estimators) + '_' + ycol[0]
if hyperopt_model:
    cab = lightgbmExperiment(problem,
                             metric,
                             eval_metric=eval_metric,
                             n_estimators=n_estimators,
                             hyperopt_evals=hyperopt_evals,
                             n_splits=n_splits)

    categorical_binary = np.array([i in categorical for i in Xcols])
    cab.run(X_train, y_train, X_test, y_test, None, categorical_binary)

    bst = cab.bst

    with open(join(data_dir, model_name + '.pickle'), 'wb') as f:
        pickle.dump(bst, f)
else:
    # shap on a subset of values
    with open(join(data_dir, model_name + '.pickle'), 'rb') as f:
        bst = pickle.load(f)

# get a subset of the test set and get shap values
# or just load it from the save file
if compute_shap:
    rand_inds = np.random.choice(np.arange(len(X_test)), n_subset_for_shap)
    X_test_sub = X_test[rand_inds, :]
    y_test_sub = y_test[rand_inds]
    shap_values = shap.TreeExplainer(bst).shap_values(X_test_sub)

    with open(join(data_dir, model_name + '_shap.pickle'), 'wb') as f:
        pickle.dump((X_test_sub, y_test_sub, shap_values), f)

else:
    with open(join(data_dir, model_name + '_shap.pickle'), 'rb') as f:
        X_test_sub, y_test_sub, shap_values = pickle.load(f)

plt.figure(0)
shap.summary_plot(shap_values, X_test_sub, feature_names=Xcols, show=False)
plt.tight_layout()
plt.savefig(join(data_dir, model_name + '_shap.png'))