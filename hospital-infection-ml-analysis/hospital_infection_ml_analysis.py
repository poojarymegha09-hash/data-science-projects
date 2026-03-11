# %%
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns

# Load the dataset
df = pd.read_csv(
    r"C:\Users\Megha Poojary\OneDrive - Université Libre de Bruxelles\Bureau\hospital_infection_project\hospital_infections.csv"
)
print("Shape:", df.shape)
print(df.head())
print(df.info())


# %%
import sys
print(sys.executable)


# %%
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns

df = pd.read_csv(
    r"C:\Users\Megha Poojary\OneDrive - Université Libre de Bruxelles\Bureau\hospital_infection_project\hospital_infections.csv"
)

print("Shape:", df.shape)
df.head()


# %% [markdown]
# # Projet : Infections nosocomiales
# 
# ## Phase 1 – Exploration et préparation des données
# 

# %%
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns

# Make plots appear inside the notebook
%matplotlib inline

# Better display for pandas tables
pd.set_option("display.max_columns", None)

# Load the dataset
df = pd.read_csv(
    r"C:\Users\Megha Poojary\OneDrive - Université Libre de Bruxelles\Bureau\hospital_infection_project\hospital_infections.csv"
)

print("Shape:", df.shape)
df.head()


# %%
df.info()
df.dtypes


# %%
df.isna().sum()


# %%
df.describe()


# %%
df.describe(include="object")


# %%
for col in ["sexe", "service", "antibiotherapie"]:
    print(col)
    print(df[col].value_counts())
    print("\n")


# %%
import matplotlib.pyplot as plt
import seaborn as sns

categorical_cols = ["sexe", "service", "antibiotherapie"]

for col in categorical_cols:
    plt.figure(figsize=(7,4))
    sns.countplot(data=df, x=col, palette="viridis")
    plt.title(f"Répartition de {col}")
    plt.xlabel(col)
    plt.ylabel("Nombre d'observations")
    plt.xticks(rotation=45)
    plt.show()


# %%
import matplotlib.pyplot as plt
import seaborn as sns

numeric_cols = ["age", "duree_sejour", "jours_catheter", "temperature", "crp", "leucocytes"]

for col in numeric_cols:
    plt.figure(figsize=(7,4))
    sns.histplot(df[col], kde=True, color="skyblue")
    plt.title(f"Distribution de {col}")
    plt.xlabel(col)
    plt.ylabel("Fréquence")
    plt.show()


# %%
for col in numeric_cols:
    plt.figure(figsize=(6,2))
    sns.boxplot(x=df[col], color="lightgreen")
    plt.title(f"Boîte à moustaches de {col}")
    plt.show()


# %%
for col in numeric_cols:
    plt.figure(figsize=(6,3))
    sns.violinplot(x=df[col], color="lightcoral")
    plt.title(f"Violin plot de {col}")
    plt.show()


# %%
# Imputation of missing values by median
df['temperature'] = df['temperature'].fillna(df['temperature'].median())
df['crp'] = df['crp'].fillna(df['crp'].median())
df['leucocytes'] = df['leucocytes'].fillna(df['leucocytes'].median())

df.isna().sum()


# %% [markdown]
# # Correlation heatmaps

# %%
import matplotlib.pyplot as plt
import seaborn as sns

plt.figure(figsize=(10,6))
sns.heatmap(df.corr(numeric_only=True), annot=True, cmap="coolwarm")
plt.title("Correlation of matrices")
plt.show()


# %% [markdown]
# # Countplot

# %%
sns.countplot(data=df, x="infection_nosocomiale", palette="viridis")
plt.title("Répartition de l'infection nosocomiale")
plt.show()
df["infection_nosocomiale"].value_counts(normalize=True) * 100

# %% [markdown]
# # Positive rate

# %%
df['infection_nosocomiale'].mean()


# %% [markdown]
# # Numeric vs Target

# %%
# Age
sns.boxplot(data=df, x="infection_nosocomiale", y="age")


# %%
# Duree_sejour
sns.boxplot(data=df, x="infection_nosocomiale", y="duree_sejour")

# %%
# JOURS_CATHETER
sns.boxplot(data=df, x="infection_nosocomiale", y="jours_catheter")

# %%
# temerature
sns.boxplot(data=df, x="infection_nosocomiale", y="temperature")

# %%
# CRP
sns.boxplot(data=df, x="infection_nosocomiale", y="crp")

# %%
# Leucocytes
sns.boxplot(data=df, x="infection_nosocomiale", y="leucocytes")

# %% [markdown]
# # Categorical Vs Target

# %%
# sexe
sns.barplot(data=df, x="sexe", y="infection_nosocomiale", palette="viridis")


# %%
# service
sns.barplot(data=df, x="service", y="infection_nosocomiale", palette="viridis")

# %%
# Antibiotherpie
sns.barplot(data=df, x="antibiotherapie", y="infection_nosocomiale", palette="viridis")

# %% [markdown]
# # Encoding of categorical variables

# %%
df_encoded = pd.get_dummies(
    df,
    columns=["sexe", "service", "antibiotherapie"],
    drop_first=True
)

df_encoded.head()


# %%
df_encoded.info()


# %% [markdown]
# # Train/Test split

# %%
from sklearn.model_selection import train_test_split

# Features (X) = all columns except the target
X = df_encoded.drop("infection_nosocomiale", axis=1)

# Target (y)
y = df_encoded["infection_nosocomiale"]

# Train/Test Split
X_train, X_test, y_train, y_test = train_test_split(
    X, y, test_size=0.2, random_state=42, stratify=y
)

X_train.shape, X_test.shape


# %% [markdown]
# # Standardise numeric features

# %%
from sklearn.preprocessing import StandardScaler

# Copy X_train and X_test to avoid modifying them by mistake
X_train_scaled = X_train.copy()
X_test_scaled = X_test.copy()

numeric_cols = ['age', 'duree_sejour', 'jours_catheter', 
                'temperature', 'crp', 'leucocytes']

scaler = StandardScaler()
X_train_scaled[numeric_cols] = scaler.fit_transform(X_train[numeric_cols])
X_test_scaled[numeric_cols] = scaler.transform(X_test[numeric_cols])

X_train_scaled.head()


# %% [markdown]
# # Logistic regression

# %%
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import classification_report, confusion_matrix, roc_auc_score, roc_curve
import matplotlib.pyplot as plt
import seaborn as sns

# Train model
log_reg = LogisticRegression(max_iter=1000)
log_reg.fit(X_train_scaled, y_train)

# Predictions
y_pred_log = log_reg.predict(X_test_scaled)
y_prob_log = log_reg.predict_proba(X_test_scaled)[:, 1]

# Evaluation
print("Classification Report (Logistic Regression):")
print(classification_report(y_test, y_pred_log))

print("Confusion Matrix:")
sns.heatmap(confusion_matrix(y_test, y_pred_log), annot=True, fmt="d", cmap="Blues")
plt.show()

# AUC
auc_log = roc_auc_score(y_test, y_prob_log)
print("AUC:", auc_log)

# ROC Curve
fpr, tpr, _ = roc_curve(y_test, y_prob_log)
plt.plot(fpr, tpr, label=f"AUC = {auc_log:.3f}")
plt.xlabel("False Positive Rate")
plt.ylabel("True Positive Rate")
plt.title("ROC Curve — Logistic Regression")
plt.legend()
plt.show()


# %%
# Decision tree
from sklearn.tree import DecisionTreeClassifier

tree = DecisionTreeClassifier(max_depth=5, random_state=42)
tree.fit(X_train_scaled, y_train)

y_pred_tree = tree.predict(X_test_scaled)

print("Decision Tree:")
print(classification_report(y_test, y_pred_tree))

sns.heatmap(confusion_matrix(y_test, y_pred_tree), annot=True, fmt="d", cmap="Purples")
plt.show()


# %%
# Random Forest
from sklearn.ensemble import RandomForestClassifier

rf = RandomForestClassifier(n_estimators=200, random_state=42)
rf.fit(X_train_scaled, y_train)

y_pred_rf = rf.predict(X_test_scaled)

print("Random Forest:")
print(classification_report(y_test, y_pred_rf))

sns.heatmap(confusion_matrix(y_test, y_pred_rf), annot=True, fmt="d", cmap="Greens")
plt.show()


# %% [markdown]
# # Statistical GLM

# %%
import statsmodels.api as sm

# %%
X_train_glm = sm.add_constant(X_train_scaled)
X_test_glm = sm.add_constant(X_test_scaled)

# %%
X_train_glm = X_train_glm.astype(float) #converting to float
y_train_glm = y_train.astype(float)

X_test_glm = X_test_glm.astype(float)


# %%
glm_model = sm.Logit(y_train, X_train_glm) #GLM
glm_result = glm_model.fit()

print(glm_result.summary())

# %%
# Probabilities
y_proba_glm = glm_result.predict(X_test_glm) #Predictions

# Class prediction
y_pred_glm = (y_proba_glm >= 0.5).astype(int)

# classification metrics
print("Classification Report – GLM")
print(classification_report(y_test, y_pred_glm))

# GLM Accuracy
from sklearn.metrics import accuracy_score
glm_accuracy = accuracy_score(y_test, y_pred_glm)
print("GLM Accuracy:", glm_accuracy)

# GLM AUC
from sklearn.metrics import roc_auc_score
auc_glm = roc_auc_score(y_test, y_proba_glm)
print("GLM AUC:", auc_glm)


# %%
#AUC
auc_glm = roc_auc_score(y_test, y_proba_glm)
print("AUC (GLM):", auc_glm)

# %%
#Confusion matrix
cm_glm = confusion_matrix(y_test, y_pred_glm)
sns.heatmap(cm_glm, annot=True, fmt="d", cmap="Blues")
plt.title("Confusion Matrix – GLM")
plt.show()

# %%
#Brier Score
from sklearn.metrics import brier_score_loss
brier_glm = brier_score_loss(y_test, y_proba_glm)
print("Brier Score (GLM):", brier_glm)

# %%
# Calibration curve
from sklearn.calibration import calibration_curve

prob_true, prob_pred = calibration_curve(y_test, y_proba_glm, n_bins=10)

plt.plot(prob_pred, prob_true, marker='o')
plt.plot([0,1], [0,1], 'k--')
plt.xlabel("Predicted probability")
plt.ylabel("Observed frequency")
plt.title("Calibration Curve - GLM")
plt.show()


# %%
#GAM model
from pygam import LogisticGAM, s
from sklearn.metrics import classification_report, roc_auc_score

# Select only the GAM smooth variables
gam_features = ['age', 'crp', 'temperature', 'jours_catheter', 'duree_sejour']

X_train_gam = X_train_scaled[gam_features]
X_test_gam = X_test_scaled[gam_features]

# Build GAM with smooth terms
gam = LogisticGAM(
    s(0) + s(1) + s(2) + s(3) + s(4)
).fit(X_train_gam, y_train)

# Predictions
y_pred_gam = gam.predict(X_test_gam)
y_proba_gam = gam.predict_proba(X_test_gam)

print("Classification Report (GAM):")
print(classification_report(y_test, y_pred_gam))

# AUC
auc_gam = roc_auc_score(y_test, y_proba_gam)
print("GAM AUC:", auc_gam)


# %%
import matplotlib.pyplot as plt

plt.figure(figsize=(12, 8))
for i, term in enumerate(gam_features):
    plt.subplot(2, 3, i+1)
    XX = gam.generate_X_grid(term=i)
    plt.plot(XX[:, i], gam.partial_dependence(term=i, X=XX))
    plt.title(f"Smoothing term: {term}")
plt.tight_layout()
plt.show()


# %%
#GAM vs GLM (bias-variance)
print("<<<<<<<<<<GLM vs GAM Comparison>>>>>>>>>")
print("GLM AUC:", auc_glm)
print("GAM AUC:", auc_gam)

print("GLM Accuracy:", glm_accuracy)
print("GAM Accuracy:", gam.score(X_test_gam, y_test))

# %%
# Analysis of error

from sklearn.metrics import roc_curve, roc_auc_score

## ROC for GLM
fpr_glm, tpr_glm, _ = roc_curve(y_test, y_proba_glm)
auc_glm = roc_auc_score(y_test, y_proba_glm)

plt.plot(fpr_glm, tpr_glm, label=f"AUC GLM = {auc_glm:.3f}")
plt.plot([0,1], [0,1], 'k--')  # baseline
plt.xlabel("False Positive Rate")
plt.ylabel("True Positive Rate")
plt.title("ROC Curve — GLM")
plt.legend()
plt.show()


##ROC for GAM
fpr_gam, tpr_gam, _ = roc_curve(y_test, y_proba_gam)
auc_gam = roc_auc_score(y_test, y_proba_gam)

plt.plot(fpr_gam, tpr_gam, label=f"AUC GAM = {auc_gam:.3f}", color="green")
plt.plot([0,1], [0,1], 'k--')
plt.xlabel("False Positive Rate")
plt.ylabel("True Positive Rate")
plt.title("ROC Curve — GAM")
plt.legend()
plt.show()


# %%
#Precision Recall

from sklearn.metrics import precision_recall_curve, average_precision_score

## Precision-Recall for GLM
prec_glm, rec_glm, _ = precision_recall_curve(y_test, y_proba_glm)
ap_glm = average_precision_score(y_test, y_proba_glm)

plt.plot(rec_glm, prec_glm, label=f"AP = {ap_glm:.3f}")
plt.xlabel("Recall")
plt.ylabel("Precision")
plt.title("Precision-Recall Curve — GLM")
plt.legend()
plt.show()

print("Average Precision (GLM):", ap_glm)

## Precision-Recall for GAM
prec_gam, rec_gam, _ = precision_recall_curve(y_test, y_proba_gam)
ap_gam = average_precision_score(y_test, y_proba_gam)

plt.plot(rec_gam, prec_gam, label=f"AP = {ap_gam:.3f}", color="green")
plt.xlabel("Recall")
plt.ylabel("Precision")
plt.title("Precision-Recall Curve — GAM")
plt.legend()
plt.show()

print("Average Precision (GAM):", ap_gam)



# %%
# False Negative Profile

# Identify false negatives for GLM
false_neg_glm_mask = (y_test == 1) & (y_pred_glm == 0)
false_neg_glm = X_test[false_neg_glm_mask]

print("Number of false negatives (GLM):", false_neg_glm.shape[0])
false_neg_glm.head()

# Identify false negatives for GAM
false_neg_gam_mask = (y_test == 1) & (y_pred_gam == 0)
false_neg_gam = X_test[false_neg_gam_mask]

print("Number of false negatives (GAM):", false_neg_gam.shape[0])
false_neg_gam.head()


# %% [markdown]
# Phase 3- Optimisation

# %%
# Validation croisée (K fold)

from sklearn.model_selection import cross_val_score
from sklearn.ensemble import RandomForestClassifier
from xgboost import XGBClassifier

# Random Forest
rf = RandomForestClassifier(n_estimators=200, random_state=42)
scores_rf = cross_val_score(rf, X_train_scaled, y_train, cv=5, scoring='roc_auc')
print("Random Forest CV AUC:", scores_rf.mean())

# XGBoost
xgb = XGBClassifier(
    n_estimators=200,
    learning_rate=0.1,
    max_depth=4,
    subsample=0.8,
    eval_metric='logloss'
)
scores_xgb = cross_val_score(xgb, X_train_scaled, y_train, cv=5, scoring='roc_auc')
print("XGBoost CV AUC:", scores_xgb.mean())


# %%
#tuning

##Tuning random forest
from sklearn.model_selection import RandomizedSearchCV

param_rf = {
    "n_estimators": [100, 200, 300],
    "max_depth": [3, 5, 10, None],
    "max_features": ["sqrt", "log2"]
}

rf = RandomForestClassifier(random_state=42)
rf_search = RandomizedSearchCV(rf, param_rf, cv=5, scoring="roc_auc")
rf_search.fit(X_train_scaled, y_train)

print("Best RF params:", rf_search.best_params_)
print("Best RF AUC:", rf_search.best_score_)

##Tuning XGboost
param_xgb = {
    "n_estimators": [100, 200, 300],
    "learning_rate": [0.01, 0.05, 0.1],
    "max_depth": [3, 4, 5],
    "subsample": [0.7, 0.8, 1.0]
}

xgb = XGBClassifier(eval_metric="logloss")

xgb_search = RandomizedSearchCV(xgb, param_xgb, cv=5, scoring="roc_auc")
xgb_search.fit(X_train_scaled, y_train)

print("Best XGB params:", xgb_search.best_params_)
print("Best XGB AUC:", xgb_search.best_score_)



# %%
from xgboost import XGBClassifier

xgb = XGBClassifier(
    objective='binary:logistic',
    eval_metric='logloss',
    use_label_encoder=False,
    random_state=42
)

xgb_param_grid = {
    "n_estimators": [100, 200, 300],
    "learning_rate": [0.01, 0.05, 0.1],
    "max_depth": [3, 4, 5],
    "subsample": [0.6, 0.8, 1.0],
    "colsample_bytree": [0.6, 0.8, 1.0]
}

# %%
##Optimisation with RandomizedSearchCV
from sklearn.model_selection import RandomizedSearchCV

# Hyperparameter grid for Random Forest
rf_param_grid = {
    'n_estimators': [100, 200, 300],
    'max_depth': [3, 5, 7, None],
    'max_features': ['sqrt', 'log2'],
    'min_samples_split': [2, 5, 10],
    'min_samples_leaf': [1, 2, 4]
}

rf_random = RandomizedSearchCV(
    estimator=rf,
    param_distributions=rf_param_grid,
    n_iter=20,            # number of random combinations
    scoring='roc_auc',
    cv=5,
    random_state=42,
    n_jobs=-1
)

rf_random.fit(X_train_scaled, y_train)
print("Best RF params:", rf_random.best_params_)
print("Best RF CV AUC:", rf_random.best_score_)

xgb_random = RandomizedSearchCV(
    estimator=xgb,
    param_distributions=xgb_param_grid,
    n_iter=20,
    scoring='roc_auc',
    cv=5,
    random_state=42,
    n_jobs=-1
)

xgb_random.fit(X_train_scaled, y_train)
print("Best XGB params:", xgb_random.best_params_)
print("Best XGB CV AUC:", xgb_random.best_score_)


# %%
#Benchmark final
from sklearn.metrics import classification_report, roc_auc_score

# --- Random Forest ---
rf_best = rf_search.best_estimator_
rf_pred = rf_best.predict(X_test_scaled)
rf_proba = rf_best.predict_proba(X_test_scaled)[:,1]
print("RF AUC:", roc_auc_score(y_test, rf_proba))
print(classification_report(y_test, rf_pred))

# --- XGBoost ---
xgb_best = xgb_search.best_estimator_
xgb_pred = xgb_best.predict(X_test_scaled)
xgb_proba = xgb_best.predict_proba(X_test_scaled)[:,1]
print("XGB AUC:", roc_auc_score(y_test, xgb_proba))
print(classification_report(y_test, xgb_pred))


# %%
#Calibration of model
from sklearn.metrics import brier_score_loss

print("Brier GLM:", brier_score_loss(y_test, y_proba_glm))
print("Brier GAM:", brier_score_loss(y_test, y_proba_gam))
print("Brier RF:", brier_score_loss(y_test, rf_proba))
print("Brier XGB:", brier_score_loss(y_test, xgb_proba))


# %% [markdown]
# # SHAP

# %%
from sklearn.ensemble import RandomForestClassifier

rf = RandomForestClassifier(
    n_estimators=200,
    max_depth=5,
    max_features='sqrt',
    random_state=42
)

rf.fit(X_train_scaled, y_train)

# %%
import shap
shap.initjs()


# %%
#FOr random forest
explainer_rf = shap.TreeExplainer(rf)
shap_values_rf = explainer_rf.shap_values(X_test_scaled)


# %%
#For xgboost
explainer_xgb = shap.TreeExplainer(xgb_random.best_estimator_)
shap_values_xgb = explainer_xgb.shap_values(X_test_scaled)


# %%
#SHAP Global imporatnace
shap.summary_plot(shap_values_rf, X_test_scaled, feature_names=X_test.columns)  #random forest
shap.summary_plot(shap_values_xgb, X_test_scaled, feature_names=X_test.columns) #xgboost

# %%
#SHAP dependance Plots
shap.dependence_plot("temperature", shap_values_xgb, X_test_scaled, feature_names=X_test.columns)
shap.dependence_plot("crp", shap_values_xgb, X_test_scaled, feature_names=X_test.columns)
shap.dependence_plot("duree_sejour", shap_values_xgb, X_test_scaled, feature_names=X_test.columns)


# %%
#Interactions
shap.dependence_plot(
    "temperature", shap_values_xgb, X_test_scaled, feature_names=X_test.columns, interaction_index="auto"
)


# %%
#Local Explanations
patients =[37,99,1068]

i = 37
shap.force_plot(
    explainer_xgb.expected_value,
    shap_values_xgb[i],
    X_test_scaled.iloc[i],
    feature_names=X_test.columns
)

i = 99
shap.force_plot(
    explainer_xgb.expected_value,
    shap_values_xgb[i],
    X_test_scaled.iloc[i],
    feature_names=X_test.columns
)

i = 599
shap.force_plot(
    explainer_xgb.expected_value,
    shap_values_xgb[i],
    X_test_scaled.iloc[i],
    feature_names=X_test.columns
)



