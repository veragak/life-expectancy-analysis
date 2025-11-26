# Life Expectancy Analysis Using PCA and Elastic Net

This project investigates international differences in life expectancy using 
global health, nutrition, demographic, and socioeconomic indicators.  
The analysis applies **Principal Component Analysis (PCA)** for dimensionality 
reduction and **Elastic Net regression** for predictive modeling and variable 
selection.

The aim is to both **understand the main drivers of life expectancy** and 
**predict life expectancy for new countries** using optimized components.

This project was originally developed for the course  
**FEM11149 – Introduction to Data Science (Erasmus School of Economics)**, 
and has been reorganized into a clean, reproducible data-science workflow.

---
## Repository Structure
- **`R-Code/`**
  - `01_prepare_data.R` — clean + merge datasets  
  - `02_fit_models.R` — PCA + Elastic Net  
  - `03_make_predictions.R` — predictions for new countries  
  - `04_complete_code.Rmd` — optional combined script  

- **`data/`**
  - `world_health_indicators_data.csv` — predictor variables  
  - `life_expectancy_data.csv` — target variable  
  - `predictions_data.csv` — new data for predictions  

- **`figures/`**
  - `01_PCA_scree_plot.png`  
  - `02_permutation_test.png`  
  - `03_bootstrap.png`  
  - `04_boostrap_variance.png`  
  - `05_elastic_net.png`  
  - `06_elastic_net_coefficient.png`  

- **`report/`**
  - `life_expectancy_analysis.Rmd` — full reproducible analysis  
  - `life_expectancy_analysis.pdf` — final written report  

- **`README.md`**
---

##Project Overview

The goal of this project is to understand which factors most strongly predict
life expectancy worldwide and to build a robust predictive model.

The analysis includes:

### **1. Data Preparation**
- Cleaning and merging datasets from public global health sources  
- Handling missing values  
- Scaling and standardizing variables

### **2. Principal Component Analysis (PCA)**
- Reducing multicollinearity  
- Identifying interpretable components (e.g., health system, environment)  
- Visualizing:
  - Scree plot  
  - Loading structures  
  - Bootstrap stability  
  - Permutation significance tests  

### **3. Elastic Net Regression**
- Combines L1 (Lasso) and L2 (Ridge) penalties  
- Reduces overfitting  
- Handles correlated predictors  
- Cross-validation used to select optimal λ  
- Visualizations included:
  - CV error curve  
  - Coefficient paths  

### **4. Prediction**
A trained model predicts life expectancy for new countries based on indicators
in `predictions_data.csv`.

---




