🚗 Car Price Predictive Modeling
A statistical modeling project focused on identifying how vehicle dimensions such as length, width, height, and wheelbase affect the price of a car. This analysis uses multiple regression techniques, variable transformations, and robust estimation methods to improve model accuracy and reliability.

👩‍💻 Author
Ajuna P John
Assignment Type: Statistical Modeling Project

🎯 Objective
To determine the effect of car size (length, width, height, wheelbase) on vehicle price using exploratory data analysis and a range of regression modeling techniques. The project addresses normality violations, outliers, and multicollinearity using both traditional and modern statistical tools.

📂 Dataset Overview
File Name: CarPrice.csv

Observations: 143

Variables:

Price – Car price (dependent variable)

Length, Width, Height, Wheelbase – Predictor variables (in inches/mm)

🧪 Methodology
🧹 Data Preprocessing
Checked for missing values (none found)

Plotted histograms and QQ-plots to assess normality

Detected and addressed outliers using Cook’s Distance and DFFITS

📊 Exploratory Data Analysis
Strong correlation found between price and car dimensions

High multicollinearity observed among predictors (r > 0.85)

📈 Modeling Techniques
Technique	Description
Multiple Linear Regression	Base model to understand linear relationship
Box-Cox Transformation	Applied to fix non-normal distribution of residuals
Robust Regression (MM)	Addressed high influence of outliers
Bootstrapping	Resampled model parameters to assess stability and confidence intervals
Model Selection	Best subset, stepwise (AIC), and k-fold cross-validation for feature choice

🔍 Key Findings
All four size variables are significant contributors to price.

Box-Cox transformation (λ = 0.2) improved residual normality and model fit.

Robust regression reduced the influence of high-leverage outliers.

Bootstrapped confidence intervals confirmed the consistency of coefficient estimates.

✅ Final Model
Best model based on stepwise selection (AIC) and 10-fold cross-validation

Final predictors: All size variables retained

Residual diagnostics passed all major tests for linearity, homoscedasticity, and normality

📉 Evaluation Metrics
R² (Multiple Regression): 0.894

Adjusted R²: 0.891

RMSE (Test): Approximately 4500 USD

MAPE: Under 10%

🧰 Tech Stack
Language: R

Libraries: car, MASS, boot, olsrr, robustbase, ggplot2, leaps
