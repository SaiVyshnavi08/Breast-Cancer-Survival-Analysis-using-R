# Breast Cancer Survival Analysis using R
This project analyzes breast cancer patient data to predict whether a patient is likely to survive for 5 years or more after diagnosis. It uses machine learning models including Logistic Regression, Decision Tree Classifier, and a Neural Network. The dataset comes from the SEER (Surveillance, Epidemiology, and End Results) database.

## 📌 Project Goals
Learn how to apply machine learning for binary classification
Explore real medical data
Understand model evaluation metrics like Precision, Recall, F1 Score, and AUC
Practice visualizing data for insights

## 📊 Visualizations
Age Distribution Histogram
5-Year Survival Pie Chart
Year-wise Death Bar Chart
Gender Distribution Bar Plot
Correlation Heatmap
ROC & AUC Curves for all models

## 🔍 Key Insights
Most patients were aged between 40 and 65.
Around 64.6% survived 5 years or more.
Majority were female and married at the time of diagnosis.
Neural networks outperformed simpler models like logistic regression and decision trees in survival prediction.

## Conclusion
The Logistic Regression model offered strong interpretability and a solid performance across metrics. It achieved an F1 score of 0.8059 and an AUC of 0.7493, demonstrating reasonable discriminative power.
The Decision Tree Classifier excelled in recall (0.8681) and F1 score (0.8117), indicating a strong ability to capture true positive cases—patients who survived five years or more. However, its AUC of 0.7109, slightly lower than that of the logistic model, suggests a reduced ability to differentiate between positive and negative classes at various thresholds.
The Neural Network outperformed both traditional models in several key metrics, including precision (0.7693) and AUC (0.7677). Its ROC curve showed the most favorable balance between sensitivity and specificity, highlighting its superior ability to correctly classify patients across varying decision thresholds.
