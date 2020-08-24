
#### Overview
This project focused on classification and prediction. The purpose of the analysis is to build a logistic regression model and use it to obtain a classification rule which is used to predict whether a credit card application can be approved.
#### Steps
1. remove obs with missing value, convert categorical variable to factor in R
2. make plots to explore the dataset
3. train logistic regression model use 10 fold validation, plot ROC calculate AUC, apply the rule to test set to calculate the accuracy
4. train logistic regression model with Lasso penalty, determine tuning parameter and cut-off point, apply to test set.
5. Compare model from 3 and 4 and make decision
