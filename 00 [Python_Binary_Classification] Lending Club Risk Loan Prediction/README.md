## file name: 0 data preprocessing LendingClub.ipynb

### Purpose¶
Preparing lending club data for binary class classification

### Overview¶
1. only keep rows with target classes ('Charged Off' and 'Fully Paid')

2. remove 
    
    * columns with unique value
    * columns contain more than 40% missing values
    * columns which may cause informatoin leakage
    * columns provide redundant information
    * rows with too many missing values
    
3. Categorical variable
    * remove variables with too many levels
    * dummy coding
    
4. fill missing values using Multivariate imputer

5. data normalization

6. train test spliting 

## file name: 1 Modeling on imbalanced dataset.ipynb
### Overview
Lending Club loan dataset is an imbalanced dataset with good and bad loan ratio around 8:2. The purpose of this task is to distinguish bad loans from good loans. There are several approaches to deal with this imbalanced probelm:

    1. Change the evaluation metric from accuracy to F1 score, since F1 score is a balanced mesaure of precision and recall. 
    2. Under, over or SMOTE sampling the training dataset to make classes balance, then train the model.
    3. Put more weight on the minority class.

Since imbalance ratio at 8:2 is not very severe, this notebook is modeled on the original imbalanced dataset and see what best approach it can get. The modeling steps are:

    1. Check correlation between description features and target feature
    2. Select best model from a list of candidates
    3. Model parameter tuning
    4. Threshold selection
#### outcome 
After all these steps, the final model is Logistic Regression with L1(Lasso) penalty C=1 and threshold 0.3. The best training performance are: F1: 69.04, Precision: 76.61, Recall: 62.83, AUC: 78.78, Accuracy: 87.84. The testing performance are: F1: 68.43, Precision: 75.75, Recall: 62.39, AUC: 78.46, Accuracy: 87.63. There is no big disparency between training and testing performance, which means overfitting is prohibited. 
