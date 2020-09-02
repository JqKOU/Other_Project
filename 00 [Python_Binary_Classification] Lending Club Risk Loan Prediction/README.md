## file: data_preprocessing_LendingClub.ipynb

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

### Upcoming
binary classification modeling 
