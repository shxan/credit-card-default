# Credit Card Application Approval
Maximizing profitability of credit card business by: 1) predicting probability of default for new applicants and 2) deciding profit-maximizing cut-off 

## Executive Summary
This model serves to inform a hypothetical bank’s decisions on who to give credit to, based on demographics factors such as age, gender, education level, marriage status, and bank specific factors such as payment history, bill history, etc. Feature engineering was done to increase the number of parameters.

Loss from false positives were >3 times the profit from true positive, at -$5000 and +$1500 respectively, and we optimised the cut-off threshold to be 0.2308. This means that, if probability of default is >23%, the application should be declined to maximize profits.

Default probability prediction were modelled using AIC, CTree, RPART, Random Forest and XGBoost. The final model selected was an ensemble model, which took the maximum probability of default predicted from each model, which outperformed the next best model (AIC) by 1.3%. (i.e. generates 1.3% higher profit)

When compared against the evaluation dataset, this model generated a profit of 560k from 1000 clients, which places it in the top quintile for model performance. This performance is in line with expectations from the testing dataset.

## Data preparation and cleaning
All 25 variables were corrected classified to their respective data type (e.g. Sex from INT to Factor, Payment statuses as factor, etc.). 

Several levels had too little data points once reclassifications were done (e.g. very late payments, or less often used payment statuses). These were combined to prevent the model from learning irrelevant trends from invalid groups and hence overfit.

Additional feature engineering were done based on our contextual knowledge of the industry, and to generate additional indicators of good credit history. Some examples are: Percent of bill paid,  Percent of good behavior, etc.

## Analysis
Threshold cutoff is determined to be 0.2307 by using the formula: Profit from true positive / (Profit from true positive + Absolute Loss from false negative). This formula works because profits and losses are linear in this case, and we could create a goal seek to maximize profit in more complicated cases.

The objective function used is not AUC or accuracy as with traditional analytics projects. More accurately, it should be represented by: 
    | Actual: no default | Actual: default
---- | ---- | ----
Predicted: no default | $1500 | -$5000
Predicted: default | $0 | $0

Data is split into 16000 in training dataset and 8000 in testing dataset.

### Stepwise Logit Regression 
AIC is used as criterion

    | Actual: no default | Actual: default
---- | ---- | ----
Predicted: no default | 5239 | 770
Predicted: default | 993 | 998

![logit plot](/graphs/stepwise-logit-plot.png)
![logit lift](/graphs/stepwise-logit-lift.png)
![logit roc](/graphs/stepwise-logit-roc.png)


### CART - Classification Tree  

    | Actual: no default | Actual: default
---- | ---- | ----
Predicted: no default | 4995 | 713
Predicted: default | 1237 | 1055

![ctree plot](/graphs/ctree.png)
![ctree lift](/graphs/ctree-lift.png)
![ctree roc](/graphs/ctree-roc.png)

### RPart

    | Actual: no default | Actual: default
---- | ---- | ----
Predicted: no default | 5937 | 1162
Predicted: default | 295 | 606

![rpart leaves](/graphs/rpart-leaves.png)
![rpart error](/graphs/rpart-error.png)
![rpart roc(/graphs/rpart-roc.png)

### Random Forest

    | Actual: no default | Actual: default
---- | ---- | ----
Predicted: no default | 5831 | 1077
Predicted: default | 401 | 691

![random forest importance](/graphs/random-forest-importance.png)
![random forest plot](/graphs/random-forest-plot.png)
![random forest lift](/graphs/random-forest-lift.png)
![random forest roc](/graphs/random-forest-roc.png)

### Gradient Boosting - XGBoost

    | Actual: no default | Actual: default
---- | ---- | ----
Predicted: no default | 6066 | 1392
Predicted: default | 166 | 376

![xgboost plot](/graphs/xgboost.png)
![xgboost lift](/graphs/xgboost-lift.png)

## Conclusion

Of the various models, AIC generated the highest profit, followed by CTree and XGBoost. We also explored creating an Ensemble model using various methods on probability of default such as mean, median, max, etc. and found that taking the maximum probability of default across all models produces the best result in the testing dataset. 

While the Ensemble model performed the best in this case, we believe a better performing model can be created if we further optimise the parameters on gradient boosting and random forest. 
