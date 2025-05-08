## Heart Attack Risk Prediction Model

This application uses a lasso logistic regression model to predict the risk of heart attack based on patient data. 

You can find the code for the model and application on [GitHub](https://github.com/avrincon/predicting-heart-attacks).

### Key Predictors

The most important predictors in the model are:

- **Troponin**: A protein released into the bloodstream during a heart attack
- **CK-MB (Creatine Kinase-MB)**: An enzyme released when heart muscle is damaged

### Model Details

The model was fitted on the [Heart Attack Risk Assessment Dataset](https://www.kaggle.com/datasets/fajobgiua/heart-attack-risk-assessment-dataset/data) from Kaggle.
Three anomalous data points for heart rate were removed from the dataset.
In addition, troponin and CK-MB values were log-transformed to reduce skewness and improve model convergence.

### Disclaimer

This app is for educational purposes only and should not be used for medical diagnosis.