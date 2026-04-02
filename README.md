# Classification___Heart_Desease
The purpose of this study is to predict the risk of a myocardial infarction using data mining techniques

## 🩺 Introduction & Dataset

By analyzing various health factors, the research aims to estimate the probability of an individual suffering a heart attack, specifically comparing models like **K-Nearest Neighbors** (KNN) and **Logistic Regression**.

The study concludes that Logistic Regression is the superior model for this predictive task.
The dataset contains a total of 918 observations after removing duplicates. The analysis utilizes 12 variables, including *age*, *sex*, *chest pain type*, *resting blood pressure*, *cholesterol*, and *maximum heart rate*, with *HeartDisease* serving as the target variable for classification.
The sample is characterized as being 79% male with an average age of approximately 53.5 years

## ⚙️ Methods 

- **Logistic Regression**: this was chosen as the main model due to its ability to handle both qualitative and quantitative variables. Stepwise selection based on the Akaike Information Criterion (AIC) is applied to identify the most informative variables.

- **K-Nearest Neighbors** (KNN): this was tested as a non-parametric alternative. K=55 was the optimal tuning parameter, as it maximized accuracy on the validation set.

Linear Discriminant Analysis (LDA) and Quadratic Discriminant Analysis (QDA) were initially considered but discarded because the dataset did not meet the necessary normality assumptions.

## 🏆 Results

- **Validation Set** performance: KNN initially showed slightly higher accuracy (0.84) compared to logistic regression (0.82).
- **Test Set** performance: in the final evaluation on the test set, Logistic Regression proved superior, achieving an accuracy of 0.89 and an Area Under the Curve (AUC) of 0.93, while KNN's accuracy dropped to 0.79

The results indicate that increasing age, being male, high fasting blood sugar levels, and specific electrocardiogram abnormalities significantly increase the probability of a cardiac event.
