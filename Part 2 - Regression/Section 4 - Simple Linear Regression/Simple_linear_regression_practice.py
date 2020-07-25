##
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from sklearn.model_selection import train_test_split
from sklearn.linear_model import LinearRegression

##
dataset = pd.read_csv('Part 2 - Regression\Section 4 - Simple Linear Regression\Salary_Data.csv')
##
X = dataset.iloc[:, :-1].values
Y = dataset.iloc[:, -1].values
##
X_train, X_test, Y_train, Y_test = train_test_split(X, Y, test_size=0.2)
##
regressor = LinearRegression()
regressor.fit(X_train, Y_train)
##
y_pred = regressor.predict(X_test)
##
plt.scatter(X_train, Y_train, color='red')
plt.plot(X_train, regressor.predict(X_train), color='blue')
plt.title("Salary vs Experience (Training Set)")
plt.xlabel("Experience (Years )")
plt.ylabel("Salary")
# plt.subplot(1, 2, 1, figsize=(1, 1))
plt.show()
##
plt.scatter(X_test, Y_test, color='red')
plt.plot(X_train, regressor.predict(X_train), color='blue')
plt.title("Salary vs Experience (Test Set)")
plt.xlabel("Experience (Years )")
plt.ylabel("Salary")
# plt.subplot(1, 2, 2, figsize=(1, 1))
plt.show()
