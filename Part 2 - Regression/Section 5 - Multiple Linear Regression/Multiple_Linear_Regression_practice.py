##
import numpy as np
import pandas as pd
from sklearn.compose import ColumnTransformer
from sklearn.preprocessing import OneHotEncoder
from sklearn.model_selection import train_test_split
from sklearn.linear_model import LinearRegression
import matplotlib.pyplot as plt

##
dataset = pd.read_csv('Part 2 - Regression/Section 5 - Multiple Linear Regression/50_Startups.csv')
##
X = dataset.iloc[:, :-1].values
Y = dataset.iloc[:, -1].values
##
ct = ColumnTransformer(transformers=[('encode', OneHotEncoder(), [-1])], remainder='passthrough')
X = np.array(ct.fit_transform(X))
##
X_train, X_test, Y_train, Y_test = train_test_split(X, Y, train_size=0.8)
##
regressor = LinearRegression()
regressor.fit(X_train, Y_train)
##
Y_pred = regressor.predict(X_test)
np.set_printoptions(precision=2)
print(np.concatenate((Y_pred.reshape(len(Y_pred), 1), Y_test.reshape(len(Y_test), 1)), 1))

