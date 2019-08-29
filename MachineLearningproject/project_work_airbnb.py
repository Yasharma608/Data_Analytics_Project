# -*- coding: utf-8 -*-
"""
Created on Tue Feb 19 03:38:12 2019

@author: yasha_000
"""
#importing the library
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns

#loading the data
df = pd.read_csv('BristolAirbnbListings.csv')
x = df2.drop(['price'],axis=1)
y = df2['price'].values


#splitting the dataset into the training and testset

from sklearn.cross_validation import train_test_split
x_train, x_test, y_train, y_test = train_test_split(x,y,test_size = 0.2)            
            
#feature scaling
from sklearn.preprocessing import StandardScaler
sc_x = StandardScaler()
x_train = sc_x.fit_transform(x_train)
x_test = sc_x.transform(x_test)            

#fitting multiple linear regression to traing set
from sklearn.linear_model import LinearRegression
regressor = LinearRegression()
regressor.fit(X_train, Y_train)

#predicting the test set result()
y_test_pred = regressor.predict(x_test)
y_train_pred = regressor.predict(x_train)

from math import sqrt
from sklearn.metrics import mean_squared_error,r2_score

rms_ols2 = sqrt(mean_squared_error(y_test,y_test_pred))
print('MSE train: %.3f, test: %.3f' % (
        mean_squared_error(y_train, y_train_pred),
        mean_squared_error(y_test, y_test_pred)))
print('R^2 train: %.3f, test: %.3f' % (
        r2_score(y_train, y_train_pred),
        r2_score(y_test, y_test_pred)))
print(rms_ols2)
print('Variance score: %.2f' % r2_score(y_test, y_test_pred))



from sklearn.preprocessing import StandardScaler
sc_X = StandardScaler()
x_train = sc_X.fit_transform(x_train)
x_test = sc_X.transform(x_test)
sc_y = StandardScaler()
y_train = sc_y.fit_transform(y_train)



# Fitting Decision Tree Regression to the dataset
from sklearn.tree import DecisionTreeRegressor
regressor = DecisionTreeRegressor(random_state = 0)
regressor.fit(x_train, y_train)

# Predicting a new result
y_pred = regressor.predict(67)



# Feature Scaling
from sklearn.preprocessing import StandardScaler
sc_X = StandardScaler()
sc_y = StandardScaler()
x = sc_X.fit_transform(x)
y = sc_y.fit_transform(y)

# Fitting SVR to the dataset
from sklearn.svm import SVR
regressor = SVR(kernel = 'rbf')
regressor.fit(x, y)

# Predicting a new result
y_pred = regressor.predict(60)
y_pred = sc_y.inverse_transform(y_pred)


# Visualising the Test set results
plt.scatter(X_test, y_test, color = 'red')
plt.plot(X_train, regressor.predict(X_train), color = 'blue')
plt.title('Salary vs Experience (Test set)')
plt.xlabel('Years of Experience')
plt.ylabel('Salary')
plt.show()
import statsmodels.formula.api as sm
def backwardElimination(x, SL):
    numVars = len(x[0])
    temp = np.zeros((1945,1)).astype(int)
    for i in range(0, numVars):
        regressor_OLS = sm.OLS(y, x).fit()
        maxVar = max(regressor_OLS.pvalues).astype(float)
        adjR_before = regressor_OLS.rsquared_adj.astype(float)
        if maxVar > SL:
            for j in range(0, numVars - i):
                if (regressor_OLS.pvalues[j].astype(float) == maxVar):
                    temp[:,j] = x[:, j]
                    x = np.delete(x, j, 1)
                    tmp_regressor = sm.OLS(y, x).fit()
                    adjR_after = tmp_regressor.rsquared_adj.astype(float)
                    if (adjR_before >= adjR_after):
                        x_rollback = np.hstack((x, temp[:,[0,j]]))
                        x_rollback = np.delete(x_rollback, j, 1)
                        print (regressor_OLS.summary())
                        return x_rollback
                    else:
                        continue
    regressor_OLS.summary()
    return x
 











df1=df.dropna()
filter=df1['postcode'].str.contains('^[B|b][S|s][0-9|a-z|A-Z|\S]+')
df2= df1[filter]

#droping the unwanted data 
df2=df2.drop(['postcode'], axis=1)
#ovweview of data by HID
hd=df.head(10)
inf=df.info(10)
dec=df.describe()
df.hist(bins=50, figsize=(12,9))

#spiliting the dataset



# Taking care of missing data
from sklearn.preprocessing import Imputer
imputer = Imputer(missing_values = 'NaN', axis = 0)
imputer = imputer.fit(df2)
df2 = imputer.transform(df2)

# Encoding categorical data
# Encoding the Independent Variable
from sklearn.preprocessing import  OneHotEncoder
labelencoder_X = LabelEncoder()
onehotencoder_x = OneHotEncoder(categorical_features = [4])
df2 = onehotencoder_x.fit_transform(df2).toarray()


from sklearn.preprocessing import  OneHotEncoder
onehotencoder = OneHotEncoder(categorical_features = [5])
df2 = onehotencoder.fit_transform(df2).toarray()


from sklearn.preprocessing import  OneHotEncoder
onehotencoder = OneHotEncoder(categorical_features = [2])
df2 = onehotencoder.fit_transform(df2).toarray()

# Encoding the Dependent Variable
labelencoder_y = LabelEncoder()
y = labelencoder_y.fit_transform(y)

#what are the most popular areas
sns.lmplot(x='latitude', y='longitude', data=df2, fit_reg=False, size=34)
plt.xlim(51.4042986357,51.5125545036)
plt.ylim(2.7014181506, -2.5146350022)
plt.show()
plt.figtext(51.4666839,-2.5827410237,'Ashley', fontsize = 20)
plt.figtext(0.19,0.4,'Ciutat Vella', fontsize = 20)
plt.figtext(0.14,0.58,'Sants-Montjuïc', fontsize = 20)
plt.figtext(0.3,0.49,'Eixample', fontsize = 20)
plt.figtext(0.28,0.7,'Les Corts', fontsize = 20)
plt.figtext(0.36,0.65,'Sarrià-Sant Gervasi', fontsize = 20)
plt.figtext(0.40,0.51,'Gràcia', fontsize = 20)
plt.figtext(0.62,0.42,'Nou Barris', fontsize = 20)
plt.figtext(0.55,0.32,'Sant Andreu', fontsize = 20)
plt.figtext(0.5,0.55,'Horta-Guinardó ', fontsize = 20)

plt.show()

unique=pd.unique(df2.neighbourhood,df2.longitude,df2.latitude)
long=pd.unique(df2.longitude)
lati=pd.unique(df2.latitude)

unique1=pd.DataFrame(unique)


these all should be in int or float for this i am using regex or pd
review_scores_rating              2039 non-null object
review_scores_accuracy            2037 non-null object
review_scores_cleanliness         2037 non-null object
review_scores_checkin             2035 non-null object

accommodates                      2375 non-null object
bathrooms                         2372 non-null object
bedrooms                          2372 non-null object
beds                              2372 non-null object

boleans =[]
for length in df1['review_scores_rating']:
        if length = range[:10]:
            booleans.append(True)
        else:
            booleans.append(False)
            
sns.lmplot(x='latitude', y='longitude', data=df2, fit_reg=False, hue='neighbourhood', size=10)
plt.ylim(2.58,2.78)
plt.xlim(51.45,51.59) 
plt.figtext(51.4,2.58,'Ashley', fontsize = 20)

plt.show
plt.figtext(0.38,0.28,'Sant Martí', fontsize = 20)
plt.figtext(0.19,0.4,'Ciutat Vella', fontsize = 20)
plt.figtext(0.14,0.58,'Sants-Montjuïc', fontsize = 20)
plt.figtext(0.3,0.49,'Eixample', fontsize = 20)
plt.figtext(0.28,0.7,'Les Corts', fontsize = 20)
plt.figtext(0.36,0.65,'Sarrià-Sant Gervasi', fontsize = 20)
plt.figtext(0.40,0.51,'Gràcia', fontsize = 20)
plt.figtext(0.62,0.42,'Nou Barris', fontsize = 20)
plt.figtext(0.55,0.32,'Sant Andreu', fontsize = 20)
plt.figtext(0.5,0.55,'Horta-Guinardó ', fontsize = 20)

plt.show()            
            
            

#splitting the dataset into the training and testset

from sklearn.cross_validation import train_test_split
X_train, X_test, Y_train, Y_test = train_test_split(x,y,test_size = 0.2)            
            
            
            
            
            