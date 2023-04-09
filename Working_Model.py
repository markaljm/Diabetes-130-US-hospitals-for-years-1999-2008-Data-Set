#!/usr/bin/env python
# coding: utf-8

# In[1]:


import pandas as pd 
import numpy as np
import random # to generate random values


# In[2]:


df = pd.read_csv('diabetic_data.csv')


# # Investigating Data

# In[3]:


# Firstly we wish to see the number of rows and columns that the data set has.
df.shape


# In[4]:


gender_counts = df['gender'].value_counts()
print(gender_counts)


# The three records with "Unknown/Invalid" will be removed. This represents 0.003% of the data set

# In[5]:


race_counts = df['race'].value_counts()
print(race_counts)


# The records with "?" will be changed to "Other". This represents  2.23% of the data set.

# In[6]:


age_counts = df['age'].value_counts()
print(age_counts)


# The brackets will be removed and a random number, from within the given range, will be generated.

# In[7]:


weight_counts = df['weight'].value_counts()
print(weight_counts)


# 1. The brackets will be removed and a random number, from within the given range, will be generated.
# 
# 2. The values ">200" will be removed
# 
# 3. We will use the generated figures to help generate a model to help impue the 98,569 missing values.

# # Cleaning Data

# In[8]:


# replacing value in the "race" column

# replace '?' with 'other' in 'race' column
df['race'] = df['race'].replace('?', 'Other')

# Removing unwanted rows

# remove rows where the 'gender' column contains 'Unknown/Invalid'
df = df[df['gender'] != 'Unknown/Invalid']

# remove rows where the 'weight' column contains '>200'
df = df[df['weight'] != '>200']


# In[9]:


# define a function to transform the "weight" column
def transform_weight(w):
    if w == '?':
        return w
    else:
        a, b = w[1:-1].split('-')
        return np.random.uniform(float(a), float(b))

# apply the function to the "weight" column
df['weight'] = df['weight'].apply(transform_weight)


# In[10]:


# define a function to generate a random age given an age range string
def generate_age_from_range(age_range_str):
    # remove the brackets from the age range string
    age_range_str = age_range_str.replace('[', '').replace(')', '')

    # convert the age range string to a tuple of integers
    age_range = tuple(map(int, age_range_str.split('-')))

    # generate a random age between the two numbers in the age range
    generated_age = random.randint(age_range[0], age_range[1])

    return generated_age

# apply the function to the 'age_range' column to generate a new 'age' column
df['age'] = df['age'].apply(generate_age_from_range)


# In[11]:


race_counts = df['race'].value_counts()
print(race_counts)


# In[12]:


age_counts = df['age'].value_counts()
print(age_counts)


# In[13]:


weight_counts = df['weight'].value_counts()
print(weight_counts)


# In[14]:


# replace '?' with '' in 'weight' column
df['weight'] = df['weight'].replace('?', '')


# In[15]:


df.to_csv('roo.csv', index=False)


# # Regession Model

# In[16]:


from sklearn.model_selection import train_test_split
from sklearn.linear_model import LinearRegression


# In[17]:


# Load the data
df = pd.read_csv('roo.csv')


# In[18]:


# Encode the categorical columns
race_encoded = pd.get_dummies(df['race'], prefix='race')
gender_encoded = pd.get_dummies(df['gender'], prefix='gender')


# In[19]:


# Combine the encoded columns with the numerical columns
df_encoded = pd.concat([df[['weight', 'age']], race_encoded, gender_encoded], axis=1)


# In[20]:


# Split into complete rows and rows with missing weight
complete_rows_encoded = df_encoded.dropna(subset=['weight'])
missing_weight_rows_encoded = df_encoded[df_encoded['weight'].isna()]


# In[21]:


# Split into complete rows and rows with missing weight
complete_rows_encoded = df_encoded.dropna(subset=['weight'])
missing_weight_rows_encoded = df_encoded[df_encoded['weight'].isna()]


# In[22]:


# Split the complete data into training and testing sets
X_train, X_test, y_train, y_test = train_test_split(complete_rows_encoded.drop('weight', axis=1), 
                                                    complete_rows_encoded['weight'], 
                                                    test_size=0.3, 
                                                    random_state=42)


# In[23]:


# Fit the linear regression model using the training data
reg = LinearRegression().fit(X_train, y_train)


# In[24]:


# Fill in missing values with predicted weights
predicted_weights = reg.predict(missing_weight_rows_encoded.drop('weight', axis=1))
df_encoded.loc[df_encoded['weight'].isna(), 'weight'] = predicted_weights


# In[25]:


# Save the filled-in dataframe to a new csv file
df_encoded.to_csv('roo_filled.csv', index=False)


# In[ ]:




