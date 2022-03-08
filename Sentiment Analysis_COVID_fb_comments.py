#!/usr/bin/env python
# coding: utf-8

# In[3]:


import sys
get_ipython().system('{sys.executable} -m pip install pattern')


# In[1]:


from pattern.nl import sentiment
import pandas as pd
import numpy as np
import os


# In[2]:


os.getcwd() 
os.chdir('C:\\Users\\leale\\Desktop\\PhD\\3-Coding') 


# In[3]:


print(sentiment('Een onwijs spannend goed boek!'))


# In[4]:


file = 'Fb_data_forpy.csv'
df = pd.read_csv(file)


# In[5]:


df.head()


# In[6]:


index = df.index
number_of_rows = len(index)

print(number_of_rows)


# In[7]:


df['Sentiment'] = df.apply(lambda row: sentiment(row.message)[0], 1)


# In[10]:


df.head(50)


# In[9]:


#Add Subjectivity score column
df['Subjectivity'] = df.apply(lambda row: sentiment(row.message)[1], 1)


# In[56]:


# Each word in the lexicon has scores for:
# 1)     polarity: negative vs. positive    (-1.0 => +1.0)
# 2) subjectivity: objective vs. subjective (+0.0 => +1.0)
# 3)    intensity: modifies next word?      (x0.5 => x2.0)


# In[11]:


df.to_csv('FB_allergy_senti_scores.csv', encoding='utf-8')

