#!/usr/bin/env python
# coding: utf-8

# In[ ]:


get_ipython().run_line_magic('matplotlib', 'inline')
import pandas as pd
import matplotlib.pyplot as plt


# In[14]:


df = pd.read_csv('')
df.head()


# In[ ]:


df['Added Vendor']=""
df['USD Amount']=""
df.head()


# In[ ]:


for i in df.index:
    df.at[i,'USD Amount']=df.at[i,'Amount']
df.head()


# In[ ]:


df2 = pd.read_csv('vendor.csv')
df2


# In[ ]:


key_vendor = df2.loc[0,'Vendor']
key_vendor


# In[ ]:


df.loc[df['Transaction Detail'].str.contains(key_vendor, case =False, regex = True)]


# In[ ]:


for i in df.index:
    if df.loc[i,'Transaction Detail'].find(key_vendor) !=-1:
        df.loc[i,'Added Vendor'] = key_vendor
df


# In[ ]:


for j in df2.index:
    key_vendor = df2.loc[j, 'Vendor']
    for i in df.index:
        if df.loc[i,'Transaction Detail'].find(key_vendor) !=-1:
            df.loc[i,'Added Vendor'] = key_vendor


# In[ ]:


df


# In[ ]:


df.to_csv('vendor_CC_DD_Mar_21.csv', index = False, header = True)


# In[ ]:





# In[ ]:




