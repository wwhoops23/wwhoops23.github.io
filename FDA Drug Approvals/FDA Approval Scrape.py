#!/usr/bin/env python
# coding: utf-8

# # FDA Drug Approval Scrape
# ## By: Jake Indursky
# 
# ### Process:
# - The FDA drug approval website has unique pages for each month of drug approvals 
# - Instead of downloading 480 unique CSVs, this code iterates through each month's web page 
#     - and scrapes the information into a dataframe
# 
# ### Notes:
# - There are some months that had to be skipped over and need to be manually input

# ### Scrape FDA Website

# In[40]:


import requests
from bs4 import BeautifulSoup
import pandas as pd
import datetime

months_back = 12 * 40
current_date = datetime.datetime.now()

def get_data_for_month(month, year):
    url = f"https://www.accessdata.fda.gov/scripts/cder/daf/index.cfm?event=reportsSearch.process&rptName=1&reportSelectMonth={month}&reportSelectYear={year}&nav"
    response = requests.get(url)
    soup = BeautifulSoup(response.text, "html.parser")
    table = soup.find("table")
    if table:
        df = pd.read_html(str(table))[0]
        df['Month'] = f"{month}/{year}"
        return df
    else:
        return pd.DataFrame({"Month": [f"{month}/{year}"], "NO TABLE FOUND": [True]})

current_date = datetime.datetime.now()
all_data = []

for i in range(12*40):
    month = current_date.month
    year = current_date.year
    df = get_data_for_month(month, year)
    all_data.append(df)
    current_date -= datetime.timedelta(days=30)

result = pd.concat(all_data)

