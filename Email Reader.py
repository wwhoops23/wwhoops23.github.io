#!/usr/bin/env python
# coding: utf-8

# # Export Email Content to CSV
# 
# - This code simply iterates through email files and extracts the sender and receiver information and content

# In[4]:


import os
import email
import pandas as pd

# Function to extract email addresses from email messages
def extract_email_data(file_path):
    with open(file_path, 'r', encoding='utf-8') as file:
        msg = email.message_from_file(file)
        sender = msg.get('From')
        receiver = msg.get('To')
        content = ''
        if msg.is_multipart():
            for part in msg.walk():
                if part.get_content_type() == 'text/plain':
                    content += part.get_payload(decode=True).decode('utf-8', 'ignore')
        else:
            content = msg.get_payload(decode=True).decode('utf-8', 'ignore')
        return sender, receiver, content

# Path to the folder containing email messages
folder_path = 'INSERT HERE'
output_folder = 'INSERT HERE'  # Path to the folder where you want to save the CSV

# List to store email data
data = []

# Iterate through each file in the folder
for filename in os.listdir(folder_path):
    file_path = os.path.join(folder_path, filename)
    if os.path.isfile(file_path):
        sender, receiver, content = extract_email_data(file_path)
        data.append({'Sender': sender, 'Receiver': receiver, 'Content': content})

# Create a DataFrame
df = pd.DataFrame(data)

# Output the DataFrame
output_csv = os.path.join(output_folder, 'email_addresses.csv')
df.to_csv(output_csv, index=False)


# In[9]:





# In[ ]:




