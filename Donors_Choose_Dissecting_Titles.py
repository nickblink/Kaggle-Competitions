# This Python 3 environment comes with many helpful analytics libraries installed
# It is defined by the kaggle/python docker image: https://github.com/kaggle/docker-python
# For example, here's several helpful packages to load in 

import numpy as np # linear algebra
import pandas as pd # data processing, CSV file I/O (e.g. pd.read_csv)

# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory

from collections import Counter
import json
import os

def find_second_last(text,pattern):
    return (text.rfind(pattern,0,text.rfind(pattern)))
    
# function to split data. Because there is text data with commas and the data is in a comma-separated format,
# this takes some wrangling.


print(os.listdir("../input"))
os.chdir("../input")

with open('train.csv','r') as f1:
    train = f1.read()

train = train.strip().split('\n')
print(train[0])

#titles = [x.split(',')[8] for x in train][1:] # this ain't the way to do it.
#train_approvals = [x.split(',')[-1] for x in train[1:]]

########## Get inverse document frequency ###################
doc_freq = {}
doc_count = {}
N = len(titles)
for i in range(N):
    title = titles[i]
    for useless in [',','\\','.','!','"','\"','?',':']:
        title = title.replace(useless,'') # what about comma but not space separated words?
    title = title.lower()
    title = title.split(' ')
    while '' in title:
        title.remove('')
    for word in np.unique(title):
        try:
            doc_freq[word] += 1/N
            doc_count[word] += 1
        except:
            doc_freq[word] = 1/N
            doc_count[word] = 1

num = []        
for key in doc_count.keys():
    num.append(doc_count[key])
num_count = Counter(num)

cnt = 0
too_few = []
for key in doc_freq.keys():
    if doc_freq[key] > .05:
        print(key)
        print(doc_freq[key])
    if doc_freq[key] <= 10/N: # term only occurs in fewer than 10 documents
        too_few.append(key)
        cnt = cnt + 1
        
for key in too_few:
    del doc_freq[key]


################# counting terms by title ##################
term_list = list(doc_freq.keys())
#title_terms = [None] * N
title_terms = np.zeros(shape=(N,len(term_list)))
for i in range(N):
    title = titles[i]
    for useless in [',','\\','.','!','"','\"','?',':']:
        title = title.replace(useless,'') # what about comma but not space separated words?
    title = title.lower()
    title = title.split(' ')
    while '' in title:
        title.remove('')
    title_new = []
    for j in range(len(title)):
        if title[j] in term_list:
            title_terms[i, term_list.index(title[j])] += 1
    

def split_line(line):
    # Dont count ""
    line_list = list()
    end_of_line = line[find_second_last(line,','):]
    line_short = line[:find_second_last(line,',')]
    max_iter = 0
    while(line_short.find('\",')>=0 or line_short.find(',\"')>=0) and max_iter<20:
        while(line_short[0] == '\"'):
            line_short = line_short[1:]
        max_iter += 1
        if(line_short.find('\",')>= 0 and line_short.find(',\"')>= 0):
            ind = min(line_short.find('\",'),line_short.find(',\"'))
        else:
            ind = max(line_short.find('\",'),line_short.find(',\"'))
        #if(line_short[ind-1])
        line_list.append(line_short[:ind])
        line_short = line_short[(ind+2):]
        if line_short[:3]==',,\"':
            line_short=line_short[2:]
            line_list.append('')
            line_list.append('')
    line_list.append(line_short)
    line_list.append(end_of_line[1:(end_of_line.rfind(','))])
    line_list.append(end_of_line[(end_of_line.rfind(',')+1):])
    return(line_list)


train[0].count(',') # 15 commas, so sixteen columns. But what to do with commas that are within text?
train[0].count('\",\"')
comma_cnt = list()
n=0
for i in range(1,len(train)):
    #line_test = train[i][:find_second_last(train[i],',')]
    #comma_cnt.append(train[i].count('\",\"')+train[i].count('\",,\"')+train[i].count('\",,,\"'))
    #comma_cnt.append(line_test.count('\",\"')+line_test.count('\",,\"')+line_test.count('\",,,\"'))
    score = len(split_line(train[i]))
    comma_cnt.append(score)
    # # if score<17:
    #     n=1
    #     print(i)
    # if score>17 and n==1:
    #     print(i)
    #     break
comma_counter = Counter(comma_cnt) # Only 1204 entries with just 15 columns. The other 178k have commas, so what to do with that?      
comma_counter
# So I could get 95% of the samples by separating out ", "," ," and , at the end. Not bad, but I should get all



#with open('doc_freq.txt', 'w') as f1:
#    f1.write('hey')
#if np.log10(i)%1 == 0:
#doc_freq = order_dict(doc_freq)

# real basic -> just choose top ten terms as features. That's wack though. It's probably best to do PCA to all terms.
# hold up -> this only gives the document frequency for the idf. Need to do term frequency.
# could create a dict of the top ~2000 terms for each doc. Or I could just create a new dict for each doc.
# Could also simply use length of essay as a feature

######### Other Ideas #############
# - Group by type of words and predict on that (i.e. adjective, noun, adverb...)














