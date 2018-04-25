# -*- coding: utf-8 -*-
"""
Created on Sun Mar 18 20:05:30 2017

@author: John Li
"""

#import re, urllib.request, os
import re,urllib2,os
import pandas.io.data as web
from datetime import datetime
import pandas as pd

#Show and Set working Directory
os.getcwd()
os.chdir('F:\\Google Drive\\Study\\FE 800 Final Project\\code')


#make a new browser, this will download pages from the web for us. This is done by calling the 
#build_opener() method from the urllib2 library
#browser=urllib.request.build_opener()
browser=urllib2.build_opener()

#disguise the browser, so that websites think it is an actual browser running on a computer
browser.addheaders=[('User-agent', 'Mozilla/5.0')]

url='https://en.wikipedia.org/wiki/List_of_S%26P_500_companies'

#read the response in html format. This is essentially a long piece of text
#myHTML=bytes.decode(browser.open(url).read())
myHTML=browser.open(url).read()

#Regular expression for locating the key word
Tickers=re.finditer('<td><a rel="nofollow" class="external text" href="(.*?)">(.*?[^reports])</a></td>', myHTML)

#create a new file for output. The 'w' parameter signifies that the file will be used for writing.
fileWriter=open('SP500list.txt','w')
writer = pd.ExcelWriter('StockData.xlsx', engine='xlsxwriter')

TickerList=[]
start = datetime(2011,2,14)
end = datetime(2016,2,14)
stockRawData={}

for Ticker in Tickers:
        #Extract the second basket as the Tickername
        Tickername=Ticker.group(2)
        
        #Write tickername into file, seprate by ','
        fileWriter.write(Tickername+'\n')
        
        #Store tickername into a list
        TickerList.append(Tickername)
        
        #Download Stock Data by Pandas        
        stockRawData[Tickername] = web.DataReader(Tickername, 'yahoo', start, end)
        stockRawData[Tickername].columns = str(Tickername)+'.'+stockRawData[Tickername].columns
        
        if Tickername=='MMM':
            dataset = stockRawData['MMM']
        else:
            dataset = pd.concat([dataset, stockRawData[Tickername]], axis=1, join='outer')
        
        
dataset.to_excel(writer, sheet_name='Sheet1')
        
#Close writing action
fileWriter.close()
writer.save()
