#Author: Carl Zheng
# Reset Work Directory
getwd()
setwd("F:/Google Drive/Study/FE 800 Final Project/code/")

######################################################  Step 1  Downlaod and Clean Data ####################################################
## stock.AllData, stock.close is the Downloada data.
## In the Stock.AllData, the sequence of every stock's data are Open, High, Low, Close, Volume and Adj.Close.

## Execute a python script for downloading list of SP500 from Wiki by General EXpression
#system("python getSPlist.py")

## In this step, the program fetch the list of components in SP500 from Wikipedia.
## And download historical data by the list.
## In R, it needs about 18 mins to complete this process.grep() or str_match_all(), getsymbols()
## However, it just costs 5 mins in Python.


## Reading Stock Data from Python output ##
stock.AllData=read.csv("StockData.csv",header=TRUE)[-1,-1]
stock.close <- stock.AllData[,seq(6,3025,6)]


## Filter the stocks which do not have enough data  
stock.close <- Filter(function(x)!any(is.na(x)), stock.close)
stock.AllData <- Filter(function(x)!any(is.na(x)), stock.AllData)
#mode(stock.close)


#Output the list of stocks with enough time period for backtest
# a <- colnames(stock.close)
# write.table(a, 'Filtered SP500.txt', sep = '\n')
# file.show('Filtered SP500.txt')
# rm(a)

## Convert the stock.close into matrix ##
close.Matrix <- matrix(data = NA, nrow = dim(stock.close)[1], ncol = dim(stock.close)[2])
for (i in 1:dim(stock.close)[2])
{
  close.Matrix[,i] <- c(as.numeric(stock.close[[i]]))
}
stock.close <- close.Matrix

AllData.Matrix <- matrix(data = NA, nrow = dim(stock.AllData)[1], ncol = dim(stock.AllData)[2])
for (i in 1:dim(stock.AllData)[2])
{
  AllData.Matrix[,i] <- c(as.numeric(stock.AllData[[i]]))
}
stock.AllData <- AllData.Matrix
#mode(AllData.Matrix)
rm(i,close.Matrix, AllData.Matrix)


#########################################################  Step 2 Bootstrap on time-series analysis ###############################################################33
##  DataSet.close is the result of Bootstrap, the DataSet[,,1] is the historical data.

#install.packages('tseries')
#install.packages('forecast')
#install.packages('stats')
#library(tseries)
library(forecast)
library(stats)

DataSet.close <- array(data=NA, dim=c(1258,476,4)) # dim=c(,, n(bootstrap)+1 )
DataSet.close[,,1] <- stock.close

for ( stock in 1:ncol(stock.close) ) 
{
  arima.para <- auto.arima(stock.close[,stock])
  
  for ( bootstrap in 2:4) # 200 bootstraps
  {
    DataSet.close[,stock,bootstrap] <- simulate(arima.para)
  }
}
rm(bootstrap,stock,arima.para)


###################################################  Step 3 Efficient Froniter analysis #####################################################################3
## weights is the result of efficient frontier analysis.
## weights[,1] is the EF result for the historical data.

#install.packages("fPortfolio")
library(fPortfolio)

Weights <- matrix(0,nrow = 201, ncol = 476)  ## nrow = 1+n(bootstrap)
for( portfolio in 1:4)
{
  #The function tangencyPortfolio returns the portfolio with the highest return/risk ratio on the efficient frontier.
  #For the Markowitz portfolio this is the same as the Sharpe ratio. 
  # temp <- tangencyPortfolio( data = as.timeSeries(DataSet.close[,,portfolio]))
  temp <- minriskPortfolio( data = as.timeSeries(DataSet.close[,,portfolio]))
  Weights[portfolio,] <- temp@spec@portfolio$weights
}
rm(temp,portfolio)

beep()


######################################################## Additional Funciton  ###########################################################################3
##  Floor numbers at Hundreds : e.g. FloorHundred(123.12) -> 100 
FloorHundred <- function(data)
{
  data <- floor(data/100) * 100
  return(data)
}

