#install.packages('TTR')
library('TTR')
## Reading Stock Data from Python output ##
stock.AllData=read.csv("StockData.csv",header=TRUE)[-1,-1]
stock.close <- stock.AllData[,seq(6,3024,6)]
stock.open <- stock.AllData[,seq(1,3024,6)]
stock.high <- stock.AllData[,seq(2,3024,6)]
stock.low <- stock.AllData[,seq(3,3024,6)]
## Filter the stocks which do not have enough data  
stock.close <- Filter(function(x)!any(is.na(x)), stock.close)
stock.open <- Filter(function(x)!any(is.na(x)), stock.open)
stock.high <- Filter(function(x)!any(is.na(x)), stock.high)
stock.low <- Filter(function(x)!any(is.na(x)), stock.low)
stock.AllData <- Filter(function(x)!any(is.na(x)), stock.AllData)
#mode(stock.close)

## Convert the stock.close into matrix ##
close.Matrix <- matrix(data = NA, nrow = dim(stock.close)[1], ncol = dim(stock.close)[2])
for (i in 1:dim(stock.close)[2])
{
  close.Matrix[,i] <- c(as.numeric(stock.close[[i]]))
}
stock.close <- close.Matrix
AllData.Matrix <- matrix(data = NA, nrow = dim(stock.AllData)[1], ncol = dim(stock.AllData)[2])

for (i in 1:dim(stock.close)[2])
{
  AllData.Matrix[,i] <- c(as.numeric(stock.AllData[[i]]))
}
stock.AllData <- AllData.Matrix
#mode(AllData.Matrix)
rm(i,close.Matrix, AllData.Matrix)


########################################################### Step 2 Bootstrap on time-series analysis #########################################################
##  DataSet.close is the result of Bootstrap, the DataSet[,,1] is the historical data.

# install.packages('tseries')
# install.packages('forecast')
# install.packages('zoo')
library(tseries)
library(forecast)

DataSet.close <- array(data=NA, dim=c(1258,476,201)) # dim=c(,, n(bootstrap)+1 )
DataSet.close[,,1] <- as.matrix(stock.close)

for ( stock in 1:ncol(stock.close)) # 200 bootstraps
{
  for ( bootstrap in 2:3 )
  {
    arima.para <- auto.arima(stock.close[,stock])
    DataSet.close[,stock,bootstrap] <- simulate(arima.para)
  }
}
rm(bootstrap,stock,arima.para)
##########################################################  Step 3 Efficient Froniter analysis ###################################################################
## weights is the result of efficient frontier analysis.
## weights[,1] is the EF result for the historical data.

#install.packages("fPortfolio")
library(fPortfolio)

Weights <- matrix(0,nrow = 201, ncol = ncol(stock.AllData))  ## nrow = 1+n(bootstrap)
for( portfolio in 1:4)
{
  #The function tangencyPortfolio returns the portfolio with the highest return/risk ratio on the efficient frontier.
  #For the Markowitz portfolio this is the same as the Sharpe ratio. 
  temp <- tangencyPortfolio( data = as.timeSeries(DataSet.close[,,portfolio]))
  Weights[portfolio,] <- temp@spec@portfolio$weights
}
rm(temp,portfolio)




######################################################## Additional Funciton  ###########################################################################3
##  Floor numbers at Hundreds : e.g. FloorHundred(123.12) -> 100 
FloorHundred <- function(data){
  data <- floor(data/100) * 100
  return(data)
}

######################################################### Strategy name #######################################################################
##  EMA 200 Calculation
SMA <-  matrix(data = NA, nrow = nrow(stock.AllData), ncol = (ncol(stock.AllData)/6))
EMA <- matrix(data = NA, nrow = nrow(stock.AllData), ncol = (ncol(stock.AllData)/6)) 
for(stock in 1:ncol(stock.close)){
  for(day in 200:nrow(stock.close))
  {
    SMA[day,stock] <- mean(stock.close[day-199:day,stock])
  }
  EMA[200,stock] <- SMA[200,stock]
  for(day in 201:nrow(stock.close))
  {
    EMA[day,stock] <- (stock.close[day,stock] - EMA[day-1,stock] ) * 2/201 + EMA[day-1,stock]
  }
}

## stochastic RSI calculation
PriceRSI <- matrix(data = 0, nrow = nrow(stock.AllData), ncol = (ncol(stock.AllData)/6))
minRSI <- matrix(data = 0, nrow = nrow(stock.AllData), ncol = (ncol(stock.AllData)/6)) 
maxRSI <- matrix(data = 0, nrow = nrow(stock.AllData), ncol = (ncol(stock.AllData)/6))
stoRSI <- matrix(data = 0, nrow = nrow(stock.AllData), ncol = (ncol(stock.AllData)/6))
for(stock in 1:ncol(stock.close))
{
  PriceRSI[,stock] <- RSI(stock.close[,stock]) ## Use RSI to calculate max RSI and min RSI
  
  for(day in 14 : nrow(stock.close))
  {
    minRSI[day,stock]<-min(PriceRSI[(day-13):day,stock])
    maxRSI[day,stock]<-max(PriceRSI[(day-13):day,stock])
    ##calculate stoRSI with formula
    stoRSI[day,stock]<-(PriceRSI[day,stock]-minRSI[day,stock])/(maxRSI[day,stock]-minRSI[day,stock])
  }
}


#################################################### Stochastic RSI Execution  #############################################################################
trades <- matrix(data = 0, nrow = nrow(stock.AllData), ncol = (ncol(stock.AllData)/6*3) ) 
colnames(trades) <- rep(c('buy & sell prices','Start & Hold days','principal & Shares'),ncol(stock.close))
## Every trade have six records, as follow,
##  'buy & sell prices'   'Start & Hold days'   'principal & Shares'
##      +200                    135                     100000
##      -260                    24                        500

for (stock in 1:ncol(stock.close)) 
{
  trades[1,stock*3] <- 1000000 * Weights[1,stock] ## set up original principal
  n <- 1 # pointer in trade[,stock]
  changes <- 0 # initial the changes, which is the money left after every buy transaction 
  flag <- 1 # mark of trade
  
  ## if weights == 0, then do not invest in this stock at all.
  
  if ( Weights[1,stock]!=0)
  {
    
    for(day in 201:nrow(stock.close) )
    {
        if ( flag%%2==1 ) 
        {
            #Oversold, Buy signal
            if ( stoRSI[day,stock] < 0.2 && EMA[day,stock] < stock.close[day,stock] )
            {
              trades[n,stock*3-2] <- stock.close[day,stock] # record price of this buy
              trades[n+1,stock*3] <- FloorHundred((trades[n,stock*3]/stock.close[day,stock]) ) # record shares of this buy transaction
              changes <- trades[n,stock*3] - stock.close[day,stock] * trades[n+1,stock*3] # record the changes unused in trade
              trades[n,stock*3-1] <- day # record the date of this buy transaction
              HoldDay <- day  # After but trasaction, begin to calculate holding days.
              n <- n+1
              flag <- flag + 1 # Change flag for sell
              #print(flag)     
            }
            if (EMA[day,stock] > stock.close[day,stock] && stoRSI[day,stock] < 0.2 && stock.close[day,stock] < stock.open[day,stock]
                && abs(stock.open[day,stock]-stock.close[day,stock]) > 0.7*(stock.high[day,stock] - stock.low[day,stock]))
            {
              trades[n,stock*3-2] <- stock.close[day,stock] # record price of this buy
              trades[n+1,stock*3] <- FloorHundred((trades[n,stock*3]/stock.close[day,stock]) ) # record shares of this buy transaction
              changes <- trades[n,stock*3] - stock.close[day,stock] * trades[n+1,stock*3] # record the changes unused in trade
              trades[n,stock*3-1] <- day # record the date of this buy transaction
              HoldDay <- day  # After but trasaction, begin to calculate holding days.
              n <- n+1
              flag <- flag + 1 # Change flag for sell
              #print(flag)     
            }
        }
        #Overbought, Sell signal
        if ( flag%%2==0 ) 
        {  
            if(stoRSI[day,stock]>0.5)
            {
              trades[n,stock*3-2] <- -(stock.close[day,stock]) # record the price of this sell
              trades[n,stock*3-1] <- day - HoldDay # record the holding day
              trades[n+1,stock*3] <- stock.close[day,stock] * trades[n,stock*3] + changes # record the holding cash after sell
              n <- n+1
              flag <- flag+1
            }
            ## stop-loss at 93%
#             if ( stock.close[day,stock] < abs(trades[n-1,stock*3-2]*0.93) && flag%%2==0 ) 
#             {
#               trades[n,stock*3-2] <- -(stock.close[day,stock]) # record the price of this sell
#               trades[n,stock*3-1] <- day - HoldDay # record the holding day
#               trades[n+1,stock*3] <- stock.close[day,stock] * trades[n,stock*3] + changes # record the holding cash after sell
#               n <- n+1
#               flag <- flag+1
#             }
            ## stop-win at 108%  
            if ( stock.close[day,stock] > abs(trades[n-1,stock*3-2]*1.08) && flag%%2==0 ) 
            {
              trades[n,stock*3-2] <- -(stock.close[day,stock]) # record the price of this sell
              trades[n,stock*3-1] <- day - HoldDay # record the holding day
              trades[n+1,stock*3] <- stock.close[day,stock] * trades[n,stock*3] + changes # record the holding cash after sell
              n <- n+1
              flag <- flag+1
            }
        }
      # delete non-round-trip trades
      if (flag%%2==0 && day==nrow(stock.close) )
      {
        # delete records of buy price, buy shares
        trades[n-1,stock*3-2] <- trades[n,stock*3] <- trades[n-1,stock*3-1] <- 0
      }
    }
  }
}
######################################################## Performance of sRSI_EMA & VWAP  ##################################################################

## Total Net Profit

# Initial the variable  
sRSI_EMA.GrossLoss <- sRSI_EMA.GrossProfit <- sRSI_EMA.LossNum <- sRSI_EMA.WinNum <- sRSI_EMA.EvenNum <- sRSI_EMA.Max.Con.Win <- sRSI_EMA.Max.Con.Loss <- 0

for ( stock in seq(3,ncol(trades),3) )
{
  n <- 1 # the No. of trades
  flag <-  0 # mark for trade statu to calculate the Max. Consecutive Winning or Losing trades.
  sRSI_EMA.Max.Con.Loss.Counter <- sRSI_EMA.Max.Con.Win.Counter <- 0 # initial the max consecutive counter
  
  while (trades[n,stock]!=0 && trades[n,stock-2]!=0 && trades[n+1,stock]!=0 && trades[n+1,stock-2]!=0) ## the principal = 0, which means there is no investment in this stock.
  {
    ## Profit and Loss Calculation ( 3 if()s )
    
    ## Losing Trade
    if (trades[n+2,stock] < trades[n,stock] ) # principal become less after this transaction
    {
      sRSI_EMA.GrossLoss <- sRSI_EMA.GrossLoss - abs(trades[n,stock-2]+trades[n+1,stock-2]) * trades[n+1,stock] # cumulate the Gross Loss
      sRSI_EMA.LossNum <- sRSI_EMA.LossNum + 1 # cumulate loss times
      
      # Calculate the Max. Consecutive Losing trades.
      if ( flag == 2 ) # flag=2 means it is a loss trade last time.
      {
        sRSI_EMA.Max.Con.Loss.Counter <- sRSI_EMA.Max.Con.Loss.Counter + 1
        if ( sRSI_EMA.Max.Con.Loss.Counter > sRSI_EMA.Max.Con.Loss) # Check whether it is the max consecutive times
        {
          sRSI_EMA.Max.Con.Loss <- sRSI_EMA.Max.Con.Loss.Counter
        }
        flag <- 2
      }
      if ( flag != 2) # flag!=2 means it is not a loss trade last time.
      {
        sRSI_EMA.Max.Con.Loss.Counter <- 1
        flag <- 2
      }
    }
    
    ## Winning Trade
    if (trades[n+2,stock] > trades[n,stock] ) # principal become more after this transaction
    {
      sRSI_EMA.GrossProfit <- sRSI_EMA.GrossProfit + abs(trades[n,stock-2]+trades[n+1,stock-2]) * trades[n+1,stock] # cumulate the Gross Profit
      sRSI_EMA.WinNum <- sRSI_EMA.WinNum + 1 # cumulate win times
      
      # Calculate the Max. Consecutive Losing trades.
      if ( flag == 1 ) # flag=1 means it is a win trade last time.
      {
        sRSI_EMA.Max.Con.Win.Counter <- sRSI_EMA.Max.Con.Win.Counter + 1
        if ( sRSI_EMA.Max.Con.Win.Counter > sRSI_EMA.Max.Con.Win) # Check whether it is the max consecutive times
        {
          sRSI_EMA.Max.Con.Win <- sRSI_EMA.Max.Con.Win.Counter 
        }
        flag <- 1
      }
      if ( flag != 1) # flag!=1 means it is not a win trade last time.
      {
        sRSI_EMA.Max.Con.Win.Counter <- 1
        flag <- 1
      }
    }
    
    ## Even Trade    
    if (trades[n+2,stock] == trades[n,stock] ) # principal didn't change after this transaction
    {
      sRSI_EMA.EvenNum <- sRSI_EMA.EvenNum +1 # cumulate even times
      flag <- 0 # initial the flag for max consecutive counter
    }
    
    n <- n+2 # step to next trade # every trade in trades[] has two line records
    
  }
}
rm(sRSI_EMA.Max.Con.Win.Counter,sRSI_EMA.Max.Con.Loss.Counter)

## sRSI_EMA.Total.TradeNum : Total Number of Trades
sRSI_EMA.Total.TradeNum <- sRSI_EMA.LossNum + sRSI_EMA.WinNum + sRSI_EMA.EvenNum
## sRSI_EMA.Total.NetProfit
sRSI_EMA.Total.NetProfit <- sRSI_EMA.GrossProfit + sRSI_EMA.GrossLoss
## sRSI_EMA.Avg.NetProft : Avg. Trade Net Profit ($)
sRSI_EMA.Avg.NetProfit <- sRSI_EMA.Total.NetProfit / sRSI_EMA.Total.TradeNum
## sRSI_EMA.Avg.WinTrade : Avg. Profit of Winning Trade ($)
sRSI_EMA.Avg.WinTrade <- sRSI_EMA.GrossProfit / sRSI_EMA.WinNum
## sRSI_EMA.Avg.LossTrade : Avg. Loss of Losing Trade ($)
sRSI_EMA.Avg.LossTrade <- sRSI_EMA.GrossLoss / sRSI_EMA.LossNum
## sRSI_EMA.Ratio.Win2Loss : Ratio Avg. Win to Avg. Loss
sRSI_EMA.Ratio.Win2Loss <- abs(sRSI_EMA.Avg.WinTrade / sRSI_EMA.Avg.LossTrade)
######################################################## Trading Sample Plotting for NO.12 stock(MMM) ##################################################
# plot(sample.close, type='l', xlab='time', ylab='Price', main='Buy/Sell Signal')
day.end <- 500

plot(stock.close[1:day.end,12], type='l', lwd=1, xlab='time(1,500)', ylab='Price', main='stoRSI and EMA (200) backtest on MMM') # Plot the first 'day.end' days' price
# Calculate points for entry and exit.
point.buy <- point.sell <- matrix(data = NA, nrow = nrow(trades), ncol = 2)
n <- 1
while ( trades[2*n-1,34]!=0 && trades[2*n-1,35]!=0 && trades[n*2,34]!=0 && trades[n*2,35]!=0)
{
  point.buy[n,1] <- trades[2*n-1,34] # Buy price
  point.buy[n,2] <- trades[2*n-1,35] # Buy time
  # point.buy[n,2] <- sample.close[trades[2*n-1,35],1] # Buy time with ts
  
  point.sell[n,1] <- abs(trades[2*n,34]) # Sell price
  point.sell[n,2] <- trades[2*n-1,35] + trades[2*n,35]
  # point.sell[n,2] <- sample.close[trades[2*n-1,35] + trades[2*n,35], 1] # Sell time with ts
  n <- n+1
}

points(x = point.buy[1:day.end,2], y = point.buy[1:day.end,1], col='green' ) # Point the first 'day.end' days' entry as green
points(x = point.sell[1:day.end,2], y = point.sell[1:day.end,1], col='red') # Point the first 'day.end' days' exit as red

# change indicators for your strategies
lines(EMA[1:day.end,12], col='orange') # Line the first 'day.end' days' EMA

legend(x = 'bottomright', inset = 0.05, legend = c('Price', 'EMA(200)', 'Entry', 'Exit'), lty = c(1,1,NA,NA), col = c('black','orange','green','red'), pch = c(NA,NA,1,1) )
plot(stoRSI[1:day.end,12], type='l', lwd=1, xlab='time(1,500)', ylab='stoRSI', main='stoRSI  backtest on MMM')
