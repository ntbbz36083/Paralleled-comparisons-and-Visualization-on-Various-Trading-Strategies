# Money Flow Index (MFI)

install.packages('TTR')
install.packages('PerformanceAnalytics')
install.packages('tseries')
library(TTR)
library(PerformanceAnalytics)
library(tseries)


########################################################### MFI calculate###############################################################
Tech_MFI <- cbind(stock.AllData[,6], MFI(stock.AllData[,c(2,3,6)], stock.AllData[,5]) ) # for the first stock

for (stock in seq(7,ncol(stock.AllData),6)) # for other stocks
{ 
  mfi <- MFI(stock.AllData[,c(stock+1,stock+2,stock+5)], stock.AllData[,stock+4])  
  temp_mfi <- cbind(stock.AllData[,stock+5],mfi)
  Tech_MFI <- cbind(Tech_MFI,temp_mfi)
}
rm(mfi,temp_mfi,stock)


###################################################### MFI trading execution############################################################
trade <- matrix(0,650,1428)    #### to store trade prices ## matrix(0,650,n*3=1428)

#trade <- matrix(data = NA, nrow = nrow(stock.AllData), ncol = (ncol(stock.AllData)/6*3) )
#colnames(trade) <- rep(c('buy & sell prices','Start & Hold days','principal & Shares'),476)
## Every trade have six records, as follow,
##  'buy & sell prices'   'Start & Hold days'   'principal & Shares'
##      +200                    135                     100000
##      -260                    24                        500

b <- c <- 1


for (i in seq(1,952,2))   #### seq(1,n*2=952,2)
{
  a <- 1
  Principal <- 1000000*Weights[1,c]
  trade[a,b+2] <- Principal
  
  ## if weights == 0, then do not invest in this stock at all.
  #if ( Weights[1,(i+1)/2] != 0)
  #{
  ## start to inpect every day price of a stock
  for (day in 16:nrow(Tech_MFI))
  {
    
    ### MFI cross 20 from above, then buy
    # if(Tech_MFI[day,i+1] <= 20 && Tech_MFI[day-1,i+1] > 20 && a%%2==1)
    if((Tech_MFI[day,i+1] <= 20 && Tech_MFI[day-1,i+1] > 20 && a%%2==1) | (News_signal[day,(i+1)*3+1] == 'ENTRY' && a%%2==1))
    {
      trade[a,b] <- Tech_MFI[day,i]
      BuyDay <- day 
      trade[a,b+1] <- BuyDay  # record the date of this buy transaction
      Shares <- FloorHundred(Principal/Tech_MFI[day,i]) # record shares of this buy transaction
      Changes <- Principal-FloorHundred(Principal/Tech_MFI[day,i])*Tech_MFI[day,i] # record the changes unused in trade
      trade[a+1,b+2] <- Shares 
      a <- a+1
    }
    
    ### MFI cross 80 from below, then sell
    if(a%%2==0)
    {
      # if(Tech_MFI[day,i+1] >= 80 && Tech_MFI[day-1,i+1] < 80)
      if((Tech_MFI[day,i+1] >= 80 && Tech_MFI[day-1,i+1] < 80) | News_signal[day,(i+1)*3+1] == 'EXIT')
      {
        trade[a,b] <- -Tech_MFI[day,i]
        HoldDay <- day-BuyDay
        trade[a+1,b+2] <- Principal <- Changes+Shares*Tech_MFI[day,i] # record the holding cash after sell
        trade[a,b+1] <- HoldDay  # holding period of this transaction
        
        a <- a+1
      }
      
      # 93% Stop-loss point
      #             if(Tech_MFI[day,i]< abs(trade[a-1,b]*0.93) && a%%2==0)
      #             {
      #               trade[a,b] <- -Tech_MFI[day,i]
      #               HoldDay <- day-BuyDay
      #               trade[a+1,b+2] <- Principal <- Changes+Shares*Tech_MFI[day,i] # record the holding cash after sell
      #               trade[a,b+1] <- HoldDay  # holding period of this transaction
      #               
      #               a <- a+1
      #             }     
      
    } 
    
  }
  # Delete not-round-trip trade
  if (trade[a-1,b]>0 && trade[a,b]==0)
  # if (a%%2==0 && day==(nrow(stock.close)-1))
  {
    trade[a-1,b]=0  # not-round-trip buy price
    trade[a-1,b+1]=0 # not-round-trip BuyDay
    trade[a-1,b+2]=0 # not-round-trip Principle
    trade[a,b+2]=0 # not-round-trip shares
  } 
  b <- b+3 
  c <- c+1
  
  #}
  
}
rm(a,b,c,i,Principal,BuyDay,Changes,HoldDay,Shares,day)



################################################## Performance analysis#################################################################
## Total Net Profit

# Initial the variable
MFI.GrossLoss <- MFI.GrossProfit <- MFI.LossNum <- MFI.WinNum <- MFI.EvenNum <- MFI.Max.Con.Win <- MFI.Max.Con.Loss <- 0
MFI.Largest.WinTrade <- MFI.Largest.LossTrade <- MFI.TradePeriod <- MFI.Total.TradingPeriod <- MFI.Max.SharesHeld <- 0
MFI.Max.Trade.Drawdown <- MFI.PnL.in.MaxTradeDrawdown <- MFI.Max.Drawdown <- 0

for ( stock in seq(3,ncol(trade),3) )
{
  n <- 1 # the No. of trades
  flag <-  0 # mark for trade statu to calculate the Max. Consecutive Winning or Losing trades.
  MFI.Max.Con.Loss.Counter <- MFI.Max.Con.Win.Counter <- 0 # initial the max consecutive counter
  
  # count the number of stocks that have trades
  if (trade[1,stock]!=0 && trade[1,stock-2]!=0 && trade[2,stock]!=0 && trade[2,stock-2]!=0)
  {
    MFI.Total.TradingPeriod <- MFI.Total.TradingPeriod + 1
  }
  
  # Find the maximum shares held
  if (trade[n+1,stock] > MFI.Max.SharesHeld)
  {
    MFI.Max.SharesHeld <- trade[n+1,stock]
  }
  
  while (trade[n,stock]!=0 && trade[n,stock-2]!=0 && trade[n+1,stock]!=0 && trade[n+1,stock-2]!=0) ## the principal = 0, which means there is no investment in this stock.
  {
    ## Profit and Loss Calculation ( 3 if()s )
    
    ## Losing Trade
    if (trade[n+2,stock] < trade[n,stock] ) # principal become less after this transaction
    {
      MFI.GrossLoss <- MFI.GrossLoss - abs(trade[n,stock-2]+trade[n+1,stock-2]) * trade[n+1,stock] # cumulate the Gross Loss
      MFI.LossNum <- MFI.LossNum + 1 # cumulate loss times
      
      # Calculate the Max. Consecutive Losing trades.
      if ( flag == 2 ) # flag=2 means it is a loss trade last time.
      {
        MFI.Max.Con.Loss.Counter <- MFI.Max.Con.Loss.Counter + 1
        if ( MFI.Max.Con.Loss.Counter > MFI.Max.Con.Loss) # Check whether it is the max consecutive times
        {
          MFI.Max.Con.Loss <- MFI.Max.Con.Loss.Counter
        }
        flag <- 2
      }
      if ( flag != 2) # flag!=2 means it is not a loss trade last time.
      {
        MFI.Max.Con.Loss.Counter <- 1
        flag <- 2
      }
      
      # Calculate the largest losing trade
      if((trade[n+2,stock]-trade[n,stock]) < MFI.Largest.LossTrade) #find the largest loss
      {
        MFI.Largest.LossTrade <- trade[n+2,stock]-trade[n,stock]
      }
      
    }
    
    ## Winning Trade
    if (trade[n+2,stock] > trade[n,stock] ) # principal become more after this transaction
    {
      MFI.GrossProfit <- MFI.GrossProfit + abs(trade[n,stock-2]+trade[n+1,stock-2]) * trade[n+1,stock] # cumulate the Gross Profit
      MFI.WinNum <- MFI.WinNum + 1 # cumulate win times
      
      # Calculate the Max. Consecutive Losing trades.
      if ( flag == 1 ) # flag=1 means it is a win trade last time.
      {
        MFI.Max.Con.Win.Counter <- MFI.Max.Con.Win.Counter + 1
        if ( MFI.Max.Con.Win.Counter > MFI.Max.Con.Win) # Check whether it is the max consecutive times
        {
          MFI.Max.Con.Win <- MFI.Max.Con.Win.Counter 
        }
        flag <- 1
      }
      if ( flag != 1) # flag!=1 means it is not a win trade last time.
      {
        MFI.Max.Con.Win.Counter <- 1
        flag <- 1
      }
      
      # Calculate the largest winning trade
      if((trade[n+2,stock]-trade[n,stock]) > MFI.Largest.WinTrade) #find the largest loss
      {
        MFI.Largest.WinTrade <- trade[n+2,stock]-trade[n,stock]
      }
      
    }
    
    ## Even Trade    
    if (trade[n+2,stock] == trade[n,stock] ) # principal didn't change after this transaction
    {
      MFI.EvenNum <- MFI.EvenNum +1 # cumulate even times
      flag <- 0 # initial the flag for max consecutive counter
    }
    
    # Calculate trading period
    MFI.TradePeriod <- MFI.TradePeriod + trade[n+1,stock-1]
    
    # Calculate Max. Trade Drawdown
    if (maxdrawdown(stock.close[trade[n,stock-1]:(trade[n,stock-1]+trade[n+1,stock-1]),stock/3])$maxdrawdown/stock.close[maxdrawdown(stock.close[trade[n,stock-1]:(trade[n,stock-1]+trade[n+1,stock-1]),stock/3])$from+trade[n,stock-1]-1,stock/3] > MFI.Max.Trade.Drawdown)
    {
      MFI.Max.Trade.Drawdown <- maxdrawdown(stock.close[trade[n,stock-1]:(trade[n,stock-1]+trade[n+1,stock-1]),stock/3])$maxdrawdown/stock.close[maxdrawdown(stock.close[trade[n,stock-1]:(trade[n,stock-1]+trade[n+1,stock-1]),stock/3])$from+trade[n,stock-1]-1,stock/3]
      # Calculate profit or loss in this trade
      MFI.PnL.in.MaxTradeDrawdown <- (trade[n+1,stock-2]+trade[n,stock-2])*trade[n+1,stock]
      # print(MFI.Max.Trade.Drawdown)
      # print(maxdrawdown(stock.close[trade[n,stock-1]:(trade[n,stock-1]+trade[n+1,stock-1]),stock/3])$maxdrawdown)
    }
    
    # Calculate Max.Drawdown peak to Valley
    if (n == 1 && maxdrawdown(stock.close[1:1258,stock/3])$maxdrawdown/stock.close[maxdrawdown(stock.close[1:1258,stock/3])$from,stock/3] > MFI.Max.Drawdown)
    {
      MFI.Max.Drawdown <- maxdrawdown(stock.close[1:1258,stock/3])$maxdrawdown/stock.close[maxdrawdown(stock.close[1:1258,stock/3])$from,stock/3]
      ## profit or loss in this stock calculate artificially
      #print(MFI.Max.Drawdown)
      # print(stock)
      # print(maxdrawdown(stock.close[1:1258,stock/3])$maxdrawdown)
      #trade[,stock]
    }
    
    n <- n+2 # step to next trade # every trade in trades[] has two line records
  }
  
}


## MFI.Total.TradeNum : Total Number of Trades
MFI.Total.TradeNum <- MFI.LossNum + MFI.WinNum + MFI.EvenNum
## MFI.Total.NetProfit
MFI.Total.NetProfit <- MFI.GrossProfit + MFI.GrossLoss
## MFI.Avg.NetProft : Avg. Trade Net Profit ($)
MFI.Avg.NetProfit <- MFI.Total.NetProfit / MFI.Total.TradeNum
## MFI.Avg.WinTrade : Avg. Profit of Winning Trade ($)
MFI.Avg.WinTrade <- MFI.GrossProfit / MFI.WinNum
## MFI.Avg.LossTrade : Avg. Loss of Losing Trade ($)
MFI.Avg.LossTrade <- MFI.GrossLoss / MFI.LossNum
## MFI.Ratio.Win2Loss : Ratio Avg. Win to Avg. Loss
MFI.Ratio.Win2Loss <- abs(MFI.Avg.WinTrade / MFI.Avg.LossTrade)
## MFI.Ratio.MatketTime : Ratio of Time in the Market
MFI.Ratio.MatketTime <- MFI.TradePeriod/(MFI.Total.TradingPeriod * 250 * 5.06) # !!!Change the duration of Backtest

######################################################## Trading Sample Plotting for NO.12 stock(MMM) ##################################################
# sample.close <- data.frame(time = seq(as.Date('2011-03-01'), by = 'days', length = 1258), stock.close[,12])
# sample.MVWAP <- data.frame(time = seq(as.Date('2011-03-01'), by = 'days', length = 1258), MovingVWAP[,12])
# sample.VWAP <- data.frame(time = seq(as.Date('2011-03-01'), by = 'days', length = 1258), VWAP[,12])


# plot(sample.close, type='l', xlab='time', ylab='Price', main='Buy/Sell Signal')
day.end <- 500
# Change the strategies name!!!!!
plot(stock.close[1:day.end,12], type='l', lwd=1, xlab='time(0,500)', ylab='Price', main='MFI backtest on MMM') # Plot the first 'day.end' days' price
# Calculate points for entry and exit.
point.buy <- point.sell <- matrix(data = NA, nrow = nrow(trade), ncol = 2)
n <- 1
while ( trade[2*n-1,34]!=0 && trade[2*n-1,35]!=0 && trade[n*2,34]!=0 && trade[n*2,35]!=0)
{
  point.buy[n,1] <- trade[2*n-1,34] # Buy price
  point.buy[n,2] <- trade[2*n-1,35] # Buy time
  # point.buy[n,2] <- sample.close[trades[2*n-1,35],1] # Buy time with ts
  
  point.sell[n,1] <- abs(trade[2*n,34]) # Sell price
  point.sell[n,2] <- trade[2*n-1,35] + trade[2*n,35]
  # point.sell[n,2] <- sample.close[trades[2*n-1,35] + trades[2*n,35], 1] # Sell time with ts
  n <- n+1
}

points(x = point.buy[1:day.end,2], y = point.buy[1:day.end,1], col='green' ) # Point the first 'day.end' days' entry as green
points(x = point.sell[1:day.end,2], y = point.sell[1:day.end,1], col='red') # Point the first 'day.end' days' exit as red

# change indicators for your strategies
#lines(Tech_MFI[1:day.end,24], col='orange') # Line the first 'day.end' days' MFI


legend(x = 'bottomright', inset = 0.05, legend = c('Price',  'Entry', 'Exit'), lty = c(1,NA,NA), col = c('black','green','red'), pch = c(NA,1,1) )

###################################Plot signal line############################################

plot(Tech_MFI[1:day.end,24], type='l', lwd=1, xlab='time(0,500)', ylab='MFI',col='orange' )
up <- matrix(80,nrow = day.end, ncol = 1)
down <- matrix(20,nrow = day.end, ncol = 1)
x.time <- matrix(c(1:day.end), nrow = day.end, ncol = 1)

lines(x = x.time[,1], y = up[,1] , col='red')
lines(x = x.time[,1], y = down[,1], col='green', lty = 5)
