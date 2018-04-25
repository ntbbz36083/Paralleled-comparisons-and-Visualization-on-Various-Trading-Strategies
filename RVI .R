# Relative Vigor Index (RVI)

install.packages('TTR')
library(TTR)

############################### Calculate Relative Vigor Index for every stock for everyday################################################
RVI <- matrix(0,1258,476)
m <-1
for (stock in seq(1,ncol(stock.AllData),6))  #### seq(1,ncol(stock.AllData),6) 
{
  for (n in 1:nrow(stock.AllData))
  {
    if ((stock.AllData[n,stock+1]-stock.AllData[n,stock+2])!= 0 )
    {
      RVI[n,m] <- (stock.AllData[n,stock+3]-stock.AllData[n,stock])/(stock.AllData[n,stock+1]-stock.AllData[n,stock+2])
    }
    if ((stock.AllData[n,stock+1]-stock.AllData[n,stock+2]) == 0) # replace NaN values
    {
      RVI[n,m] <- 0.000000001
    }
  }
  m <- m+1
}
rm(stock,m,n)

##################################### Calculate 10-period SMA and 4-period VWMA of RVI######################################################
Tech_RVI <- matrix(0,nrow(RVI),ncol(RVI)*3)
s <- 1
for (stock in 1:476) #### 1:ncol(RVI)
{
  SMA.10 <- SMA(RVI[,stock], n=10)  # 10-period simple moving average
  VWMA.4 <- VWMA(RVI[,stock],stock.AllData[,(stock-1)*6+5], n=4)  # 4-period volume weighted moving average
  Tech_RVI[,s] <- stock.AllData[,(stock-1)*6+6]
  Tech_RVI[,s+1] <- SMA.10
  Tech_RVI[,s+2] <- VWMA.4
  s <- s+3
}
rm(stock,s,SMA.10,VWMA.4)


###################################################### RVI trading execution################################################################
trade_2 <- matrix(0,nrow(stock.AllData),ncol(stock.AllData)/6*3)    #### to store trade prices ## matrix(0,650,n*3=1428)
VWMA.4 <- matrix(0,nrow(stock.AllData),ncol(stock.AllData)/6)  ### for plot


# trade_2 <- matrix(data = NA, nrow = nrow(stock.AllData), ncol = (ncol(stock.AllData)/6*3) )
# colnames(trade_2) <- rep(c('buy & sell prices','Start & Hold days','principal & Shares'),476)
## Every trade have six records, as follow,
##  'buy & sell prices'   'Start & Hold days'   'principal & Shares'
##      +200                    135                     100000
##      -260                    24                        500

b <- c <- 1 #b: columes in trade_2

for (i in seq(1,ncol(stock.AllData)/6*3,3))   #### seq(1,n*3=1428,3)
{
  a <- 1 #the rows in trade_2
  Principal <- 1000000*Weights[1,c] 
  trade_2[a,b+2] <- Principal
  
  # if (Weights[1,(i+2)/3] != 0)
  # {
    for (day in 11:nrow(Tech_RVI))
    {
      
      ### VWMA.4 crosses SMA.10 from above, then buy
      if(Tech_RVI[day,i+1] > Tech_RVI[day,i+2] && Tech_RVI[day-1,i+1] < Tech_RVI[day-1,i+2] && a%%2==1)
      {
        trade_2[a,b] <- Tech_RVI[day,i]
        VWMA.4[a,(i+2)/3] <- Tech_RVI[day,i+2]### for plot
        BuyDay <- day 
        trade_2[a,b+1] <- BuyDay  # record the date of this buy transaction
        Shares <- FloorHundred(Principal/Tech_RVI[day,i]) # record shares of this buy transaction
        Changes <- Principal-FloorHundred(Principal/Tech_RVI[day,i])*Tech_RVI[day,i] # record the changes unused in trade
        trade_2[a+1,b+2] <- Shares 
        a <- a+1
      }
      
      ### VWMA.4 crosses SMA.10 from below, then sell
      if(a%%2==0)
      {
        if(Tech_RVI[day,i+1] < Tech_RVI[day,i+2] && Tech_RVI[day-1,i+1] > Tech_RVI[day-1,i+2])
        {
          trade_2[a,b] <- -Tech_RVI[day,i]
          VWMA.4[a,(i+2)/3] <- Tech_RVI[day,i+2]### for plot
          HoldDay <- day-BuyDay
          trade_2[a+1,b+2] <- Principal <- Changes+Shares*Tech_RVI[day,i] # record the holding cash after sell
          trade_2[a,b+1] <- HoldDay  # holding period of this transaction
          
          a <- a+1
          
          
        }
        
        ## 93% Stop-loss point
        # if (Tech_RVI[day,i] < abs(trade_2[a-1,b]*0.93 )
        #  {
        #   trade_2[a,b] <- -Tech_RVI[day,i]
        #   HoldDay <- day-BuyDay
        #   trade_2[a+1,b+2] <- Principal <- Changes+Shares*Tech_RVI[day,i] # record the holding cash after sell
        #   trade_2[a,b+1] <- HoldDay  # holding period of this transaction
        #   a <- a+1
        #   }
      }          
    } 
    
    # Delete not-round-trip trade
    if(trade_2[a-1,b]>0 && trade_2[a,b]==0)
    {
      trade_2[a-1,b]=0  # not-round-trip buy price
      trade_2[a-1,b+2]=0 # not-round-trip Principle
      trade_2[a,b+2]=0 # not-round-trip shares
    }    
    b <- b+3 
    c <- c+1
  # }
  
}
rm(a,b,BuyDay,c,day,HoldDay,i,Principal,Shares,Changes)

####################################################### Performance analysis##############################################################
## Total Net Profit

# Initial the variable
RVI.GrossLoss <- RVI.GrossProfit <- RVI.LossNum <- RVI.WinNum <- RVI.EvenNum <- RVI.Max.Con.Win <- RVI.Max.Con.Loss <- 0
RVI.Largest.WinTrade <- RVI.Largest.LossTrade <- RVI.TradePeriod <- RVI.Total.TradingPeriod <- RVI.Max.SharesHeld <- 0
RVI.Max.Trade.Drawdown <- RVI.PnL.in.MaxTradeDrawdown <- RVI.Max.Drawdown <- 0

for ( stock in seq(3,ncol(trade_2),3) )
{
  n <- 1 # the No. of trades
  flag <-  0 # mark for trade statu to calculate the Max. Consecutive Winning or Losing trades.
  RVI.Max.Con.Loss.Counter <- RVI.Max.Con.Win.Counter <- 0 # initial the max consecutive counter
  
  while (trade_2[n,stock]!=0 && trade_2[n,stock-2]!=0 && trade_2[n+1,stock]!=0 && trade_2[n+1,stock-2]!=0) ## the principal = 0, which means there is no investment in this stock.
  {
    ## Profit and Loss Calculation ( 3 if()s )
    
    ## Losing Trade
    if (trade_2[n+2,stock] < trade_2[n,stock] ) # principal become less after this transaction
    {
      RVI.GrossLoss <- RVI.GrossLoss - abs(trade_2[n,stock-2]+trade_2[n+1,stock-2]) * trade_2[n+1,stock] # cumulate the Gross Loss
      RVI.LossNum <- RVI.LossNum + 1 # cumulate loss times
      
      # Calculate the Max. Consecutive Losing trades.
      if ( flag == 2 ) # flag=2 means it is a loss trade last time.
      {
        RVI.Max.Con.Loss.Counter <- RVI.Max.Con.Loss.Counter + 1
        if ( RVI.Max.Con.Loss.Counter > RVI.Max.Con.Loss) # Check whether it is the max consecutive times
        {
          RVI.Max.Con.Loss <- RVI.Max.Con.Loss.Counter
        }
        flag <- 2
      }
      if ( flag != 2) # flag!=2 means it is not a loss trade last time.
      {
        RVI.Max.Con.Loss.Counter <- 1
        flag <- 2
      }
      
      # Calculate the largest losing trade
      if((trade_2[n+2,stock]-trade_2[n,stock]) < RVI.Largest.LossTrade) #find the largest loss
      {
        RVI.Largest.LossTrade <- trade_2[n+2,stock]-trade_2[n,stock]
      }
      
    }
    
    ## Winning Trade
    if (trade_2[n+2,stock] > trade_2[n,stock] ) # principal become more after this transaction
    {
      RVI.GrossProfit <- RVI.GrossProfit + abs(trade_2[n,stock-2]+trade_2[n+1,stock-2]) * trade_2[n+1,stock] # cumulate the Gross Profit
      RVI.WinNum <- RVI.WinNum + 1 # cumulate win times
      
      # Calculate the Max. Consecutive Losing trades.
      if ( flag == 1 ) # flag=1 means it is a win trade last time.
      {
        RVI.Max.Con.Win.Counter <- RVI.Max.Con.Win.Counter + 1
        if ( RVI.Max.Con.Win.Counter > RVI.Max.Con.Win) # Check whether it is the max consecutive times
        {
          RVI.Max.Con.Win <- RVI.Max.Con.Win.Counter 
        }
        flag <- 1
      }
      if ( flag != 1) # flag!=1 means it is not a win trade last time.
      {
        RVI.Max.Con.Win.Counter <- 1
        flag <- 1
      }
      
      # Calculate the largest winning trade
      if((trade_2[n+2,stock]-trade_2[n,stock]) > RVI.Largest.WinTrade) #find the largest loss
      {
        RVI.Largest.WinTrade <- trade_2[n+2,stock]-trade_2[n,stock]
      }
      
    }
    
    ## Even Trade    
    if (trade_2[n+2,stock] == trade_2[n,stock] ) # principal didn't change after this transaction
    {
      RVI.EvenNum <- RVI.EvenNum +1 # cumulate even times
      flag <- 0 # initial the flag for max consecutive counter
    }
    
    # Calculate trading period
    RVI.TradePeriod <- RVI.TradePeriod + trade_2[n+1,stock-1]
    
    # Calculate Max. Trade Drawdown
    if (maxdrawdown(stock.close[trade_2[n,stock-1]:(trade_2[n,stock-1]+trade_2[n+1,stock-1]),stock/3])$maxdrawdown/stock.close[maxdrawdown(stock.close[trade_2[n,stock-1]:(trade_2[n,stock-1]+trade_2[n+1,stock-1]),stock/3])$from+trade_2[n,stock-1]-1,stock/3] > RVI.Max.Trade.Drawdown)
    {
      RVI.Max.Trade.Drawdown <- maxdrawdown(stock.close[trade_2[n,stock-1]:(trade_2[n,stock-1]+trade_2[n+1,stock-1]),stock/3])$maxdrawdown/stock.close[maxdrawdown(stock.close[trade_2[n,stock-1]:(trade_2[n,stock-1]+trade_2[n+1,stock-1]),stock/3])$from+trade_2[n,stock-1]-1,stock/3]
      # Calculate profit or loss in this trade
      print(RVI.Max.Trade.Drawdown)
      print(maxdrawdown(stock.close[trade_2[n,stock-1]:(trade_2[n,stock-1]+trade_2[n+1,stock-1]),stock/3])$maxdrawdown)
      RVI.PnL.in.MaxTradeDrawdown <- (trade_2[n+1,stock-2]+trade_2[n,stock-2])*trade_2[n+1,stock]
    }
    
    # Calculate Max.Drawdown peak to Valley
    if (n == 1 && maxdrawdown(stock.close[1:1258,stock/3])$maxdrawdown/stock.close[maxdrawdown(stock.close[1:1258,stock/3])$from,stock/3] > RVI.Max.Drawdown)
    {
      RVI.Max.Drawdown <- maxdrawdown(stock.close[1:1258,stock/3])$maxdrawdown/stock.close[maxdrawdown(stock.close[1:1258,stock/3])$from,stock/3]
      ## profit or loss in this stock calculate artificially
      #print(RVI.Max.Drawdown)
      # print(stock)
      #trade[stock,]
      # print(maxdrawdown(stock.close[1:1258,stock/3])$maxdrawdown)
    }
    
    n <- n+2 # step to next trade # every trade in trades[] has two line records
    
  }
}

## RVI.Total.TradeNum : Total Number of Trades
RVI.Total.TradeNum <- RVI.LossNum + RVI.WinNum + RVI.EvenNum
## RVI.Total.NetProfit
RVI.Total.NetProfit <- RVI.GrossProfit + RVI.GrossLoss
## RVI.Avg.NetProft : Avg. Trade Net Profit ($)
RVI.Avg.NetProfit <- RVI.Total.NetProfit / RVI.Total.TradeNum
## RVI.Avg.WinTrade : Avg. Profit of Winning Trade ($)
RVI.Avg.WinTrade <- RVI.GrossProfit / RVI.WinNum
## RVI.Avg.LossTrade : Avg. Loss of Losing Trade ($)
RVI.Avg.LossTrade <- RVI.GrossLoss / RVI.LossNum
## RVI.Ratio.Win2Loss : Ratio Avg. Win to Avg. Loss
RVI.Ratio.Win2Loss <- abs(RVI.Avg.WinTrade / RVI.Avg.LossTrade)
## RVI.Ratio.MatketTime : Ratio of Time in the Market
RVI.Ratio.MatketTime <- RVI.TradePeriod/(RVI.Total.TradingPeriod * 250 * 5.084) # !!!Change the duration of Backtest

######################################################## Trading Sample Plotting for NO.12 stock(MMM) ##################################################
# sample.close <- data.frame(time = seq(as.Date('2011-03-01'), by = 'days', length = 1258), stock.close[,12])
# sample.MVWAP <- data.frame(time = seq(as.Date('2011-03-01'), by = 'days', length = 1258), MovingVWAP[,12])
# sample.VWAP <- data.frame(time = seq(as.Date('2011-03-01'), by = 'days', length = 1258), VWAP[,12])


# plot(sample.close, type='l', xlab='time', ylab='Price', main='Buy/Sell Signal')
day.end <- 500
# Change the strategies name!!!!!
plot(stock.close[1:day.end,12], type='l', lwd=1, xlab='time(0,500)', ylab='Price', main='RVI backtest on MMM') # Plot the first 'day.end' days' price
# Calculate points for entry and exit.
point.buy <- point.sell <- matrix(data = NA, nrow = nrow(trade_2), ncol = 2)
n <- 1
while ( trade_2[2*n-1,34]!=0 && trade_2[2*n-1,35]!=0 && trade_2[n*2,34]!=0 && trade_2[n*2,35]!=0)
{
  point.buy[n,1] <- trade_2[2*n-1,34] # Buy price
  point.buy[n,2] <- trade_2[2*n-1,35] # Buy time
  # point.buy[n,2] <- sample.close[trades[2*n-1,35],1] # Buy time with ts
  
  point.sell[n,1] <- abs(trade_2[2*n,34]) # Sell price
  point.sell[n,2] <- trade_2[2*n-1,35] + trade_2[2*n,35]
  # point.sell[n,2] <- sample.close[trades[2*n-1,35] + trades[2*n,35], 1] # Sell time with ts
  n <- n+1
}

points(x = point.buy[1:day.end,2], y = point.buy[1:day.end,1], col='green' ) # Point the first 'day.end' days' entry as green
points(x = point.sell[1:day.end,2], y = point.sell[1:day.end,1], col='red') # Point the first 'day.end' days' exit as red

# change indicators for your strategies
lines(Tech_RVI[1:day.end,35], col='blue') # Line the first 'day.end' days' SMA.10
lines(Tech_RVI[1:day.end,36], col='orange') # Line the first 'day.end' days' VWMA.4

legend(x = 'bottomright', inset = 0.05, legend = c('Price', 'SMA.10', 'VWMA.4', 'Entry', 'Exit'), lty = c(1,1,1,NA,NA), col = c('black','blue','orange','green','red'), pch = c(NA,NA,NA,1,1) )

###################################plot signal line######################################################################

plot(Tech_RVI[1:day.end,35], type='l', lwd=1, xlab='time(0,500)', ylab='RVI', col='blue')
lines(Tech_RVI[1:day.end,36], col='orange')

point.buy.s <- point.sell.s <- matrix(data = NA, nrow = nrow(trade_2), ncol = 2)
n <- 1
while ( trade_2[2*n-1,34]!=0 && trade_2[2*n-1,35]!=0 && trade_2[n*2,34]!=0 && trade_2[n*2,35]!=0)
{
  point.buy.s[n,1] <- VWMA.4[2*n-1,12] # VWMA.4 when buy
  point.buy.s[n,2] <- trade_2[2*n-1,35] # Buy time
  
  point.sell.s[n,1] <- VWMA.4[2*n,12] # VWMA.4 when sell
  point.sell.s[n,2] <- trade_2[2*n-1,35] + trade_2[2*n,35]
  
  n <- n+1
}
points(x = point.buy.s[1:day.end,2], y = point.buy.s[1:day.end,1], col='green' ) # Point the first 'day.end' days' entry as green
points(x = point.sell.s[1:day.end,2], y = point.sell.s[1:day.end,1], col='red') # Point the first 'day.end' days' exit as red





#############round-trip test###############
error <- matrix(0,650,476)
for (x in seq(1,1428,3))
{
  
  for (y in 1:nrow(trade_2))
  {
    z <-1
    if (trade_2[y,x] < 0 && trade_2[y+1,x] < 0)
    {
      error[,z] <- trade[,x]
      z <- z+1
    }
  }
}

## Personal Analysis
for (n in ncol(trade_2):1)
{
  if (trade_2[1,n]==0) 
  {
    trade_2 <- trade_2[,-n]
  }
}
for (n in seq(ncol(trade_2),3,-3))
{
  trade_2 <- trade_2[,c(-n+1,-n+2)]
}
result <- matrix(0,nrow = 3,ncol = ncol(trade_2))
for (n in 1:ncol(trade_2))
{
  result[1,n] <- trade_2[1,n]
  for (line in 1:nrow(trade_2)-2) 
  {
    if (trade_2[line,n] != 0 && trade_2[line+1,n]==0 && trade_2[line+2,n]==0) 
    {
      result[2,n] <- trade_2[line,n]
    }
  }
  result[3,n] <- result[2,n]- result[1,n]
}


