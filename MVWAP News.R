#Author: John Li
library(TTR)
###################################################  VWAP & MVWAP Calculation ###############################################################
## VWAP, MVWAP
VWAP <- MovingVWAP <- matrix(data = NA, nrow = nrow(stock.AllData), ncol = (ncol(stock.AllData)/6) )
for (stock in seq(6,ncol(stock.AllData),6) ) 
{
  VWAP[,(stock/6)] <- VWAP(stock.AllData[,stock], stock.AllData[,stock-1], n=10)
}
for (stock in 1:ncol(VWAP) ) 
{
  for (day in 19:nrow(VWAP)) 
  {
    MovingVWAP[day,stock] <- mean(VWAP[(day-9):day, stock])
  }
}
rm(day, stock)
#############################################   MVWAP & VWAP Trade Execution ######################################################################
## buy as price crosses above VWAP/MVWAP
## sell as price crosses below VWAP/MVWAP With a 93% Stop-loss point
## MVWAP may be used by longer term traders, but VWAP only looks at ... responsive to market moves for short-term trades and strategies or it ... 
## trades is the output of every transaction.

trades <- matrix(data = 0, nrow = nrow(stock.AllData), ncol = (ncol(stock.AllData)/6*3) )
colnames(trades) <- rep(c('buy & sell prices','Start & Hold days','principal & Shares'),476)
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
    
    ## start to inpect every day price of a stock
    for (day in 19:(nrow(stock.close)-1) )
    {
      
      if ( flag%%2==1 ) 
      {
        ## buy as price crosses above VWAP/MVWAP
        if ( ( stock.close[day,stock] > VWAP[day,stock] | stock.close[day,stock] > MovingVWAP[day,stock] )
             && ( stock.close[day+1,stock] < VWAP[day+1,stock] | stock.close[day+1,stock] < MovingVWAP[day+1,stock] ) 
             | ( News_signal[day+1,stock*6+1]=='ENTRY' ) ) 
        {
          
          trades[n,stock*3-2] <- stock.close[day+1,stock] # record price of this buy
          trades[n+1,stock*3] <- FloorHundred((trades[n,stock*3]/stock.close[day+1,stock]) ) # record shares of this buy transaction
          changes <- trades[n,stock*3] - stock.close[day+1,stock] * trades[n+1,stock*3] # record the changes unused in trade
          trades[n,stock*3-1] <- day+1 # record the date of this buy transaction
          HoldDay <- day  # After but trasaction, begin to calculate holding days.
          n <- n+1
          flag <- flag + 1 # Change flag for sell
          #print(flag)
        }
      }
      
      if (flag%%2==0 ) { # In holding status
        
        ## sell as price crosses below VWAP/MVWAP
        if ( ( stock.close[day,stock] < VWAP[day,stock] | stock.close[day,stock] < MovingVWAP[day,stock] )
             && ( stock.close[day+1,stock] > VWAP[day+1,stock] | stock.close[day+1,stock] > MovingVWAP[day+1,stock] ) )
             # | ( News_signal[day+1,stock*6+1]=='EXIT' ) ) 
        {
          
          trades[n,stock*3-2] <- -(stock.close[day+1,stock]) # record the price of this sell
          trades[n,stock*3-1] <- day+1 - HoldDay # record the holding day
          trades[n+1,stock*3] <- stock.close[day+1,stock] * trades[n,stock*3] + changes # record the holding cash after sell
          n <- n+1
          flag <- flag+1
          
        }
#         ## 95% Stop-loss point
#         if ( stock.close[day,stock] < abs(trades[n-1,stock*3-2]*0.95) && flag%%2==0 ) 
#         {
#         
#             trades[n,stock*3-2] <- -(stock.close[day+1,stock]) # record the price of this sell
#             trades[n,stock*3-1] <- day - HoldDay # record the holding day
#             trades[n+1,stock*3] <- stock.close[day+1,stock] * trades[n,stock*3] + changes # record the holding cash after sell
#             n <- n+1
#             flag <- flag+1
#         
#         }
      }
      
      # delete non-round-trip trades
      if (flag%%2==0 && day==(nrow(stock.close)-1) )
      {
        # delete records of buy price, buy shares
        trades[n-1,stock*3-2] <- trades[n,stock*3] <- trades[n-1,stock*3-1] <- 0
      }
    }
  }
}
rm(changes,day,flag,HoldDay,n,stock)
# rm(MovingVWAP,VWAP)


########################################################  Performance of MVWAP & VWAP  #############################################################################################

##  Total Net Profit

# Initial the variable
MVWAP.GrossLoss <- MVWAP.GrossProfit <- MVWAP.LossNum <- MVWAP.WinNum <- MVWAP.EvenNum <- MVWAP.Max.Con.Win <- MVWAP.Max.Con.Loss <- 0
MVWAP.Largest.WinTrade <- MVWAP.Largest.LossTrade <- MVWAP.TradePeriod <- MVWAP.TradePeriod <- 0

for ( stock in seq(3,ncol(trades),3) )
{
  n <- 1 # the No. of trades
  flag <-  0 # mark for trade statu to calculate the Max. Consecutive Winning or Losing trades.
  MVWAP.Max.Con.Loss.Counter <- MVWAP.Max.Con.Win.Counter <- 0 # initial the max consecutive counter
  
  while (trades[n,stock]!=0 && trades[n,stock-2]!=0 && trades[n+1,stock]!=0 && trades[n+1,stock-2]!=0) ## the principal = 0, which means there is no investment in this stock.
  {
    ## Profit and Loss Calculation ( 3 if()s )
    
    ## Losing Trade
    if (trades[n+2,stock] < trades[n,stock] ) # principal become less after this transaction
    {
      MVWAP.GrossLoss <- MVWAP.GrossLoss - abs(trades[n,stock-2]+trades[n+1,stock-2]) * trades[n+1,stock] # cumulate the Gross Loss
      MVWAP.LossNum <- MVWAP.LossNum + 1 # cumulate loss times
      
      # Calculate the Max. Consecutive Losing trades.
      if ( flag == 2 ) # flag=2 means it is a loss trade last time.
      {
        MVWAP.Max.Con.Loss.Counter <- MVWAP.Max.Con.Loss.Counter + 1
        if ( MVWAP.Max.Con.Loss.Counter > MVWAP.Max.Con.Loss) # Check whether it is the max consecutive times
        {
          MVWAP.Max.Con.Loss <- MVWAP.Max.Con.Loss.Counter
        }
        flag <- 2
      }
      if ( flag != 2) # flag!=2 means it is not a loss trade last time.
      {
        MVWAP.Max.Con.Loss.Counter <- 1
        flag <- 2
      }
      
      # Calculate the largest losing trade
      if((trades[n+2,stock]-trades[n,stock]) < MVWAP.Largest.LossTrade) #find the largest loss
      {
        MVWAP.Largest.LossTrade <- trades[n+2,stock]-trades[n,stock]
      }
      
    }
    
    ## Winning Trade
    if (trades[n+2,stock] > trades[n,stock] ) # principal become more after this transaction
    {
      MVWAP.GrossProfit <- MVWAP.GrossProfit + abs(trades[n,stock-2]+trades[n+1,stock-2]) * trades[n+1,stock] # cumulate the Gross Profit
      MVWAP.WinNum <- MVWAP.WinNum + 1 # cumulate win times
      
      # Calculate the Max. Consecutive Losing trades.
      if ( flag == 1 ) # flag=1 means it is a win trade last time.
      {
        MVWAP.Max.Con.Win.Counter <- MVWAP.Max.Con.Win.Counter + 1
        if ( MVWAP.Max.Con.Win.Counter > MVWAP.Max.Con.Win) # Check whether it is the max consecutive times
        {
          MVWAP.Max.Con.Win <- MVWAP.Max.Con.Win.Counter 
        }
        flag <- 1
      }
      if ( flag != 1) # flag!=1 means it is not a win trade last time.
      {
        MVWAP.Max.Con.Win.Counter <- 1
        flag <- 1
      }
      
      # Calculate the largest winning trade
      if((trades[n+2,stock]-trades[n,stock]) > MVWAP.Largest.WinTrade) #find the largest loss
      {
        MVWAP.Largest.WinTrade <- trades[n+2,stock]-trades[n,stock]
      }
    }
    
    ## Even Trade    
    if (trades[n+2,stock] == trades[n,stock] ) # principal didn't change after this transaction
    {
      MVWAP.EvenNum <- MVWAP.EvenNum +1 # cumulate even times
      flag <- 0 # initial the flag for max consecutive counter
    }
    
    # Calculate trading period
    MVWAP.TradePeriod <- MVWAP.TradePeriod + trades[n+1,stock-1]
    
    n <- n+2 # step to next trade # every trade in trades[] has two line records
    
  }
}
rm(MVWAP.Max.Con.Win.Counter,MVWAP.Max.Con.Loss.Counter)

## MVWAP.Total.TradeNum : Total Number of Trades
MVWAP.Total.TradeNum <- MVWAP.LossNum + MVWAP.WinNum + MVWAP.EvenNum
## MVWAP.Total.NetProfit
MVWAP.Total.NetProfit <- MVWAP.GrossProfit + MVWAP.GrossLoss
## MVWAP.Avg.NetProft : Avg. Trade Net Profit ($)
MVWAP.Avg.NetProfit <- MVWAP.Total.NetProfit / MVWAP.Total.TradeNum
## MVWAP.Avg.WinTrade : Avg. Profit of Winning Trade ($)
MVWAP.Avg.WinTrade <- MVWAP.GrossProfit / MVWAP.WinNum
## MVWAP.Avg.LossTrade : Avg. Loss of Losing Trade ($)
MVWAP.Avg.LossTrade <- MVWAP.GrossLoss / MVWAP.LossNum
## MVWAP.Ratio.Win2Loss : Ratio Avg. Win to Avg. Loss
MVWAP.Ratio.Win2Loss <- abs(MVWAP.Avg.WinTrade / MVWAP.Avg.LossTrade)



library(tseries)
## Max Drawdown
MaxDD_total <- matrix(0, nrow = 7, ncol = 476)
# row#1 the absolute of MDD , day Num of start and end points,
rownames(MaxDD_total) <- c('weight','ABS(MDD)', 'MDD %', 'start_day', 'end_day','start_price','end_price')
MaxDD_total['weight',] <- Weights[1,]
## the Max % of Max Drawdown
MaxDD_total_percent <- MaxDD_total_abs <- 0

for ( stock in 1:ncol(stock.close))
{
  MaxDD_total['ABS(MDD)', stock] <- maxdrawdown(stock.close[,stock])$maxdrawdown[1]
  MaxDD_total['start_day', stock] <- maxdrawdown(stock.close[,stock])$from[1]
  MaxDD_total['end_day', stock] <- maxdrawdown(stock.close[,stock])$to[1]
  MaxDD_total['start_price', stock] <- stock.close[ MaxDD_total['start_day', stock],stock ]
  MaxDD_total['end_price', stock] <- stock.close[ MaxDD_total['end_day', stock],stock ]
  MaxDD_total['MDD %', stock] <- ( MaxDD_total['start_price',stock] - MaxDD_total['end_price',stock] ) / ( MaxDD_total['start_price', stock])
  
  if ( MaxDD_total['weight',stock]!=0 && (MaxDD_total['start_price',stock]-MaxDD_total['end_price',stock])>MaxDD_total_abs )
  {
    MaxDD_total_abs <- MaxDD_total['start_price',stock]-MaxDD_total['end_price',stock]
    MaxDD_total_percent <- MaxDD_total_abs / MaxDD_total['start_price',stock]
    # locate the No. of stock with the MDD
    stock_No._MDD <- stock
  }
}
rm(stock)

stock <- stock_No._MDD
# Net Profit as % of the Max Drawdown
  # locate the No. of stock with the MDD
# stock <- match(max(MaxDD_total['ABS(MDD)',]), MaxDD_total['ABS(MDD)',] )
stock <- stock*3 # change dimension index for the table of trades.
## Every trade have six records, as follow,
##  'buy & sell prices'   'Start & Hold days'   'principal & Shares'
##      +200                    135                     100000
##      -260                    24                        500

# only process the stock with MDD
MDD_net_profit <- n <- 1
while (trades[n,stock-1]>=MaxDD_total['start_day',stock/3] && trades[n,stock-1]<=MaxDD_total['end_day',stock/3]
       && trades[n,stock-2]!=0 && trades[n+1,stock]!=0 && trades[n+1,stock-2]!=0 )
{
  MDD_net_profit <- MDD_net_profit + trades[n+2,stock] - trades[n,stock]
  
  n <- n+2
}

rm(stock2, stock)

# MDD trade
MDD_trade <- MDD_trade_percent <- MDD_trade_profit <- 0
for (stock in 1: ncol(stock.close))
{
  if (Weights[1,stock] != 0 )
  {
    n=1
    while (trades[n,stock*3-1]!=0 && trades[n,stock*3-2]!=0 && trades[n+1,stock*3]!=0 && trades[n+1,stock*3-2]!=0 )
    {
      start_day <- trades[n,stock*3-1]
      end_day <- trades[n,stock*3-1] + trades[n+1,stock*3-1]
      
      if ( end_day < 1259)
      {
        MDD_trade_temp_abs <- maxdrawdown( stock.close[start_day : end_day , stock] )
        if ( MDD_trade_temp_abs$maxdrawdown[1] > MDD_trade)
        {
          MDD_trade <- MDD_trade_temp_abs$maxdrawdown[1]
          MDD_trade_percent <- abs( trades[n,stock*3-2] + trades[n+1,stock*3-2] ) / max(trades[n,stock*3-2], abs(trades[n+1,stock*3-2]) )
          MDD_trade_profit <- trades[n+2,stock*3] - trades[n,stock*3]
        }
      }
      n=n+2
    }
  }
}

rm(MDD_trade_temp_abs,start_day, end_day)

##########################################################  Perfomance Analysis  ###################################################################
## Personal Analysis
for (n in ncol(trades):1)
{
  if (trades[1,n]==0) 
  {
    trades <- trades[,-n]
  }
}
for (n in seq(ncol(trades),3,-3))
{
  trades <- trades[,c(-n+1,-n+2)]
}
result <- matrix(0,nrow = 3,ncol = ncol(trades))
for (n in 1:ncol(trades))
{
  result[1,n] <- trades[1,n]
  for (line in 1:nrow(trades)-2) 
  {
    if (trades[line,n] != 0 && trades[line+1,n]==0 && trades[line+2,n]==0) 
    {
      result[2,n] <- trades[line,n]
    }
  }
  
  result[3,n] <- result[2,n]- result[1,n]
}

######################################################## Trading Sample Plotting for NO.12 stock(MMM) ##################################################
# sample.close <- data.frame(time = seq(as.Date('2011-03-01'), by = 'days', length = 1258), stock.close[,12])
# sample.MVWAP <- data.frame(time = seq(as.Date('2011-03-01'), by = 'days', length = 1258), MovingVWAP[,12])
# sample.VWAP <- data.frame(time = seq(as.Date('2011-03-01'), by = 'days', length = 1258), VWAP[,12])


# plot(sample.close, type='l', xlab='time', ylab='Price', main='Buy/Sell Signal')
day.end <- 1258
stock.No <- 305
# Change the strategies name!!!!!
plot(stock.close[1:day.end,stock.No], type='l', lwd=1, xlab='time(0,500)', ylab='Price', main='MVWAP/VWAP backtest on sample stock') # Plot the first 'day.end' days' price
# Calculate points for entry and exit.
point.buy <- point.sell <- matrix(data = NA, nrow = nrow(trades), ncol = 2)
n <- 1
while ( trades[2*n-1,stock.No*3-2]!=0 && trades[2*n-1,stock.No*3-1]!=0 && trades[n*2,stock.No*3-2]!=0 && trades[n*2,stock.No*3-1]!=0)
{
  point.buy[n,1] <- trades[2*n-1,stock.No*3-2] # Buy price
  point.buy[n,2] <- trades[2*n-1,stock.No*3-1] # Buy time
  # point.buy[n,2] <- sample.close[trades[2*n-1,35],1] # Buy time with ts
  
  point.sell[n,1] <- abs(trades[2*n,stock.No*3-2]) # Sell price
  point.sell[n,2] <- trades[2*n-1,stock.No*3-1] + trades[2*n,stock.No*3-1]
  # point.sell[n,2] <- sample.close[trades[2*n-1,35] + trades[2*n,35], 1] # Sell time with ts
  n <- n+1
}

points(x = point.buy[1:day.end,2], y = point.buy[1:day.end,1], col='green' ) # Point the first 'day.end' days' entry as green
points(x = point.sell[1:day.end,2], y = point.sell[1:day.end,1], col='red') # Point the first 'day.end' days' exit as red

# change indicators for your strategies
lines(VWAP[1:day.end,stock.No], col='blue') # Line the first 'day.end' days' VWAP
lines(MovingVWAP[1:day.end,stock.No], col='orange') # Line the first 'day.end' days' MVWAP

legend(x = 'topright', inset = 0.05, legend = c('Price', 'VWAP', 'MVWAP', 'Entry', 'Exit'), lty = c(1,1,1,NA,NA), col = c('black','blue','orange','green','red'), pch = c(NA,NA,NA,1,1) )

########################################################  Additional Funciton  ###########################################################################3
##  Floor numbers at Hundreds : e.g. FloorHundred(123.12) -> 100 
FloorHundred <- function(data){
  data <- floor(data/100) * 100
  return(data)
}

beep()