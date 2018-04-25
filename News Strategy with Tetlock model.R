#Author: John Li
########################################## Read Stock Data ##################################################################################
# Reset Work Directory
getwd()
setwd("F:/Google Drive/Study/code/")

## Reading Stock Data from Python output ##
stock.AllData=read.csv("StockData.csv",header=TRUE)[-1,]
colnames(stock.AllData)[1] <- 'Date'

## delete redundant stocks
stock.AllData <- Filter(function(x)!any(is.na(x)), stock.AllData)

## create a ticker list
ticker_list <- c()
i=476
for (n in seq(ncol(stock.AllData),7,-6) )
{
  stock.AllData[,c(n,n-1,n-3)] <- NA
  stock.AllData[,c(n-1,n-4)] <- 0
  ticker <- gsub("\\..*","",colnames(stock.AllData)[n])
  ticker_list[i]=ticker
  i=i-1
  colnames(stock.AllData)[c(n,n-1,n-3,n-4)] <- c(ticker,'News #','Trade_time','Effect')
}
rm(n,ticker, i)

## create a time list
time_list <- levels(stock.AllData[,1])
time_list <- time_list[-length(time_list)]
for (n in 1:length(time_list))
{
  time_list[n] <- strsplit(time_list[n], ' ')[[1]][1]
}
rm(n)

#################################################### Read News data ###########################################################
# Reset Work Directory
getwd()
setwd("F:/Google Drive/Study/FE 800 Final Project/News_data/")

# read News data # need to change manually for every year's data.
News_data=read.csv("News_2011_easyuse.csv",header=FALSE)[,c(-3,-4,-5)] #2011
# News_data=read.csv("News_2012_easyuse.csv",header=FALSE)[,c(-3,-4,-5)] #2012
# News_data=read.csv("News_2013_easyuse.csv",header=FALSE)[,c(-3,-4,-5)] #2013
# News_data=read.csv("News_2014_easyuse.csv",header=FALSE)[,c(-3,-4,-5)] #2014

colnames(News_data) <- c('Time', 'Ticker', 'Effect', 'Trade Time')

# to calculate the processing time
start_time <- proc.time()

## generate indicators from the News data for trading
for (n in 1:nrow(News_data))
{
  print(n)
  # get the time of a piece of News
  time <- toString(News_data[n,1])
  # get the date of the piece of News
  day <- strsplit(time, ' ')[[1]][1]
  # get the hour of the piece of News happened at that date
  hour <- as.numeric(strsplit(strsplit(time, ' ')[[1]][2], ':')[[1]][1])
  # get the minute of the piece of News happened at that date
  minute <- as.numeric(strsplit(strsplit(time, ' ')[[1]][2], ':')[[1]][2])
  # get the index of timestamp in the stock.AllData
  ix_time <- match(day, time_list)
  # get the index of ticker in the stock.AllData 
  ix_ticker <- match(News_data[n,2], ticker_list)
  
  if (!is.na(ix_time) && !is.na(ix_ticker) ) # If it's not NA, which means stock.AllData could find the date the News happened.
  {
    # the counter of News happened in the date
    stock.AllData[ix_time,6*ix_ticker] = stock.AllData[ix_time,6*ix_ticker] +1
    
    
    # identify the News happened in trade time or not.
    # If it's in trade time (9:30-15:59), display "Y";
    # If it's before trade time (0:00-9:29), display "B_N";
    # If it's after trade time (16:00-23:59), display "A_N".
    if ( ( hour < 9 ) | ( hour==9 && minute<30 ) ) ###### "B_N"
    {
      # record the signal of trade time 
      stock.AllData[ix_time,(6*ix_ticker-2)] <- 'B_N'
      # record the effect
      stock.AllData[ix_time,(6*ix_ticker-3)] <- stock.AllData[ix_time,(6*ix_ticker-3)] + News_data[n,'Effect']
      stock.AllData[ix_time+1,(6*ix_ticker-3)] <- stock.AllData[ix_time+1,(6*ix_ticker-3)] + News_data[n,'Effect']
      stock.AllData[ix_time+1,(6*ix_ticker-3)] <- stock.AllData[ix_time+1,(6*ix_ticker-3)] + News_data[n,'Effect']*0.5
      
    } else if ( ( hour > 9 && hour < 16 ) | (hour==9 && minute >=30 ) ) ###### Y
    {
      # record the signal of trade time 
      stock.AllData[ix_time,(6*ix_ticker-2)] <- 'Y'
      # record the effect
      stock.AllData[ix_time,(6*ix_ticker-3)] <- stock.AllData[ix_time,(6*ix_ticker-3)] + News_data[n,'Effect']
      stock.AllData[ix_time+1,(6*ix_ticker-3)] <- stock.AllData[ix_time+1,(6*ix_ticker-3)] + News_data[n,'Effect']
      stock.AllData[ix_time+1,(6*ix_ticker-3)] <- stock.AllData[ix_time+1,(6*ix_ticker-3)] + News_data[n,'Effect']*0.5
      
    } else if ( ( hour >= 16) ) ###### A_N
    {
      # record the signal of trade time 
      stock.AllData[ix_time,(6*ix_ticker-2)] <- 'A_N'
      # record the effect at the next day, since the News happened after trading time, which would affect the second day's price.
      stock.AllData[ix_time+1,(6*ix_ticker-3)] <- stock.AllData[ix_time+1,(6*ix_ticker-3)] + News_data[n,'Effect']
      stock.AllData[ix_time+2,(6*ix_ticker-3)] <- stock.AllData[ix_time+2,(6*ix_ticker-3)] + News_data[n,'Effect']
      stock.AllData[ix_time+2,(6*ix_ticker-3)] <- stock.AllData[ix_time+2,(6*ix_ticker-3)] + News_data[n,'Effect']*0.5
    }
    
  }# end of if statment for whether the effect of News should be recorded.
}# end of for to read every piece of news.

#display the processing time
print(proc.time()-start_time)
# sound after process
# install.packages("beepr")
library(beepr)
beep()

rm(time,start_time,day,hour,ix_ticker,ix_time,minute, n, News_data)

# write.csv(stock.AllData, file = "News Trading analytics.csv")

################################################# Generating & analyze signals of trading #################################################################
rm(ticker_list, time_list)
signal_total <- signal_correct <- 0


# Define a function to check effect, and generate signal ###################################3
generate_signal <- function(date, effect, Data.table)
{
  if ( Data.table[date,effect] >0 )
  {
    return('ENTRY')
  } else if ( Data.table[date, effect] <0 )
  {
    return('EXIT')
  } else {
    return(NA)
  }
}# end of function defining.


#Check signals day by day
for( day in 1:nrow(stock.AllData))
{
  #check signals stock by stock
  for ( stock in seq(7,ncol(stock.AllData),6) )
  {
    # Generate signal of ENTRY
    # There are  kinds of situation:
    # 1.  B_N & not the first day in record : check the yesterday's close and that date's open, && generate a signal in that day;
    # 2.  Y : check the open and close in that date, && generate a signal in that day;
    # 3.  A_N & not the last day in record : check the date's close and the next day's open, && generate a signal in the next day.
    if ( !is.na(stock.AllData[day, stock-3]) )
    {
      if ( (stock.AllData[day,stock-3]=='B_N' && day!=1) | (stock.AllData[day,stock-3]=='Y') ) 
      { #### situation No.1 and No.2
        signal_total <- signal_total +1 # counter
        signal <- stock.AllData[day,stock] <- generate_signal(day, stock-4, stock.AllData) # could trade in that day
        rm(signal)
        
      } else if( stock.AllData[day,stock-3]=='A_N' && day!=nrow(stock.AllData) ) 
      { #### situation No.3
        signal_total <- signal_total +1 # counter
        signal <- stock.AllData[day+1,stock] <- generate_signal(day+1, stock-4, stock.AllData) # could trade in the next day
        rm(signal)
      } # end of if statement of generating signals
    } # end of if statement of !is.na
    
  }# end of for statement stock by stock
}# end of for statement day by day

# sound after process
# install.packages("beepr")
library(beepr)
beep()

write.csv(stock.AllData, file = "News Trading signals.csv", na = 'NO')

################################### Indicator users part #####################################################
# Reset Work Directory
getwd()
setwd("F:/Google Drive/Study/FE 800 Final Project/News_data/")

# Read the date already processed
News_signal = read.csv("News Trading signals.csv",header=TRUE)[,-1]

# stock.AllData <- News_signal


