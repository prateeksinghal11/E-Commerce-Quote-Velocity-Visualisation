#Graph 6. Revenue Comparision
#Package Decleration Section
install.packages("reshape")
install.packages("sqldf")
install.packages("ggplot2")
install.packages("scales")
install.packages("plotrix")

library(ggplot2)
library(reshape)
library(sqldf)
library(scales)
require("plotrix")
library(ggplot2)
library(gridExtra)
library(reshape2)
setwd("C:\\Users\\Prateek\\SkyDrive\\Actual Analytics\\SyberPlace\\Task 1 Dashboard\\RCode")

dataset1 =  read.csv("syberplace sample data.csv") # Input 1. Pass the dataset containing actual input data(Eg. syberplace sample data.csv )
dataset2 = data.frame(MarketPlace=c("svh","snapdeal","ebay","flipkart","rediff","shopclues","naaptol","paytm","syberplace","mysmartprice","icubes","indiatimes","junglee","buyhatke"))  # Input 2. Pass the dataset containing the MarketPlace column defining all the marketplaces to be analyed(Eg. inputMarketPlace <- data.frame(MarketPlace=c("svh","snapdeal")) )
dataset3 = data.frame(StartDate="2014-10-01",EndDate="2014-11-30") # Input 3. Start & End Date Entry (Eg. data.frame(StartDate="2014-10-01",EndDate="2014-10-01") )
dataset4 = read.csv("CostData.csv") # Input 4. Pass the dataset containinf the percentages of the marketplace (inputCostData <- read.csv("CostData.csv"))

# Testing for 2 Market Places
funcDelayPaymentInDaysDf(dataset1,dataset2,dataset3,dataset4)


funcDelayPaymentInDaysDf   <- function(dataset1,dataset2,dataset3,dataset4)
{
  coin=0
  #count.fields(dataset2)
  #unique(dataset2$MarketPlace)
  graphdf= data.frame(MarketPlace = character(),DaysShippedDelay =numeric())
  
  
  TDaysShippedDelay = 0
  TMarketPlace=as.character("Svh & Snapdeal")
  
  for( i in dataset2$MarketPlace)
  { 
    dataset02 = data.frame(MarketPlace=i)
    
    output  = funcReturnDelayPaymentInDaysDf(dataset1,dataset02,dataset3,dataset4)
    if(i=="svh")
    {
      
      #TDaysShippedDelay = TDaysShippedDelay+ output
      
    }
    else if(i=="snapdeal")
    {
      #dfTemp$MarketPlace="Svh & Snapdeal"
      #TDaysShippedDelay = TDaysShippedDelay+ output
      
    }
    else
    {
      
      graphdf=  rbind(graphdf, data.frame(MarketPlace = i,DaysShippedDelay=output))
      
    }
    
    #print(output)
    coin = coin+1
  }
  #TMarketPlace
  #TCancellations
  dataset002 = data.frame(MarketPlace=c("svh","snapdeal"))
                          output02  = funcReturnDelayPaymentInDaysDf(dataset1,dataset002,dataset3,dataset4)  
                          graphdf=  rbind(graphdf, data.frame(MarketPlace = TMarketPlace,DaysShippedDelay=output02))
                          
                          
                          #coin
                          #graphdf
                          output01  = funcReturnDelayPaymentInDaysDf(dataset1,dataset2,dataset3,dataset4)
                          #output01
                          graphdf=  rbind(graphdf, data.frame(MarketPlace = "Total",DaysShippedDelay=output01))
                          
                          #graphdf
                          
                          
                        pl =  ggplot(graphdf, aes(y=(DaysShippedDelay),x=MarketPlace)) +
                            geom_bar(width=.6, position = position_dodge(width=0.7), stat = "identity") +
                            geom_text(aes(label = paste(round(DaysShippedDelay,2),"",sep=""),ymax=0),
                                      position=position_dodge(width=0.9), vjust=-0.25) + ylab("")+xlab("Market Place") +scale_y_continuous(labels = comma) + ggtitle(" Invoice Delays in Days")
                          
  return(pl)
}
#data_table


#****************************************************************************************
funcReturnDelayPaymentInDaysDf   <- function(dataset1,dataset2,dataset3,dataset4){
  
  inputData = dataset1 
  inputMarketPlace = dataset2
  dateSelection = dataset3
  inputCostData = dataset4
  
  
  inputData$Marketplace = tolower(inputData$Marketplace)
  inputData = sqldf("select * from inputData where MarketPlace in(select inputMarketPlace.MarketPlace as mp from inputMarketPlace)") 
  #nrow(inputData)
  #inputData$Marketplace
  # Step 2. Input Date Range for analysis
  startDate <-as.character(dateSelection$StartDate)
  endDate <- as.character(dateSelection$EndDate)  
  
  # Step 3. Process Order Date into R format and Select data from the defined Order Date Range
  inputData$Date <- as.Date(inputData$Date, format="%m/%d/%Y")
  selectedDataOnOrderDate=""
  selectedDataOnOrderDate <- funcOrderDateRangeSelection(inputData,startDate,endDate)
  
  numberOfDays =as.numeric( as.Date(endDate) - as.Date(startDate)) +1  
  #numberOfDays
  
  # Step 4. NA.
  # Step 5. Order Sales Pivot
  
  selectedDataOnOrderDate <- sqldf("select * from selectedDataOnOrderDate where SellingPrice >=0")
  
  #nrow(selectedDataOnOrderDate)
  selectedDataOnOrderDate$categorical <- selectedDataOnOrderDate$Marketplace # Assign Category 
  selectedDataOnOrderDate$ <- selectedDataOnOrderDate$SellingPrice
  pivotOrderSale<-funcPivotSum(selectedDataOnOrderDate)
  pivotOrderSale # *********************************************************************************Order Sales Pivot
  
  
  # Step 6. Shipped Revenue Calculations - Select shipped data with specified inputs dates
  #nrow(selectedDataOnOrderDate) # process selectedDataOnOrderDate
  selectedDataOnDispatchDate=""
  selectedDataOnDispatchDate <- sqldf("select * from selectedDataOnOrderDate where DispatchDate not in('RTO','Cancelled') and length(trim(DispatchDate))!=0")
  #nrow(selectedDataOnDispatchDate)
  G4_Calc1_Cancelled = sqldf("select * from selectedDataOnOrderDate where DispatchDate ='Cancelled' and length(trim(DispatchDate))!=0")
  sum(G4_Calc1_Cancelled$SellingPrice)
  G4_Calc2_RTO = sqldf("select * from selectedDataOnOrderDate where DispatchDate ='RTO' and length(trim(DispatchDate))!=0")
  sum(G4_Calc2_RTO$SellingPrice)
  G4_Calc3_NULL = sqldf("select sum(SellingPrice) as SumSellingPrice, MarketPlace from selectedDataOnOrderDate where length(trim(DispatchDate))<3 and sellingprice>0 group by MarketPlace")
  #sum(as.numeric(G4_Calc3_NULL$SumSellingPrice))
  #as.numeric(G4_Calc3_NULL$SumSellingPrice)
  # G4 Calc4_Delivered After Max Date
  selectedDataOnDispatchDate$DispatchDate <- as.Date(selectedDataOnDispatchDate$DispatchDate, format="%m/%d/%Y")
  endDate = as.Date(endDate)
  LateDelivery = selectedDataOnDispatchDate[selectedDataOnDispatchDate$DispatchDate > endDate,]
  #LateDelivery = selectedDataOnDispatchDate[selectedDataOnDispatchDate$DispatchDate > endDate,]
  # G4_Calc4_LateDelivery= sqldf("select sum(sellingPrice) as lateDelivery from LateDelivery where sellingprice > 0")
  G4_Calc4_LateDelivery= sqldf("select sellingPrice as lateDelivery from LateDelivery where sellingprice > 0")
  #sum(G4_Calc4_LateDelivery$lateDelivery)
  G4_Calc4_LateDelivery = sum( as.numeric(G4_Calc4_LateDelivery$lateDelivery))
  
  
  
  # G4 
  
  
  selectedDataOnDispatchDate$DispatchDate <- as.Date(selectedDataOnDispatchDate$DispatchDate, format="%m/%d/%Y")
  #inputData$D <- as.Date(inputData$Date, format="%m/%d/%Y")
  selectedDataOnDispatchDateRange <- funcDispatchDateRangeSelection(selectedDataOnDispatchDate,startDate,endDate)
  #selectedDataOnDispatchDateRange # selectedDataOnDispatchDateRange contains the data set for processing of shipped revenue
  
  # Step 7. Shipped Revenue Pivot
  selectedDataOnDispatchDateRange <- sqldf("select * from selectedDataOnDispatchDateRange where SellingPrice >=0")
  #nrow(selectedDataOnDispatchDateRange)
  selectedDataOnDispatchDateRange$categorical <- selectedDataOnDispatchDateRange$Marketplace # Assign Category 
  selectedDataOnDispatchDateRange$value <- selectedDataOnDispatchDateRange$SellingPrice
  pivotShippedRevenue<-funcPivotSum(selectedDataOnDispatchDateRange)
  pivotShippedRevenue # *********************************************************************************shipped revenue Pivot
  #*******************************************************************For Graph 3. 
  head(selectedDataOnDispatchDateRange)
  # Calc 3. Per day shipped revenue without delay
  calc3_1 = sqldf("select selectedDataOnDispatchDateRange.SellingPrice,selectedDataOnDispatchDateRange.ShippingDelay from selectedDataOnDispatchDateRange where selectedDataOnDispatchDateRange.ShippingDelay>0")
  calc3_1$ShippingDelayRevenue = calc3_1$SellingPrice/calc3_1$ShippingDelay
  calc3_1$ShippingDelayRevenue #Part 1 calc 3
  calc3_2 = sqldf("select selectedDataOnDispatchDateRange.SellingPrice,selectedDataOnDispatchDateRange.ShippingDelay from selectedDataOnDispatchDateRange where selectedDataOnDispatchDateRange.ShippingDelay=0")
  calc3_2$ShippingDelayRevenue = calc3_2$SellingPrice 
  calc3_2$ShippingDelayRevenue #Part 2 calc 3
  
  calc3_perDayShippedReveneueWithoutDelay  = (sum(calc3_1$ShippingDelayRevenue) +  sum(calc3_2$ShippingDelayRevenue))/numberOfDays
  calc3_perDayShippedReveneueWithoutDelay
  
  
  
  # Calc 4. Revenue Loss Due to Shipping Delay
  #calc4=sqldf("select sum(selectedDataOnDispatchDateRange.SellingPrice*selectedDataOnDispatchDateRange.ShippingDelay) as revlossduetoshippedDelay from selectedDataOnDispatchDateRange where selectedDataOnDispatchDateRange.ShippingDelay > 1 ") * 0.24/365
  #calc4$revlossduetoshippedDelay
  
  
  # Calc 5. Per Day New Revenue Flow Afer Market Fee
  
  
  #******************************************************************** For Graph 2.- Reuesed in Graph 3 Calc. (Calc 5)
  # Step 8. Input costData.csv
  #pivotShippedRevenue
  
  inputCostData = inputCostData
  
  inputCostData$MarketPlace=tolower(inputCostData$MarketPlace)
  
  costShipedRevenueMerged=sqldf("select * from inputCostData,pivotShippedRevenue where inputCostData.MarketPlace = pivotShippedRevenue.categorical")
  
  # Step 9. Calculating "Market Place fee". and RealizedValue
  
  costShipedRevenueMerged$MarketPlaceFee = costShipedRevenueMerged$FeesPercent*costShipedRevenueMerged$value*0.01
  # G4 Calc 5 MarketFee
  G4_Calc5_MarketFee = sum(costShipedRevenueMerged$MarketPlaceFee)
  G4_Calc5_MarketFee
  calc5_PerDayReveneueFlowAfterMarketFee=calc3_perDayShippedReveneueWithoutDelay - ( sum(costShipedRevenueMerged$MarketPlaceFee))/ numberOfDays
  calc5_PerDayReveneueFlowAfterMarketFee
  
  costShipedRevenueMerged$RealizedValue = costShipedRevenueMerged$value-costShipedRevenueMerged$MarketPlaceFee
  costShipedRevenueMerged
  realizedValueDf = costShipedRevenueMerged # realized value df
  selectedDataOnDispatchDateRange$Marketplace
  
  #G4 Calc 6 Payment Delay Revenue Loss
  EffectivePaymentDelayonRealizedPrice= sqldf("select sum(RealizedPrice*paymentdelay) as EffPaymentDelayonRealizedPrice, marketplace from selectedDataOnDispatchDateRange where PaymentDelay>0 group by marketplace")
  #selectedDataOnDispatchDateRange$EffectivePaymentDelayonRealizedPrice = selectedDataOnDispatchDateRange$RealizedPrice*selectedDataOnDispatchDateRange$PaymentDelay
  #head(selectedDataOnDispatchDateRange$EffectivePaymentDelayonRealizedPrice)
  DelayPaymentInDays=sum(EffectivePaymentDelayonRealizedPrice$EffPaymentDelayonRealizedPrice)/sum(realizedValueDf$RealizedValue)
  DelayPaymentInDays
  return(DelayPaymentInDays)
  
}





multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}



# ************************************************Supporting Functions**************************************
#Function to Select data between defined dates
funcOrderDateRangeSelection <- function(df,x,y)
{
  newdf <- df[df$Date >= x & df$Date <= y,]
  return(newdf)
} 

#Function to Pivot(SUM) dataframe 
funcPivotSum <- function(df)#df$categorical should contain the value of category to be pivoted and df$value should contain value to be pivotd
{
  df <- melt(df, id=which(names(df)=="categorical"), measure = which(names(df)=="value"))
  df$categorical = tolower(df$categorical)
  pivot = cast(df,categorical~variable,sum, margins=c("grand_row"))
  return(pivot)
}

#Function for Data Selection of input date Range
funcDispatchDateRangeSelection <- function(df,x,y)
{
  newdf <- df[df$DispatchDate >= x & df$DispatchDate <= y,]
  return(newdf)
}
