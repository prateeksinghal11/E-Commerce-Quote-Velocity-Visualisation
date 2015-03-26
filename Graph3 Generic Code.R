#Graph 3. Day Wise Revenue Realization
#Package Decleration Section
install.packages("reshape")
install.packages("sqldf")
install.packages("ggplot2")
install.packages("scales")
library(ggplot2)
library(reshape)
library(sqldf)
library(scales)


#Data Input Section
# Map 1-based optional input ports to variables
#**********************************************************************Sample Code
dataset1 <- maml.mapInputPort(1) # class: data.frame 
dataset2 <- maml.mapInputPort(2) # class: data.frame
#**********************************************************************Sample Code
dataset1 = ? # Input 1. Pass the dataset containing actual input data(Eg. syberplace sample data.csv )
dataset2 = ? # Input 2. Pass the dataset containing the MarketPlace column defining all the marketplaces to be analyed(Eg. inputMarketPlace <- data.frame(MarketPlace=c("svh","snapdeal")) )
dataset3 = ? # Input 3. Start & End Date Entry (Eg. data.frame(StartDate="2014-10-01",EndDate="2014-10-01") )
dataset4 = ? # Input 4. Pass the dataset containinf the percentages of the marketplace (inputCostData <- read.csv("CostData.csv"))


#This section is for testing purpose of input section
dataset1 =  read.csv("syberplace sample data.csv") # Input 1. Pass the dataset containing actual input data(Eg. syberplace sample data.csv )
dataset2 = data.frame(MarketPlace=c("svh","snapdeal","ebay","flipkart","rediff","shopclues","naaptol","paytm","syberplace","mysmartprice","icubes","indiatimes","junglee","buyhatke"))  # Input 2. Pass the dataset containing the MarketPlace column defining all the marketplaces to be analyed(Eg. inputMarketPlace <- data.frame(MarketPlace=c("svh","snapdeal")) )
dataset3 = data.frame(StartDate="2014-10-01",EndDate="2014-11-30") # Input 3. Start & End Date Entry (Eg. data.frame(StartDate="2014-10-01",EndDate="2014-10-01") )
dataset4 = read.csv("CostData.csv") # Input 4. Pass the dataset containinf the percentages of the marketplace (inputCostData <- read.csv("CostData.csv"))

# Testing for 2 Market Places
dataset2 = data.frame(MarketPlace=c("svh","snapdeal"))  # Input 2. Pass the dataset containing the MarketPlace column defining all the marketplaces to be analyed(Eg. inputMarketPlace <- data.frame(MarketPlace=c("svh","snapdeal")) )
funcDayWiseRevenuerealization(dataset1,dataset2,dataset3,dataset4) # Function Call
#Testing input recreation ENDs





# Main Function
funcDayWiseRevenuerealization   <- function(dataset1,dataset2,dataset3,dataset4){
  
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
  selectedDataOnOrderDate$value <- selectedDataOnOrderDate$SellingPrice
  pivotOrderSale<-funcPivotSum(selectedDataOnOrderDate)
  pivotOrderSale # *********************************************************************************Order Sales Pivot
  
  
  # Step 6. Shipped Revenue Calculations - Select shipped data with specified inputs dates
  #nrow(selectedDataOnOrderDate) # process selectedDataOnOrderDate
  selectedDataOnDispatchDate=""
  selectedDataOnDispatchDate <- sqldf("select * from selectedDataOnOrderDate where DispatchDate not in('RTO','Cancelled') and length(trim(DispatchDate))!=0")
  #nrow(selectedDataOnDispatchDate)
  
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
  calc4=sqldf("select sum(selectedDataOnDispatchDateRange.SellingPrice*selectedDataOnDispatchDateRange.ShippingDelay) as revlossduetoshippedDelay from selectedDataOnDispatchDateRange where selectedDataOnDispatchDateRange.ShippingDelay > 1 ") * 0.24/365
  calc4$revlossduetoshippedDelay
 
  
  # Calc 5. Per Day New Revenue Flow Afer Market Fee
  
  
    #******************************************************************** For Graph 2.- Reuesed in Graph 3 Calc. (Calc 5)
    # Step 8. Input costData.csv
    #pivotShippedRevenue
  
    inputCostData = inputCostData
  
    inputCostData$MarketPlace=tolower(inputCostData$MarketPlace)
  
    costShipedRevenueMerged=sqldf("select * from inputCostData,pivotShippedRevenue where inputCostData.MarketPlace = pivotShippedRevenue.categorical")
  
      # Step 9. Calculating "Market Place fee". and RealizedValue
  
    costShipedRevenueMerged$MarketPlaceFee = costShipedRevenueMerged$FeesPercent*costShipedRevenueMerged$value*0.01
    calc5_PerDayReveneueFlowAfterMarketFee=calc3_perDayShippedReveneueWithoutDelay - ( sum(costShipedRevenueMerged$MarketPlaceFee))/ numberOfDays
    calc5_PerDayReveneueFlowAfterMarketFee
  
  #costShipedRevenueMerged$RealizedValue = costShipedRevenueMerged$value-costShipedRevenueMerged$MarketPlaceFee
  #costShipedRevenueMerged
  #realizedValueDf = costShipedRevenueMerged # realized value df
  #selectedDataOnDispatchDateRange$Marketplace
  #sum(realizedValueDf$RealizedValue)
  
  #Step 10. Calculating Realized revenue after payment delay
  #head(selectedDataOnDispatchDateRange$RealizedPrice,40)
  #nrow(selectedDataOnDispatchDateRange)
  
  #head(selectedDataOnDispatchDateRange$RealizedPrice/selectedDataOnDispatchDateRange$ShippingDelay,100)
  #nrow(dfsd)
  #selectedDataOnDispatchDateRange
  selectedDataOnDispatchDateRange$Marketplace = tolower(selectedDataOnDispatchDateRange$Marketplace)
  valueRealizedAfterPaymentdf=sqldf("select sum(RealizedPrice/PaymentDelay) as valueRealizedAfterPayment,Marketplace from selectedDataOnDispatchDateRange  where PaymentDelay>0 group by Marketplace")
  #dfsd=sqldf("select RealizedPrice/PaymentDelay  from selectedDataOnDispatchDateRange  where PaymentDelay>0")
  
  #Step 11.
  
  #realizedValueDf$MarketPlace
 # Merg1df=sqldf("select valueRealizedAfterPaymentdf.MarketPlace,RealizedValue,valueRealizedAfterPayment from valueRealizedAfterPaymentdf,realizedValueDf where trim(realizedValueDf.MarketPlace)=trim(valueRealizedAfterPaymentdf.Marketplace)")
  #Merg1df
  Merg2df= sqldf("select pivotOrderSale.categorical as MarketPlace,pivotOrderSale.value as OrderSales,pivotShippedRevenue.value as ShippedRevene from pivotOrderSale,pivotShippedRevenue where pivotShippedRevenue.categorical= pivotOrderSale.categorical and pivotShippedRevenue.categorical!='(all)'")
  #Merg2df
  #Merg3df= sqldf("select Merg1df.MarketPlace,RealizedValue,valueRealizedAfterPayment,OrderSales,ShippedRevene from Merg1df,Merg2df where Merg1df.Marketplace=Merg2df.MarketPlace")
  
  #sum(Merg2df$OrderSales)
  #sum(Merg2df$ShippedRevene)
  #sum(Merg3df$RealizedValue)
  #sum(Merg3df$valueRealizedAfterPayment)
  #sum(Merg3df$OrderSales)
  
  #numberOfDays
  #calc3_perDayShippedReveneueWithoutDelay
  df1=""
  df1= data.frame(xAxis=character(),yAxis=numeric())
  df1 = rbind(df1, data.frame(xAxis="Order / Day",yAxis=sum(Merg2df$OrderSales)/numberOfDays)) # Calc 1. OrderSales/Day
  df1 = rbind(df1, data.frame(xAxis="Shipped Revenue / Day",yAxis=sum(Merg2df$ShippedRevene)/numberOfDays)) # Calc 2. Shipped Revenue / Day
  df1 = rbind(df1, data.frame(xAxis="Per Day Shipped Reveneue Without Delay",yAxis=calc3_perDayShippedReveneueWithoutDelay)) # Calc 3.
  df1 = rbind(df1, data.frame(xAxis="Revenue loss due to shipped Delay",yAxis=calc4$revlossduetoshippedDelay)) # Calc 4.
  df1 = rbind(df1, data.frame(xAxis="Per day Net revenue flow after market fee",yAxis=calc5_PerDayReveneueFlowAfterMarketFee)) # Calc 5.
              
  #df1 = rbind(df1, data.frame(xAxis="Realized Value",yAxis=sum(Merg3df$RealizedValue)))
  df1 = rbind(df1, data.frame(xAxis="Per Day Realized Reveneue After Payment Delay",yAxis=sum(valueRealizedAfterPaymentdf$valueRealizedAfterPayment)/numberOfDays)) #Calc 6. Per Day Realized Reveneue After Payment Delay
  
  ggplot(df1, aes(y=(yAxis/100000),x=xAxis)) +
    geom_bar(width=.6, position = position_dodge(width=0.7), stat = "identity",colour="#1E90FF",fill="#1E90FF") +
    geom_text(aes(label = paste(round(yAxis/100000,2),"",sep=""),ymax=0),
              position=position_dodge(width=0.9), vjust=-0.25) + ylab("Lacs")+xlab("") +scale_y_continuous(labels = comma) + ggtitle("Day Wise Revenue realization") # Developed by : Prateek Singhal 

}

# Contents of optional Zip port are in ./src/
# source("src/yourfile.R");
# load("src/yourData.rdata");




# Sample operation
data.set = rbind(dataset1, dataset2);

# You'll see this output in the R Device port.
# It'll have your stdout, stderr and PNG graphics device(s).
plot(data.set);

# Select data.frame to be sent to the output Dataset port
maml.mapOutputPort("data.set");







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



 