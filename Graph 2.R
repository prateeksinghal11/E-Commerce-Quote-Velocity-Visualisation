#Graph 2. Overall Quote Velocity               # Developed by : Prateek Singhal 
install.packages("reshape")
install.packages("sqldf")
install.packages("ggplot2")
install.packages("scales")
library(ggplot2)
library(reshape)
library(sqldf)
library(scales)
# Step 1.  Reading CSV file
setwd("C:\\Users\\Prateek\\SkyDrive\\Actual Analytics\\SyberPlace\\Task 1 Dashboard\\RCode")
inputMarketPlace <- data.frame(MarketPlace=c("svh","snapdeal"))
#dat
inputData <- read.csv("syberplace sample data.csv")
inputData$Marketplace = tolower(inputData$Marketplace)
unique(inputData$Marketplace)
inputData = sqldf("select * from inputData where MarketPlace in(select inputMarketPlace.MarketPlace as mp from inputMarketPlace)") 
sqldf("select dat.MarketPlace as mp from dat")
nrow(inputData)
inputData$Marketplace
# Step 2. Input Date Range for analysis
startDate <- "2014-10-01"    #input 1
endDate <- "2014-11-30"     #input 2


# Step 3. Process Order Date into R format and Select data from the defined Order Date Range
inputData$Date <- as.Date(inputData$Date, format="%m/%d/%Y")
selectedDataOnOrderDate=""
selectedDataOnOrderDate <- funcOrderDateRangeSelection(inputData,startDate,endDate)
#Function for Data Selection of input date Range
#*******************************************************************************************************************************
funcOrderDateRangeSelection <- function(df,x,y)
{
  newdf <- df[df$Date >= x & df$Date <= y,]
  return(newdf)
}


# Step 4. Process MarketPlace into SVH & Snap Deals
###########selectedDataOnOrderDate <- funcDataPrepMarketPlaceRename(selectedDataOnOrderDate) 
#Function for Data Prepration of input data
#*******************************************************************************************************************************
funcDataPrepMarketPlaceRename <- function(sampleFile)#df$categorical should contain the value of category to be pivoted and df$value should contain value to be pivotd
{
  sampleFile$Marketplace <- as.character(sampleFile$Marketplace)
  sampleFile$Marketplace[sampleFile$Marketplace == "SVH"] = "SVH & Snap Deals"
  sampleFile$Marketplace[sampleFile$Marketplace == "Snapdeal"] = "SVH & Snap Deals"
  sampleFile$Marketplace[sampleFile$Marketplace == "(all)"] = "Total"
  return(sampleFile)
}

# Step 5. Order Sales Pivot

selectedDataOnOrderDate <- sqldf("select * from selectedDataOnOrderDate where SellingPrice >=0")

nrow(selectedDataOnOrderDate)
selectedDataOnOrderDate$categorical <- selectedDataOnOrderDate$Marketplace # Assign Category 
selectedDataOnOrderDate$value <- selectedDataOnOrderDate$SellingPrice
pivotOrderSale<-funcPivotSum(selectedDataOnOrderDate)
pivotOrderSale # *********************************************************************************Order Sales Pivot
#Function to Pivot(SUM) dataframe 
#*******************************************************************************************************************************
funcPivotSum <- function(df)#df$categorical should contain the value of category to be pivoted and df$value should contain value to be pivotd
{
  df <- melt(df, id=which(names(df)=="categorical"), measure = which(names(df)=="value"))
  df$categorical = tolower(df$categorical)
  pivot = cast(df,categorical~variable,sum, margins=c("grand_row"))
  return(pivot)
}

# Step 6. Shipped Revenue Calculations - Select shipped data with specified inputs dates
nrow(selectedDataOnOrderDate) # process selectedDataOnOrderDate
selectedDataOnDispatchDate=""
selectedDataOnDispatchDate <- sqldf("select * from selectedDataOnOrderDate where DispatchDate not in('RTO','Cancelled') and length(trim(DispatchDate))!=0")
nrow(selectedDataOnDispatchDate)

selectedDataOnDispatchDate$DispatchDate <- as.Date(selectedDataOnDispatchDate$DispatchDate, format="%m/%d/%Y")
#inputData$D <- as.Date(inputData$Date, format="%m/%d/%Y")
selectedDataOnDispatchDateRange <- funcDispatchDateRangeSelection(selectedDataOnDispatchDate,startDate,endDate)
selectedDataOnDispatchDateRange # selectedDataOnDispatchDateRange contains the data set for processing of shipped revenue

#Function for Data Selection of input date Range
#*******************************************************************************************************************************
funcDispatchDateRangeSelection <- function(df,x,y)
{
  newdf <- df[df$DispatchDate >= x & df$DispatchDate <= y,]
  return(newdf)
}

# Step 7. Shipped Revenue Pivot
selectedDataOnDispatchDateRange <- sqldf("select * from selectedDataOnDispatchDateRange where SellingPrice >=0")
nrow(selectedDataOnDispatchDateRange)
selectedDataOnDispatchDateRange$categorical <- selectedDataOnDispatchDateRange$Marketplace # Assign Category 
selectedDataOnDispatchDateRange$value <- selectedDataOnDispatchDateRange$SellingPrice
pivotShippedRevenue<-funcPivotSum(selectedDataOnDispatchDateRange)
pivotShippedRevenue # *********************************************************************************shipped revenue Pivot

#******************************************************************** For Graph 2.
# Step 8. Input costData.csv
pivotShippedRevenue

inputCostData <- read.csv("CostData.csv")

inputCostData$MarketPlace=tolower(inputCostData$MarketPlace)

costShipedRevenueMerged=sqldf("select * from inputCostData,pivotShippedRevenue where inputCostData.MarketPlace = pivotShippedRevenue.categorical")

# Step 9. Calculating "Market Place fee". and RealizedValue

costShipedRevenueMerged$MarketPlaceFee = costShipedRevenueMerged$FeesPercent*costShipedRevenueMerged$value*0.01
costShipedRevenueMerged$RealizedValue = costShipedRevenueMerged$value-costShipedRevenueMerged$MarketPlaceFee
realizedValueDf = costShipedRevenueMerged # realized value df
selectedDataOnDispatchDateRange$Marketplace
sum(realizedValueDf$RealizedValue)

#Step 10. Calculating Realized revenue after payment delay
#head(selectedDataOnDispatchDateRange$RealizedPrice,40)
#nrow(selectedDataOnDispatchDateRange)

#head(selectedDataOnDispatchDateRange$RealizedPrice/selectedDataOnDispatchDateRange$ShippingDelay,100)
nrow(dfsd)
selectedDataOnDispatchDateRange
selectedDataOnDispatchDateRange$Marketplace = tolower(selectedDataOnDispatchDateRange$Marketplace)
valueRealizedAfterPaymentdf=sqldf("select sum(RealizedPrice/PaymentDelay) as valueRealizedAfterPayment,Marketplace from selectedDataOnDispatchDateRange  where PaymentDelay>0 group by Marketplace")
#dfsd=sqldf("select RealizedPrice/PaymentDelay  from selectedDataOnDispatchDateRange  where PaymentDelay>0")

#Step 11.

#realizedValueDf$MarketPlace
Merg1df=sqldf("select valueRealizedAfterPaymentdf.MarketPlace,RealizedValue,valueRealizedAfterPayment from valueRealizedAfterPaymentdf,realizedValueDf where trim(realizedValueDf.MarketPlace)=trim(valueRealizedAfterPaymentdf.Marketplace)")
Merg1df
Merg2df= sqldf("select pivotOrderSale.categorical as MarketPlace,pivotOrderSale.value as OrderSales,pivotShippedRevenue.value as ShippedRevene from pivotOrderSale,pivotShippedRevenue where pivotShippedRevenue.categorical= pivotOrderSale.categorical")
Merg2df
Merg3df= sqldf("select Merg1df.MarketPlace,RealizedValue,valueRealizedAfterPayment,OrderSales,ShippedRevene from Merg1df,Merg2df where Merg1df.Marketplace=Merg2df.MarketPlace")
sum(Merg3df$OrderSales)
sum(Merg3df$ShippedRevene)
sum(Merg3df$RealizedValue)
sum(Merg3df$valueRealizedAfterPayment)

df1= data.frame(xAxis=character(),yAxis=numeric())
df1 = rbind(df1, data.frame(xAxis="Order Sales",yAxis=sum(Merg3df$OrderSales)))
df1 = rbind(df1, data.frame(xAxis="Shipped Revenue",yAxis=sum(Merg3df$ShippedRevene)))
df1 = rbind(df1, data.frame(xAxis="Realized Value",yAxis=sum(Merg3df$RealizedValue)))
df1 = rbind(df1, data.frame(xAxis="Value Realized After Payment",yAxis=sum(Merg3df$valueRealizedAfterPayment)))

ggplot(df1, aes(y=(yAxis/10000000),x=xAxis)) +
  geom_bar(width=.6, position = position_dodge(width=0.7), stat = "identity") +
  geom_text(aes(label = paste(round(yAxis/10000000,2),"",sep=""),ymax=0),
            position=position_dodge(width=0.9), vjust=-0.25) + ylab("Crores")+xlab("") +scale_y_continuous(labels = comma) + ggtitle("Overall Quote velocity ") # Developed by : Prateek Singhal 
