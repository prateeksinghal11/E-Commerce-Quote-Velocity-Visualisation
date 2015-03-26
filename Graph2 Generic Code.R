#Graph 2. Overall Quote Velocity 
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



dataset1$Date <-format(as.Date(dataset1$Date, format="%m/%d/%Y"), "%Y-%m-%d")
dataset1$Date <-as.Date(dataset1$Date, format="%m/%d/%Y")
dataset1=sqldf("select * from dataset1 where Date !='null' order by date desc ")


dataset2=data.frame(MarketPlace=unique(tolower(dataset1$Marketplace)))
dataset3 = data.frame(StartDate = min(dataset1$Date),EndDate=max(dataset1$Date))


df1 = funcOverallQuotevelocity(dataset1,dataset2,dataset3,dataset4) # Function Call
#Testing input recreation ENDs
ggplot(df1, aes(y=(yAxis/10000000),x=xAxis)) +
  geom_bar(width=.6, position = position_dodge(width=0.7), stat = "identity",colour="#1E90FF",fill="#1E90FF") +
  geom_text(aes(label = paste(round(yAxis/10000000,2),"",sep=""),ymax=0),
            position=position_dodge(width=0.9), vjust=-0.25) + ylab("Crores")+xlab("") +scale_y_continuous(labels = comma) + ggtitle("Overall Quote velocity ") # Developed by : Prateek Singhal 




# Main Function
funcOverallQuotevelocity  <- function(dataset1,dataset2,dataset3,dataset4){
  
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

#******************************************************************** For Graph 2.
# Step 8. Input costData.csv
#pivotShippedRevenue

inputCostData = inputCostData

inputCostData$MarketPlace=tolower(inputCostData$MarketPlace)

costShipedRevenueMerged=sqldf("select * from inputCostData,pivotShippedRevenue where inputCostData.MarketPlace = pivotShippedRevenue.categorical")

# Step 9. Calculating "Market Place fee". and RealizedValue

costShipedRevenueMerged$MarketPlaceFee = costShipedRevenueMerged$FeesPercent*costShipedRevenueMerged$value*0.01
costShipedRevenueMerged$RealizedValue = costShipedRevenueMerged$value-costShipedRevenueMerged$MarketPlaceFee
realizedValueDf = costShipedRevenueMerged # realized value df
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
Merg1df=sqldf("select valueRealizedAfterPaymentdf.MarketPlace,RealizedValue,valueRealizedAfterPayment from valueRealizedAfterPaymentdf,realizedValueDf where trim(realizedValueDf.MarketPlace)=trim(valueRealizedAfterPaymentdf.Marketplace)")
#Merg1df
Merg2df= sqldf("select pivotOrderSale.categorical as MarketPlace,pivotOrderSale.value as OrderSales,pivotShippedRevenue.value as ShippedRevene from pivotOrderSale,pivotShippedRevenue where pivotShippedRevenue.categorical= pivotOrderSale.categorical")
#Merg2df
Merg3df= sqldf("select Merg1df.MarketPlace,RealizedValue,valueRealizedAfterPayment,OrderSales,ShippedRevene from Merg1df,Merg2df where Merg1df.Marketplace=Merg2df.MarketPlace")
#sum(Merg3df$OrderSales)
#sum(Merg3df$ShippedRevene)
#sum(Merg3df$RealizedValue)
#sum(Merg3df$valueRealizedAfterPayment)

df1= data.frame(xAxis=character(),yAxis=numeric())
df1 = rbind(df1, data.frame(xAxis="Order Sales",yAxis=sum(Merg3df$OrderSales)))
df1 = rbind(df1, data.frame(xAxis="Shipped Revenue",yAxis=sum(Merg3df$ShippedRevene)))
df1 = rbind(df1, data.frame(xAxis="Realized Value",yAxis=sum(Merg3df$RealizedValue)))
df1 = rbind(df1, data.frame(xAxis="Value Realized After Payment",yAxis=sum(Merg3df$valueRealizedAfterPayment)))
return(df1)
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
  df$categorical = tolower(df$categorical)
  pivot = sqldf("select sum(value) as value,categorical as categorical from df group by categorical")
  pivot = rbind(pivot,data.frame(categorical="(all)",value=sum(pivot$value)))
  # df <- melt(df, id=which(names(df)=="categorical"), measure = which(names(df)=="value"))
  #
  #  pivot = cast(df,categorical~variable,sum, margins=c("grand_row"))
  
  return(pivot)
}

#Function for Data Selection of input date Range
funcDispatchDateRangeSelection <- function(df,x,y)
{
  newdf <- df[df$DispatchDate >= x & df$DispatchDate <= y,]
  return(newdf)
}
