                                                #Graph 1. Orders sales revenue Vs Shipped revenue               # Developed by : Prateek Singhal 
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
inputData <- read.csv("syberplace sample data.csv")


# Step 2. Input Date Range for analysis
#startDate <- "2014-10-01"    #input 1
#endDate <- "2014-11-30"     #input 2


# Step 3. Process Order Date into R format and Select data from the defined Order Date Range
inputData$Date <- as.Date(inputData$Date, format="%m/%d/%Y")

endDate=max(inputData$Date)
startDate=min(inputData$Date)
selectedDataOnOrderDate <- funcOrderDateRangeSelection(inputData,startDate,endDate)
nrow(selectedDataOnOrderDate)
        #Function for Data Selection of input date Range
#*******************************************************************************************************************************
funcOrderDateRangeSelection <- function(df,x,y)
{
  newdf <- df[df$Date >= x & df$Date <= y,]
  return(newdf)
}


# Step 4. Process MarketPlace into SVH & Snap Deals
selectedDataOnOrderDate <- funcDataPrepMarketPlaceRename(selectedDataOnOrderDate)
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


# Step 8. DataFrame prepared for plotting graph by merging order and shipped revenue
pivotOrderSale$Group <- "Order Sales"
pivotShippedRevenue$Group <-"Shipped Revenue"
pivotGraph1 <- rbind(pivotOrderSale,pivotShippedRevenue)

pivotGraph1 = pivotGraph1[order(pivotGraph1$value, decreasing=TRUE), ]
pivotGraph1
# Step 9.  Plot Graph 

ggplot(pivotGraph1, aes(y=(value/10000000),x=reorder(categorical,-value), fill = Group)) +
  geom_bar(width=.6, position = position_dodge(width=0.7), stat = "identity") +
  geom_text(aes(label = paste(round(value/10000000,2),"",sep=""),ymax=0),
            position=position_dodge(width=0.9), vjust=-0.25) + ylab("Revenue in Crores")+xlab("Market Place") +scale_y_continuous(labels = comma) + ggtitle("Orders sales revenue Vs Shipped revenue") # Developed by : Prateek Singhal 
