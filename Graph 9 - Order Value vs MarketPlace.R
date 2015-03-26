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

funcOrderValue(dataset1,dataset2,dataset3,dataset4) # Function Call
#Testing input recreation ENDs










# Main Function
funcOrderValue <- function(dataset1,dataset2,dataset3,dataset4){
  
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
  
 df1 = pivotOrderSale
  ggplot(df1, aes(y=value/10000000,x=reorder(categorical,-value))) +
    geom_bar(width=.6, position = position_dodge(width=0.7), stat = "identity",colour="#1E90FF",fill="#1E90FF") +
    geom_text(aes(label = paste(round(value/10000000,2),"",sep=""),ymax=0), position=position_dodge(width=0.9), vjust=-0.25) + ylab("Crores")+xlab("") +scale_y_continuous(labels = comma) + ggtitle("") # Developed by : Prateek Singhal 
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
