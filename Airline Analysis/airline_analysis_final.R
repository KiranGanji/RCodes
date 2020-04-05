## Libraries
library(data.table)
library(lubridate)
library(stringr)
library(MASS)
library(fitdistrplus)
library(ggplot2)

## Setting the work directory
rm(list = ls())
setwd("F:\\MBA\\IIMK\\1_Term\\Personal\\Articles\\dot-airline-on-time-performance-statistics")


## Main analysis
## Data sourced from : https://www.transtats.bts.gov/DL_SelectFields.asp?Table_ID=236

# Read the data
aircarriers <- fread("AirCarriers.csv")
aircarriers[,Description:=NULL,]

# Clean airports file
airports <- fread("Airports.csv")
airports[,airport_name:=trimws(str_split(Description,":")[[1]][2]),by=1:nrow(airports)]
airports[,airport_location:=trimws(str_split(Description,":")[[1]][1]),by=1:nrow(airports)]
airports <- na.omit(airports)
airports[,Description:=NULL,]


# Main data file - Read
DataDf_oct <- fread("BTS_US_October_2019.csv")
DataDf_sep <- fread("BTS_US_September_2019.csv")
DataDf_august <- fread("BTS_US_August_2019.csv")

DataDf <- rbindlist(list(DataDf_august,DataDf_sep,DataDf_oct),use.names = T)

# Clean the main data file
requiredcols <- c("FL_DATE","OP_UNIQUE_CARRIER","OP_CARRIER_FL_NUM","ORIGIN_AIRPORT_ID","ORIGIN","DEST_AIRPORT_ID",
                  "DEST","CRS_DEP_TIME","DEP_TIME","DEP_DELAY","ARR_DELAY","CANCELLED","CARRIER_DELAY",
                  "WEATHER_DELAY","NAS_DELAY","SECURITY_DELAY","LATE_AIRCRAFT_DELAY")

DataDf <- DataDf[,(requiredcols),with=F]
DataDf[,FL_DATE:=ymd(FL_DATE),]
DataDf <- DataDf[!is.na(DEP_DELAY),,]
DataDf[,Day:=day(FL_DATE),]
DataDf[,Month:=month(FL_DATE),]
DataDf[,undayid := (Month*100+Day),]
DataDf[,Dep_delay:=ifelse(DEP_DELAY>0,1,0),]
DataDf[,Dep_delay_time:=ifelse(Dep_delay==1,DEP_DELAY,NA),]


aggDf <- DataDf[,.(Total_delays = sum(Dep_delay,na.rm = T),
                   Delay_time_median = median(Dep_delay_time,na.rm = T), 
                   Total_flights = .N),.(OP_UNIQUE_CARRIER,FL_DATE)]
setorder(aggDf,OP_UNIQUE_CARRIER,FL_DATE)


## Calculate the delay percentages
aggDf <- DataDf[,.(Total_delays = sum(Dep_delay,na.rm = T),
                   Delay_time_median = median(Dep_delay_time,na.rm = T), 
                   Total_flights = .N),.(OP_UNIQUE_CARRIER,FL_DATE)]
setorder(aggDf,OP_UNIQUE_CARRIER,FL_DATE)


# Calculate the Carrier/Day of the Week/Week of the month - Delay probability
aggDf <- merge(aggDf,aircarriers[,.(SmallCode,Airlines),],by.x = c("OP_UNIQUE_CARRIER"),
               by.y = c("SmallCode"),all.x = T)

uniqairlines <- unique(aggDf$Airlines)
i <- uniqairlines[1]
airlines_dist_df <- data.frame()
count <- 1

for(i in uniqairlines){
  tempdf <- aggDf[Airlines == i,,]
  fit <- fitdist(tempdf$Total_delays,"norm")
  airlines_dist_df[count,"Airlines"] <- i
  airlines_dist_df[count,"Mean"] <- round(fit$estimate[1])
  airlines_dist_df[count,"Std.Deviation"] <- round(fit$estimate[2])
  airlines_dist_df[count,"Total_Delays"] <- sum(tempdf$Total_delays)
  airlines_dist_df[count,"Total_flights"] <- sum(tempdf$Total_flights)
  
  m <- ggplot(tempdf, aes(x = Total_delays)) + 
    geom_histogram(aes(y =..density..), bins = 10, colour = "black", fill = "white") +
    stat_function(fun = dnorm, args = list(mean = mean(tempdf$Total_delays), sd = sd(tempdf$Total_delays))) +
    xlab("Delays(#)")+ ggtitle(i)
  
  ggsave(path = paste0(getwd(),"/Plots/"),plot = m,filename = paste0(i,".png"))
  
  count <- count +1
}


## Percentages
aggDf <- DataDf[,.(Total_delays = sum(Dep_delay,na.rm = T),
                   Delay_time_median = median(Dep_delay_time,na.rm = T), 
                   Total_flights = .N),.(OP_UNIQUE_CARRIER,FL_DATE)]
setorder(aggDf,OP_UNIQUE_CARRIER,FL_DATE)
aggDf <- merge(aggDf,aircarriers[,.(SmallCode,Airlines),],by.x = c("OP_UNIQUE_CARRIER"),
               by.y = c("SmallCode"),all.x = T)

aggDf[,Percentage_Delay:=(Total_delays/Total_flights),]

uniqairlines <- unique(aggDf$Airlines)
airlines_dist_df <- data.frame()
count <- 1

for(i in uniqairlines){
  tempdf <- aggDf[Airlines == i,,]
  fit <- fitdist(tempdf$Percentage_Delay,"norm")
  airlines_dist_df[count,"Airlines"] <- i
  airlines_dist_df[count,"Mean"] <- round(fit$estimate[1],2)
  airlines_dist_df[count,"Std.Deviation"] <- round(fit$estimate[2],2)
  airlines_dist_df[count,"Total_Delays"] <- sum(tempdf$Total_delays)
  airlines_dist_df[count,"Total_flights"] <- sum(tempdf$Total_flights)
  
  m <- ggplot(tempdf, aes(x = Percentage_Delay)) +
    geom_histogram(aes(y =..density..), bins = 10, colour = "black", fill = "white") +
    stat_function(fun = dnorm, args = list(mean = mean(tempdf$Percentage_Delay), sd = sd(tempdf$Percentage_Delay))) +
    xlab("Delays(#)")+ ggtitle(i)

  ggsave(path = paste0(getwd(),"/Plots/"),plot = m,filename = paste0(i,".png"))
  
  count <- count +1
}


## Run Mu test for every possible combination
testdf <- data.frame()
count <- 1

for(j in uniqairlines){
  for(i in uniqairlines){
    if(i==j){next}
    tempdf1 <- aggDf[Airlines == j,.(Percentage_Delay),]
    tempdf2 <- aggDf[Airlines == i,.(Percentage_Delay),]
    test_result <- wilcox.test(tempdf1$Percentage_Delay,tempdf2$Percentage_Delay,alternative = "less")
    testdf[count,"Main_Airline"] <- j
    testdf[count,"Comparitive_Airline"] <- i
    testdf[count,"p-value"] <- test_result$p.value
    testdf[count,"Better"] <- ifelse(test_result$p.value < 0.01,"Yes","No")
    count <- count +1
  }  
}

fwrite(testdf,"test_results_percentages.csv", row.names = F)
















