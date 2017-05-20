#Libraries
library(RCurl)
library(rjson)
library(XML)
library(data.table)
library(plyr)
library(xlsx)

#Setting the work directory
rm(list = ls())
setwd("D:/MMM")

#Main Code
cities <- c("kochi","delhi","mumbai", "chennai", "hyderabad","kolkata","bengaluru")
area_codes <- c(204289,202396,204842,206671,202190,206690,204108)

CityCodes <- as.data.frame(cbind(city = cities, area_code = area_codes))
CityCodes <- data.table(CityCodes)

NdaysDf <- fread("ndays.csv")
TemperaturesDf <- data.frame()

yrVec <- c(2015, 2016)

for(cit in cities){
  print(cit)
  AreaCode <- as.character(CityCodes[city == cit,area_code,])
  
  for(yr in yrVec){
    print(yr)
    if(yr == 2015){mth <- c(1:12)}else{mth <- c(1:10)}
    for(m in mth){
      print(m)
      Ndays <- NdaysDf[Year == yr & Month == m,Ndays,]
      url <- paste0("http://www.accuweather.com/en/in/",cit,"/",AreaCode,"/month/",AreaCode,"?monyr=", m,"/01/", yr,
                    "&view=table")
      SOURCE <-  getURL(url,encoding="UTF-8") #Download the pages
      SourceParsed <- htmlParse(SOURCE)
      
      Dates <- xpathSApply(SourceParsed, "//th/time", xmlValue)
      Temperatures <- xpathSApply(SourceParsed, "//td[5]", xmlValue)[1:Ndays]
      
      Df <- as.data.frame(cbind(date=Dates, Temp = Temperatures))
      Df$year <- yr
      Df$city <- cit
      TemperaturesDf <- rbind(TemperaturesDf, Df)
    }
  }
}

write.xlsx(TemperaturesDf,"Temperatures_delhi.xlsx", row.names = FALSE)


#Getting the URLS

url <- paste0("http://www.accuweather.com/en/browse-locations/asi/in")
SOURCE <-  getURL(url,encoding="UTF-8") #Download the pages

SourceParsed <- htmlParse(SOURCE)
hrefs <- xpathSApply(SourceParsed, "//h6/a", xmlGetAttr, 'href')






