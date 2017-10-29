# Libraries
library(RCurl)
library(rjson)
library(XML)
library(data.table)
library(plyr)
library(xlsx)
library(stringr)

#Setting the directory
rm(list = ls())
setwd("F:/MBA")
load(".RData")

#Main Code
url <- "http://www.economist.com/whichmba/full-time-mba-ranking"
pagenos <- c(0:9)

Mergedframe <- data.frame()
for(i in pagenos){
  print(i)
  urlmain <- ifelse(i==0,url,paste0(url,"?page=",i))
  SOURCE <-  getURL(urlmain,encoding="UTF-8")
  SourceParsed <- htmlParse(SOURCE)
  ranks <- xpathSApply(SourceParsed, 
                       "//td[@class='views-field views-field-field-wmba-school-rank-overall-value active']",
                       xmlValue)
  ranks <- trimws(ranks)
  schools <- xpathSApply(SourceParsed, 
                       "//td[@class='views-field views-field-field-wmba-school-name-alpha-value']",
                       xmlValue)
  schools <- trimws(schools)
  schoolurls <- xpathSApply(SourceParsed, 
                            "//td[@class='views-field views-field-field-wmba-school-name-alpha-value']/a",
                            xmlGetAttr,"href")
  schoolurls <- paste0("http://www.economist.com",schoolurls)
  country <- xpathSApply(SourceParsed, 
                         "//td[@class='views-field views-field-name']",
                         xmlValue)
  
  country <- trimws(country)
  ResultFrame <- data.table(Rank=ranks, School= schools, Country = country, URL=schoolurls)
  Mergedframe <- rbind(Mergedframe,ResultFrame)
}

collegeurls <- Mergedframe$URL
AdditionalDetailsFrame <- data.frame()

for(i in collegeurls[81:100]){
  print(i)
  collectdetails <- data.table(URL=i)
  urlmain <- paste0(i)
  SOURCE <-  getURL(urlmain,encoding="UTF-8")
  SourceParsed <- htmlParse(SOURCE)
  
  details <- xpathSApply(SourceParsed, 
                       "//div[@class='ec-wmba-school-information']",
                       xmlValue)
  tables <- readHTMLTable(SOURCE)
  reqdtable <- tables[[2]]
  reqdtable <- as.data.frame(t(reqdtable))
  colnames(reqdtable) <- as.character(unlist(reqdtable[1, ]))
  reqdtable <- reqdtable[-1, ]
  rownames(reqdtable) <- NULL
  collectdetails <- cbind(collectdetails,data.table(Details=details), reqdtable)
  
  #Looping through tabs
  tabs <- c(1,2,6,7)
  for(j in tabs){
    print(paste0("In tab ",j))
    urlmain <- paste0(i,"?tab=",j)
    SOURCE <-  getURL(urlmain,encoding="UTF-8")
    tables <- readHTMLTable(SOURCE)
    reqdtable <- tables[[2]]
    reqdtable <- as.data.frame(t(reqdtable))
    colnames(reqdtable) <- as.character(unlist(reqdtable[1, ]))
    reqdtable <- reqdtable[-1, ]
    rownames(reqdtable) <- NULL
    collectdetails <- cbind(collectdetails,reqdtable)
  }
  AdditionalDetailsFrame <- rbind.fill(AdditionalDetailsFrame,collectdetails)
}

MergedframeFinal <- merge(Mergedframe,AdditionalDetailsFrame, by=c("URL"),all.x = T)
write.xlsx(MergedframeFinal,"CollegeDetails.xlsx", row.names = F)








