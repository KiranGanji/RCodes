#Libraries
library(RCurl)
library(rjson)
library(XML)
library(data.table)
library(plyr)
library(xlsx)
library(stringr)

#Setting the work directory
rm(list = ls())
setwd("D:/TheEconomist")

#Main Code
#Getting the URLS

pagenos <- c(25:29)
urlmain <- paste0("http://www.economist.com/blogs/graphicdetail")

UrlsDf <- data.frame()

for(i in pagenos){
  print(i)
  url <- paste0(urlmain,"?page=",pagenos)
  SOURCE <-  getURL(url,encoding="UTF-8") #Download the pages
  
  SourceParsed <- htmlParse(SOURCE)
  hrefs <- xpathSApply(SourceParsed, "//article/a", xmlGetAttr, 'href')
  hrefs <- data.frame(links = hrefs)
  
  UrlsDf <- rbind(UrlsDf,hrefs)
}

UrlsDf$MainLink <- "http://www.economist.com"

UrlsDf$FullLinks <- paste0(UrlsDf$MainLink,UrlsDf$links)

Urls <- as.vector(UrlsDf$FullLinks)
Urls <- Urls[!duplicated(Urls)]
counts <- 250


for(i in 50:length(Urls)){
  print(i)
  url <- Urls[i]
  SOURCE <-  getURL(url,encoding="UTF-8") #Download the pages
  
  SourceParsed <- htmlParse(SOURCE)
  hrefs <- xpathSApply(SourceParsed, "//picture[@class='component-image blog-post__image']/img", xmlGetAttr, 'src')
  
  if(length(hrefs)!=0){
    download.file(hrefs,destfile=paste0("AutomatedMain/",counts,".png"),mode = 'wb')
    counts <- counts+1
  }
  
}




