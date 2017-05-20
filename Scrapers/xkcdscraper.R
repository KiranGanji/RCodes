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
setwd("D:/Newsletter/MJML_Template/LazyLaughs")

#Main Code
#Getting the URLS
urlmain <- paste0("https://xkcd.com")


for(i in 900:999){
  print(i)
  url <- paste0(urlmain,"/",i,"/")
  SOURCE <-  getURL(url,encoding="UTF-8") #Download the pages
  
  SourceParsed <- htmlParse(SOURCE)
  hrefs <- xpathSApply(SourceParsed, "//div[@id='comic']/img", xmlGetAttr, 'src')
  hrefs <- paste0("https:",hrefs)
  
  if(nchar(hrefs)>6){
    download.file(hrefs,destfile=paste0("xkcd/",i,".png"),mode = 'wb')
  }
}





