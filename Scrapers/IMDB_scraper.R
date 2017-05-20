#Packages
library(RCurl)
library(rjson)
library(XML)
library(data.table)
library(plyr)

#Setting the Directory
rm(list = ls())
setwd("C:/Users/ganji.kiran/Downloads/Others/MovieModel")
#setwd("E:/Machine-Learning-Proj/MovieModel")

#Main Code
MainDf <- data.frame(c("StartValue"),c(2000))
colnames(MainDf)[1] <- "MovieList"
colnames(MainDf)[2] <- "Year"

url_nos <- c(2000: 2016)
for(i in 1:length(url_nos)){
  print(i)
  RunYear <- url_nos[i]
  url <-  paste0("https://en.wikipedia.org/wiki/List_of_Telugu_films_of_",url_nos[i])
  SOURCE <-  getURL(url,encoding="UTF-8") #Download the page
  
  #Parsing the source code pulled
  SourceParsed <- htmlParse(SOURCE)
  
  #Targeting the td followed by i tags.
  movie_list <- xpathSApply(SourceParsed, "//td/i", xmlValue)
  
  roughdf <- as.data.frame(movie_list)
  colnames(roughdf)[1] <- "MovieList"
  roughdf$Year <- RunYear
  
  MainDf <- rbind(MainDf, roughdf)
}

write.csv(MainDf, "Movies.csv", row.names = FALSE)


#Pull the details from IMBD

MovieList <- read.csv("Movies.csv")

MainDf2 <- data.frame()

for(i in 290:nrow(MovieList)){
  print(i)
  MovieName <- MovieList[i,"MovieList"]
  YearOfRelease <- MovieList[i,"Year"]
  
  urlsc <- paste0("http://www.omdbapi.com/?t=", gsub(" ","+", MovieName),
                "&y=", YearOfRelease, "&plot=short&r=json")
  
  lookUp <- URLencode(urlsc)
  rd <- readLines(lookUp, warn="F") 
  dat <- as.data.frame(fromJSON(rd))
  
  if(dat$Response == "True"){
    MainDf2 <- rbind.fill(MainDf2, dat)
  }
  
}

write.csv(MainDf2, "MovieDetails.csv", row.names = FALSE)










