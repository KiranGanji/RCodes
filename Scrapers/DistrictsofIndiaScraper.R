#Libraries
library(RCurl)
library(rjson)
library(XML)
library(data.table)
library(plyr)
library(xlsx)

#Setting the Directory
rm(list = ls())
setwd("D:/Clustering/DistrictsofIndia")

#Functions
multmerge = function(mypath){
  filenames=list.files(path=mypath, full.names=TRUE)
  datalist = lapply(filenames, function(x){read.csv(file=x,header=T)})
  Reduce(function(x,y) {rbind(x,y)}, datalist)
}

#Getting data from the districts of india
url <- paste0("http://www.districtsofindia.com/chhattisgarh/alldistricts/bankandfinance/index.aspx")
SOURCE <-  getURL(url,encoding="UTF-8") #Download the pages
SourceParsed <- htmlParse(SOURCE)

#Getting the sector names and URLS
sector_info <- as.data.frame(xpathSApply(SourceParsed, "//div[@class='sections_next']//a", xmlValue))
colnames(sector_info)[1] <- "SectorInfo"
sector_info_urls <- as.data.frame(xpathSApply(SourceParsed, "//div[@class='sections_next']//a", xmlGetAttr, 'href'))
colnames(sector_info_urls)[1] <- "Sector Info Urls"

SectorInfos <- cbind(sector_info, sector_info_urls)
SectorInfos$`Sector Info Urls` <- paste0("http://districtsofindia.com",SectorInfos$`Sector Info Urls`)

write.csv(SectorInfos,"SectorsInfos.csv", row.names = FALSE)


#Within a sector getting the data attributes

urlstobescraped <- c("http://www.districtsofindia.com/westbengal/alldistricts/bankandfinance/index.aspx",
                     "http://www.districtsofindia.com/westbengal/alldistricts/demographics/scheduledcastesc-scheduledtribest/index.aspx",
                     "http://www.districtsofindia.com/westbengal/alldistricts/demographics/agegrouppopulation/index.aspx",
                     "http://www.districtsofindia.com/westbengal/alldistricts/economy/income/index.aspx",
                     "http://www.districtsofindia.com/westbengal/alldistricts/economy/economiccensus/index.aspx",
                     "http://www.districtsofindia.com/westbengal/alldistricts/economy/marketsize/index.aspx",
                     "http://www.districtsofindia.com/westbengal/alldistricts/education/index.aspx",
                     "http://www.districtsofindia.com/westbengal/alldistricts/healthandfamilywelfare/index.aspx",
                     "http://www.districtsofindia.com/westbengal/alldistricts/industriesandmining/index.aspx",
                     "http://www.districtsofindia.com/westbengal/alldistricts/labourandworkforce/index.aspx",
                     "http://www.districtsofindia.com/westbengal/alldistricts/media/index.aspx",
                     "http://www.districtsofindia.com/westbengal/alldistricts/travelandtourism/index.aspx")

url <- paste0("http://www.districtsofindia.com/chhattisgarh/alldistricts/bankandfinance/index.aspx")

for(url in urlstobescraped){
  SOURCE <-  getURL(url,encoding="UTF-8") #Download the pages
  SourceParsed <- htmlParse(SOURCE)#Parse the page
  
  list_head <- xpathSApply(SourceParsed, "//div[@class='list_head']", xmlValue)#Target the required class
  print(list_head)
  attributes <- as.data.frame(xpathSApply(SourceParsed, "//div[@class='category_container']//li", xmlValue))#target the requrired ones
  colnames(attributes)[1] <- "Attributes"
  
  attributes$Sector <- list_head
  attributes$Attributes <- gsub(" ","",attributes$Attributes)
  write.csv(attributes, paste0(gsub("/","_",list_head),".csv"), row.names = FALSE)
}

MergedDf <- multmerge(paste0(getwd(),"/Kiran/"))
write.xlsx(MergedDf,"MergedDf.xlsx", row.names = FALSE)

MergedDf <- multmerge(paste0(getwd(),"/Somenath/"))
write.xlsx(MergedDf,"MergedDfSomeNath.xlsx", row.names = FALSE)


StatesUrl <- c("jammuandkashmir",
               "himachalpradesh",
               "punjab",
               "chandigarh",
               "uttarakhand",
               "haryana",
               "rajasthan",
               "uttarpradesh",
               "bihar",
               "sikkim",
               "arunachalpradesh",
               "nagaland",
               "manipur",
               "mizoram",
               "tripura",
               "meghalaya",
               "assam",
               "westbengal",
               "jharkhand",
               "odisha",
               "chhattisgarh",
               "madhyapradesh",
               "gujarat",
               "maharashtra",
               "andhrapradesh",
               "karnataka",
               "goa",
               "kerala",
               "tamilnadu")

sectorstobelooped <- gsub("http://www.districtsofindia.com/westbengal/alldistricts/","",urlstobescraped)

for(states in StatesUrl){
  print(states)
  for(sectors in sectorstobelooped){
    print(sectors)
    url <- paste0("http://www.districtsofindia.com/",states,"/alldistricts/",sectors)
    
    SOURCE <-  getURL(url,encoding="UTF-8") #Download the pages
    SourceParsed <- htmlParse(SOURCE)
    
    list_head <- xpathSApply(SourceParsed, "//div[@class='list_head']", xmlValue)
    print(list_head)
    attributes <- as.data.frame(xpathSApply(SourceParsed, "//div[@class='category_container']//li", xmlValue))
    if(nrow(attributes) == 0){
      next()
    }
    colnames(attributes)[1] <- "Attributes"
    
    attributes$Sector <- list_head
    attributes$Attributes <- gsub(" ","",attributes$Attributes)
    attributes$State <- states
    write.csv(attributes, paste0("Allfiles/",states,"_",gsub("/","_",list_head),".csv"), row.names = FALSE)
  }
}

MergedDf <- multmerge(paste0(getwd(),"/Allfiles/"))
write.xlsx(MergedDf,"AllStatesColumns.xlsx", row.names = FALSE)

colsreqd <- c("District-wise Number of Households Having Television (Total/Rural/Urban) (2011)",
              "District-wise Number of Households Having Television (Total/Rural/Urban) (2001)",
              "District-wise Number of Houses used for Hotel, Lodge, Guest Houses etc. (Total/Rural/Urban) (2011)",
              "District-wise Employment on Rolls in Coal Mines (2013)",
              "District-wise Age Group-wise Adolescent and Youth Main Workers by Residence and Sex (As per 2011 Census)",
              "District-wise Number of Houses used for Factory, Workshop, Work Shed etc. (Total/Rural/Urban) (2011)",
              "District-wise Number of Registered Factories and Workers Employed Daily (2010)",
              "District-wise Number of Micro and Small Scale Industries and Employment (2009-2010)",
              "District-wise Per Capita Household Consumption Potential Per Annum (2009-2010)",
              "District-wise Number of Houses used for Shops and Offices (Total/Rural/Urban) (2011)",
              "District-wise Estimates of Per Capita Income {At Constant (2004-2005) Prices} (2004-2005 to 2012-2013)",
              "District-wise Purchasing Power Parity of Rural/Urban/Total Households (2007)",
              "District-wise Number of Offices, Aggregate Deposits and Gross Bank Credit of Private Sector Banks (Quarter Ended December, 2015)",
              "District-wise Number of Offices, Aggregate Deposits and Gross Bank Credit of Nationalised Banks(Quarter Ended December, 2015)",
              "District-wise Number of Offices, Aggregate Deposits and Gross Bank Credit of All Scheduled Commercial Banks(Quarter Ended December, ","2015)",
              "District-wise Number of Offices, Aggregate Deposits and Gross Bank Credit of Regional Rural Banks(Quarter Ended December, 2015)",
              "District-wise Number of Offices, Aggregate Deposits and Gross Bank Credit of Foreign Banks (Quarter Ended December, 2015)",
              "District-wise Number of Households Availing Banking Services (Total/Rural/Urban) (2001)",
              "District-wise Female Headed Household Literates Population by Residence and Sex (As per 2011 Census)",
              "District-wise Age Group-wise Population by Educational Level (Below Primary/Primary/Middle/Matric/Secondary) by Residence and Sex (As ","per 2011 Census)",
              "District-wise Age Group-wise Population by Educational Level (Higher Secondary/Intermediate/Technical Diploma or Certificate not Equal ","to Degree/Graduate and Above/Unclassified) by Residence and Sex (As per 2011 Census)",
              "District-wise Religion-wise Number of Illiterates by Residence and Sex in West Bengal (As per 2011 Census)",
              "District-wise Number of Primary, Middle and Secondary/Higher Secondary Schools (2007-2008)",
              "District-wise Number of Hospitals and Sub Divisional Hospitals (As on 31st March, 2015)",
              "District-wise Age-Group-wise Population (Person/Male/Female//Total/Rural/Urban) (As per 2011 Census)",
              "District-wise Marital Status of Single Year Age by Residence and Sex (As per 2011 Census)")


colsreqd2 <- c()

for(cols in colsreqd){
  colsreqd2 <- append(colsreqd2, trimws(unlist(strsplit(cols,"\\("))[1]))
}



colsreqd2 <- colsreqd2[colsreqd2!="2015)"]
colsreqd2 <- colsreqd2[colsreqd2!="per 2011 Census)"]
colsreqd2 <- colsreqd2[colsreqd2!="to Degree/Graduate and Above/Unclassified) by Residence and Sex"]
colsreqd2 <- colsreqd2[colsreqd2!="District-wise Age Group-wise Population by Educational Level"]
colsreqd2 <- colsreqd2[colsreqd2!="District-wise Estimates of Per Capita Income {At Constant"]

colsreqd2 <- append(colsreqd2,"District-wise Age Group-wise Population by Educational Level (Below Primary/Primary/Middle/Matric/Secondary) by Residence and Sex")
colsreqd2 <- append(colsreqd2,"District-wise Age Group-wise Population by Educational Level (Higher Secondary/Intermediate/Technical Diploma or Certificate not Equal to Degree/Graduate and Above/Unclassified) by Residence and Sex")


colsreqd3 <- as.data.frame(colsreqd2)
colnames(colsreqd3)[1] <- "ColumnPattern"

i <- 1

for(cols in colsreqd2){
  print(cols)
  colsreqd3[i,"repeats"] <- nrow(MergedDf[grep(cols, MergedDf$Attributes), ])
  i <- i+1
}

write.csv(colsreqd3,"colsreqd_count.csv", row.names = FALSE)

cols <- "District-wise Number of Registered Factories and Workers Employed Daily"
nrow(MergedDf[grep(cols, MergedDf$Attributes), ])
Yo <- MergedDf[grep(cols, MergedDf$Attributes), ]
unique(Yo$State)





