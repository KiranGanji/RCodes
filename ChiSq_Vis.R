#ChiSquare test for factor variable visualization
#Libraries
library(corrplot)
library(tidyr)

#Functions
varlist <- function (df=NULL,type=c("numeric","factor","character"), pattern="", exclude=NULL) {
  vars <- character(0)
  if (any(type %in% "numeric")) {
    vars <- c(vars,names(df)[sapply(df,is.numeric)])
  }
  if (any(type %in% "factor")) {
    vars <- c(vars,names(df)[sapply(df,is.factor)])
  }  
  if (any(type %in% "character")) {
    vars <- c(vars,names(df)[sapply(df,is.character)])
  }  
  vars[(!vars %in% exclude) & grepl(vars,pattern=pattern)]
}

#Reading a sample dataset
MainDf <- fread("HR_Data_Groups.csv", stringsAsFactors = TRUE)

#Converting some numeric columns to factor variables
colstofacs <- c("number_project", "Work_accident", "promotion_last_5years", "left")
for(i in colstofacs){
  MainDf[,eval(parse(text = i)):= as.factor(eval(parse(text = i))),]
}

#Get the factor variables
FacVars <- varlist(MainDf, "factor")

#Calculating the ChiSq test and trying to visualize the matrix
SquarTestDf <- data.frame()
counts <- 1
for(i in FacVars){
  for(j in FacVars){
    if(i!=j){
      u <- chisq.test(MainDf[,get(i),], MainDf[,get(j),])
      SquarTestDf[counts,"LeftVar"] <- i
      SquarTestDf[counts,"RightVar"] <- j
      SquarTestDf[counts,"X-Squared"] <- u$statistic
      SquarTestDf[counts,"p-value"] <- u$p.value
      SquarTestDf[counts,"Degrees"] <- u$parameter
      
      counts <- counts +1
    }
  }
}

SquarTestDf2 <- SquarTestDf
SquarTestDf2$`X-Squared` <- NULL
SquarTestDf2$Degrees <- NULL

#Converting the dataframe into a matrix
ChiSqMat <- spread(data = SquarTestDf2,key = RightVar, value = `p-value`)
ChiSqMat[is.na(ChiSqMat)] <- 0
row.names(ChiSqMat)<- ChiSqMat$LeftVar
ChiSqMat$LeftVar <- NULL
ChiSqMatMain <- as.matrix(ChiSqMat)

#Visualizing the matrix
corrplot(ChiSqMatMain, method = "number", type = "full")


