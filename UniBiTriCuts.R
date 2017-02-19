#Packages
library(data.table)

#Setting the Directory
rm(list = ls())
setwd("E:/Machine-Learning-Proj/HRData")


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


#Main Code
MainDf <- fread("HR_comma_sep.csv", stringsAsFactors = TRUE)
summary(MainDf)
colnames(MainDf)

#Converting Numerical to Factors by binning
SatGroups <- seq(0,1.2,0.2)
Satlabels= c("0_2","2_4","4_6","6_8","8_10","10=+")
MainDf[,SatGroups:= cut(satisfaction_level, breaks= SatGroups, right= FALSE, labels= Satlabels)]

#Converting Numerical to Factors by binning
LastEvalGroups <- seq(0,1.2,0.3)
LastEvallabels= c("<3","3_6","6_9","9+")
MainDf[,LastEvalGroups:= cut(last_evaluation, breaks= LastEvalGroups, right= FALSE, labels= LastEvallabels)]


#Converting Numerical to Factors by binning
MainDf$number_project <-as.factor(MainDf$number_project)
LastEvalGroups <- c(0,3,5,8)
LastEvallabels= c("3-","3_5","5+")
MainDf[,ProjectGroups:= cut(number_project, breaks= LastEvalGroups, right= FALSE, labels= LastEvallabels)]

MainDf$average_montly_hours <-as.numeric(MainDf$average_montly_hours)
LastEvalGroups <- seq(90, 320, (320-90)/5)
LastEvallabels= c("90_136","136_182","182_228","228_274","274+")
MainDf[,MonthlyHoursGroups:= cut(average_montly_hours, breaks= LastEvalGroups, right= FALSE, labels= LastEvallabels)]
quantile(MainDf$average_montly_hours)



write.csv(MainDf,"HR_Data_Groups.csv", row.names = FALSE)


#Writing univariate and all other cuts
MainDf <- fread("HR_Data_Groups.csv", stringsAsFactors = TRUE)

#Converting some numeric columns to factor variables
colstofacs <- c("number_project", "Work_accident", "promotion_last_5years", "left")
for(i in colstofacs){
  MainDf[,eval(parse(text = i)):= as.factor(eval(parse(text = i))),]
}

#Get the factor variables
FacVars <- varlist(MainDf2, "factor")

#Univariate Cuts
MainDf2<- copy(MainDf)
MainDf2 <- MainDf[left == "1",,]
MainDf2[,left:=NULL,]

MainFrame <- data.frame()
counts <- 1

for(i in FacVars){
  u <- setorder(data.frame(table(MainDf2[,get(i),])),-Freq)
  u <- data.table(u) 
  u[,Perc:= Freq/sum(Freq),]
  
  MainFrame[counts, "ColumnName"] <- i
  MainFrame[counts, "Lead1"] <- as.character(u[1,Var1,])
  MainFrame[counts, "Lead2"] <- as.character(u[2,Var1,])
  MainFrame[counts, "Lead3"] <- as.character(u[3,Var1,])
  MainFrame[counts, "Lead4"] <- as.character(u[4,Var1,])
  
  MainFrame[counts, "Freq1"] <- as.character(u[1,Freq,])
  MainFrame[counts, "Freq2"] <- as.character(u[2,Freq,])
  MainFrame[counts, "Freq3"] <- as.character(u[3,Freq,])
  MainFrame[counts, "Freq4"] <- as.character(u[4,Freq,])
  
  MainFrame[counts, "Perc1"] <- as.character(u[1,Perc,])
  MainFrame[counts, "Perc2"] <- as.character(u[2,Perc,])
  MainFrame[counts, "Perc3"] <- as.character(u[3,Perc,])
  MainFrame[counts, "Perc4"] <- as.character(u[4,Perc,])
  
  MainFrame[counts, "CummulativePerc"] <- sum(u[1:4,Perc,], na.rm = TRUE)
  
  counts <- counts +1
}

setorder(MainFrame, -CummulativePerc)
write.csv(MainFrame,"Univariate.csv", row.names = FALSE)

#Bivariate Cuts

BiFrame <- data.frame()

for(i in 1:length(FacVars)){
  for(j in 1:length(FacVars)){
    if(j>i){
      Colname1 <- FacVars[i]
      Colname2 <- FacVars[j]
      
      u <- setorder(as.data.frame(table(MainDf2[,get(Colname1),], MainDf2[,get(Colname2),])),-Freq)
      u <- data.table(u) 
      u[,Perc:= Freq/sum(Freq),]
      
      u[,Column1:= Colname1,]
      u[,Column2:= Colname2,]
      
      colsorder <- c("Column1", "Column2", "Var1", "Var2", "Freq", "Perc")
      u <- u[1:4,(colsorder),with=FALSE]
      
      BiFrame <- rbind(BiFrame, u)
    }
  }
}

setorder(BiFrame, -Perc)

write.csv(BiFrame, "BiVariateCuts.csv", row.names = FALSE)

#Trivariate Cuts

TriFrame <- data.frame()

for(i in 1:length(FacVars)){
  for(j in 1:length(FacVars)){
    if(j > i){
      for(k in 1:length(FacVars)){
        if(k>j){
          Colname1 <- FacVars[i]
          Colname2 <- FacVars[j]
          Colname3 <- FacVars[k]
          
          u <- setorder(as.data.frame(table(MainDf2[,get(Colname1),], MainDf2[,get(Colname2),], MainDf2[,get(Colname3),])),-Freq)
          u <- data.table(u) 
          u[,Perc:= Freq/sum(Freq),]
          
          u[,Column1:= Colname1,]
          u[,Column2:= Colname2,]
          u[,Column3:= Colname3,]
          
          colsorder <- c("Column1", "Column2","Column3", "Var1", "Var2", "Var3", "Freq", "Perc")
          u <- u[1:4,(colsorder),with=FALSE]
          
          TriFrame <- rbind(TriFrame, u)
        }
      }
    }
  }
}

setorder(TriFrame, -Perc)

write.csv(TriFrame, "TriVariateCuts.csv", row.names = FALSE)



