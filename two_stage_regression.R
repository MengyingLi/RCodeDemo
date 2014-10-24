# Modeling Problem: 
#  Using the given data set create a repeat sales index using the Case and Shiller methodology as described in the attached paper.
#  NOTE: This dataset was created in the following way from a database of property transactions.
#		Every transaction is matched to prior sales subject to two things:
#                        That the PropertyIDs are match
#                        and that the matched transaction occured before the current observation.
#                This causes a problem in that every transaction is matched not just to the transaction immediately prior, 
#                but to transactions prior to that. 
#   Cleaning may be required.
#
#   Comment and structure your code to be easily followed.
#
# Loading the necessary packages
if(!require(McSpatial)){
  install.packages("McSpatial")
  library(McSpatial)
}
if(!require(dplyr)){
  install.packages("dplyr")
  library(dplyr)
}
if(!require(reshape2)){
  install.packages("reshape2")
  library(reshape2)
}
if(!require(lubridate)){
  install.packages("lubridate")
  library(lubridate)
}
if(!require(lmtest)){
  install.packages("lmtest")
  library(lmtest)
}


##################################Step 1 Generate the Data############################################
# Read in the data and select the variables for modelling
yourPath <- "E:/streetEasy"
setwd(yourPath)
dat <- read.csv("Transactions.csv",header=TRUE,as.is=TRUE)
datModel <- select(dat, propertyid, regionid, transdate, transvalue, transdate_previous, transvalue_previous)

# Delete records with no previous transcations (new house transactions) because this model pertains to resale.
datModelRep <- filter(datModel,!is.na(transvalue_previous))

# Divide the whole dataset into two parts: 
# PART 1: one contains the resale dates and amount for 2005 as well as 
# those for dates immediately prior to 2005, we call it "2005 Resale Pairs".
# 
# PART 2: The other has all the resale information for dates immediately prior to 2005 only. 
# We call it "Prior Resale Pairs".

## Preprocessing.
trans05 <- select(datModelRep, propertyid, regionid, transdate, transvalue)
trans05 <- trans05[!duplicated(trans05),]
trans05$transdate <- as.Date(trans05$transdate, "%m/%d/%Y")
transPrev <- select(datModelRep,propertyid,regionid,transdate_previous,transvalue_previous)
transPrev$transdate_previous <- as.Date(transPrev$transdate_previous,"%m/%d/%Y")

# PART 1: Find 2005 Resale Pairs
## Find the most recent transaction date before 2005
lastTransDate <- group_by(transPrev,propertyid,regionid) %>% summarise(transdate_previous=max(transdate_previous))
## Get the most recent transaction value before 2005
lastTrans <- merge(lastTransDate,transPrev)
## Create the data for resale pairs which have at least one transaction in 2005
transPair05 <- merge(lastTrans,trans05)
colnames(transPair05)[3:6]=c("transDate0","transValue0","transDate1","transValue1")

# PART 2: Find Prior Resales.
## How many resales occured before 2005
n_resale <- group_by(transPrev,propertyid,regionid) %>% summarise(np=n_distinct(transdate_previous))
unique(n_resale$np) #maximum is 3
## Delete the transactions occured only once before 2005. These records are included in part 1.
multi_resale_id <- filter(n_resale,np>=2)
## Get all the records for previous resale pairs
multi_resale <- merge(select(multi_resale_id,-np),transPrev)
## Sort the transation records for each property on transaction date and generate an ordered id for each transaction
multi_resale <- arrange(multi_resale,regionid,propertyid,transdate_previous)
multi_resale <- group_by(multi_resale,regionid,propertyid) %>% mutate(ordered=(1:length(transdate_previous)-1))
multi_resale$ordered_name=paste("trans",multi_resale$ordered,sep="")
## Cast the data to wide format for trans_value and trans_date respectively
multi_resale_casted_value <- dcast(multi_resale,propertyid + regionid ~ordered_name,value.var=c("transvalue_previous"))
colnames(multi_resale_casted_value)[3:5] <- c("transValue0","transValue1","transValue2")
multi_resale_casted_date <- dcast(multi_resale,propertyid + regionid ~ordered_name,value.var=c("transdate_previous"))
colnames(multi_resale_casted_date)[3:5] <- c("transDate0","transDate1","transDate2")
multi_resale_casted <- merge(multi_resale_casted_date,multi_resale_casted_value)
## Change the records from a dataframe which contains 3 separate transactions for each record
# to a dataframe with two transaction pairs for each record (1 record per line).
multi_resale_3 <- filter(multi_resale_casted,!is.na(transDate2)) %>% select(-transDate0,-transValue0)
colnames(multi_resale_3)[3:6] <- c("transDate0","transDate1","transValue0","transValue1")
multi_resale_2 <- select(multi_resale_casted,-transDate2,-transValue2)
multi_resale_final <- rbind(multi_resale_2,multi_resale_3)
## Change the date format as when doing dcast all the dates became integer
multi_resale_final[,c("transDate0","transDate1")] <- lapply(multi_resale_final[,c("transDate0","transDate1")],
                                                         function(x)as.Date(x,origin="1970-01-01")) #change integer to date
                                                         
# Merge 2005 transaction pairs with previous transaction pairs. Append the data from Part 2 to the one from Part 1. 
transFinal=rbind(transPair05,multi_resale_final)

##################################Step2 Fit the model########################################################
# First way is to use the "repsale" function from package "McSpatial"
require(lubridate)
transFinal$time1 <- year(transFinal$transDate1)
transFinal$time0 <- year(transFinal$transDate0)
fit <- repsale(price0=log(transFinal$transValue0), price1=log(transFinal$transValue1), time0=transFinal$time0, time1=transFinal$time1,stage3="square")
#The result we are interested in is the "Time 18" (2005) index which is 

# Second way is to run step-by-step three stage regressions
## Generate the variables for the model
dPrice <- log(transFinal$transValue1)-log(transFinal$transValue0)
timeVar <- levels(factor(c(transFinal$time0,transFinal$time1)))
nTime <- length(timeVar)
nPrice <- length(dPrice)

### set up a matrix to generate the year dummy variable
timeDiff <- array(0,dim=c(nPrice,nTime-1))
for (j in seq(2,nTime)) {
  timeDiff[,j-1] <- ifelse(transFinal$time1==timeVar[j], 1,timeDiff[,j-1])#find the latter time in the resale pairs
  timeDiff[,j-1] <- ifelse(transFinal$time0==timeVar[j],-1,timeDiff[,j-1])#find the former time in the resale pairs
}
colnames(timeDiff) <- paste("Time",seq(1+1,nTime))

## First stage regression
fit1 <- lm(dPrice~timeDiff+0) 
e <- residuals(fit1)

## Second stage regression
secX <- transFinal$time1-transFinal$time0
fit2 <- lm(e^2~secX)
### Get the weights for each observation
wgt <- fitted(fit2)
#### Change the negative weights to 0
wgt <- ifelse(wgt>0, 1/wgt, 0)

## Third stage regression
fit3<- lm(dPrice~timeDiff+0,weights=wgt)
names(fit3$coefficients) <- timeVar[-1]
fit3$coefficients[length(fit3$coefficients)]
