setwd("E:/Personal Affairs/RA/Amsterdam_Ave/Plot_May_30th")
library(ggplot2)
library(reshape2)
library(ggthemes)

##Read in the data
Amsterdam = read.csv("Amsterdam.csv",header = TRUE,stringsAsFactors = FALSE,as.is = TRUE)
Amsterdam = Amsterdam[-389,-c(191:ncol(Amsterdam))]
##Change all the missings to 0
cleanfct = function(x)if(x == 99|x == "?"|is.na(x)){
  x = 0 
}else{
  x = as.numeric(x)
}
g = apply(Amsterdam[,-c(1:11)],c(1,2),cleanfct)
g = as.data.frame(g,stringsAsFactors = FALSE)
final = cbind(Amsterdam[,"neighborhood"],g)
colnames(final)[1] = "neighborhood"

#Set up the information tables which will be used later
abbre = c("WC","HK","CL","LC","UWS","MVY","MH","MVE","HH","WHS","WHN","INW")
abbreTable = cbind(unique(Amsterdam[,"neighborhood"]),abbre)
nameList = c("car","trash", "garbage", "waste", "bottle", "cigar", 
           "snow", "odor", "vermin", "graffiti", "p_graffiti", 
           "mural", "merchandise", "vendors",
           "teen_num","teen_adult","teen_fight","teen_idle","teen_other",
           "attitude","see_crime","see_police","see_interact","safety"
)
description = read.csv("Description.csv",header = TRUE,stringsAsFactors = FALSE)


##Distinguish different types of data
singleDetect = function(x){
  rr=(unique(final[,x]))
  l = length(rr)
  return(l)
}
valueCount = sapply(nameList,singleDetect)
exclude1 = names(which(valueCount==1))
majority = names(which(valueCount==2))
majorityTable = description[match(majority,description[,1]),]
others = names(which(valueCount!=1 & valueCount!=2))
otherTable = description[match(others,description[,1]),]

#Prepare the data for ggplot2
setupData = function(x){
  m = final[,c("neighborhood",x)]
  m1 = dcast(m,neighborhood~m[,2])
  m2 = t(apply(m1[,-1],1,function(x)as.numeric(x/sum(x))))
  m2 = as.data.frame(m2)
  neighborhood = as.character(m1[,"neighborhood"])
  m2 = cbind(neighborhood,m2)
  colnames(m2)=colnames(m1)
  m3 = melt(m2,id.var="neighborhood")
  #m4 = transform(m3,neighborhood = factor(neighborhood,unique(Amsterdam[,"neighborhood"])))
  m3[,1]=abbreTable[,2][match(abbreTable[,1],m3[,1])]
  m4 = transform(m3,neighborhood = factor(neighborhood,abbre))
  m4[,3]=as.numeric(format(round(m4[,3]*100, 2), nsmall = 2))
  return(m4)
}

#plot binary category variable
plotFct = function(t){
  data = binary[[t]][c(13:24),]
  plot = ggplot(data,aes(x = as.factor(neighborhood),y = value))+ geom_bar(aes(fill = variable),position = "dodge",stat="identity")
  plot1 = plot+theme_economist()+theme(text = element_text(size = 20, family="Georgia", face="bold"),legend.position="none")+geom_text(data = data,aes(label = value),vjust=-0.5,size = 7.5,family="Georgia")
  plot2 = plot1+ggtitle(majorityTable[t,2])+xlab("Neighborhood")+ylab("Percentage")
  return(plot2)
  
}
mPlotFct = function(g){
  plot1.0 = ggplot(multipleData[[g]],aes(x = as.factor(neighborhood),y = value))+ geom_bar(aes(fill = Category),position = "dodge",stat="identity")
  plot1.1 = plot1.0+theme_economist()+theme(text = element_text(size = 20, family="Georgia", face="bold"))+geom_text(data = multipleData[[g]],aes(label = value,group = Category),size = 4,vjust=-0.5,position = position_dodge(width = 1.05),family="Georgia")+scale_fill_discrete(labels = legend_name[[g]])
  plot1.2 = plot1.1+ggtitle(otherTable[g,2])+xlab("Neighborhood")+ylab("Percentage")
  return(plot1.2)
  
}


#set up two different data lists
binary = lapply(majority,setupData)
multiple = lapply(others,setupData)
multipleData = lapply(c(1:6),function(x){colnames(multiple[[x]])[2]="Category"
                                       return(multiple[[x]])})

lapply(c(1:length(majority)),plotFct)
lapply(c(1:6),mPlotFct)

#plot multiple category
teen_num = multiple[[1]]
attitude = multiple[[2]]
see_crime = multiple[[3]]
see_police = multiple[[4]]
see_interact = multiple[[5]]
safety = multiple[[6]]
multipleData = list(teen_num,attitude,see_crime,see_police,see_interact,safety)

legend_name = list(c("Missing","None","1-5","More than 5"),c("Missing","No Attention","Suspicion","Friendly","Curious","No People"),
                 c("Missing","Not at all","Some","Quite a bit"),c("Missing","Not at all","Some","Quite a bit"),
                 c("Missing","No recognition","Brief Greeting","Conversation"),c("Missing","Unsafe","Self-conscious","Comfortable","At Home"))

#teen_num_table = rbind(c("0","None"),c("1","1-5"),c("2",'5 or more'))
#colnames(teen_num_table)=c("old","Category")
#teen_num_final = merge(teen_num_table,teen_num,by.x="old",by.y="variable")[,-1]



plot = ggplot(binary[[4]][c(13:24),],aes(x = as.factor(neighborhood),y = value))+ geom_bar(aes(fill = variable),position = "dodge",stat="identity")
plot1 = plot+geom_text(data = binary[[4]][c(13:24),],aes(label = binary[[4]][c(13:24),]$value))
plotFct(4)
plot1 = plot+ggtitle("Bottle Distribution")+xlab("Neighborhood")+ylab("Density")
plot = ggplot(m4,aes(x = as.factor(neighborhood),y = value))+ geom_bar(aes(fill = variable),position = "dodge",stat="identity")
plot1 = plot+ggtitle("Bottle Distribution")+xlab("Neighborhood")+ylab("Density")
setupData("bottle")
bottle = final[,c("neighborhood","bottle")]
bottle1 = dcast(bottle,neighborhood~bottle)
bottle2 = t(apply(bottle1[,-1],1,function(x)as.numeric(x/sum(x))))
bottle2 = as.data.frame(bottle2)
neighborhood = as.character(bottle1[,"neighborhood"])
bottle2 = cbind(neighborhood,bottle2)
colnames(bottle2)=c("neighborhood","Yes","None")
bottle3 = melt(bottle2,id.var="neighborhood")
bottle3[,1]=abbreTable[,2][match(abbreTable[,1],bottle3[,1])]
bottle4 = transform(bottle3,neighborhood = factor(neighborhood,abbre))
x = format(round(1.233333, 2), nsmall = 2)


uu = signif(bottle4[,3],2)
library(ggplot2)
plot = ggplot(bottle4[c(1:12),],aes(x = as.factor(neighborhood),y = value))+ geom_bar(aes(fill = variable),position = "dodge",stat="identity")
plot1 = plot+geom_text(data = bottle4,aes(label = bottle4$value),position = position_dodge(width = 0.9))
plot1 = plot+theme_economist()+theme(text = element_text(size = 20, family="Georgia", face="bold"))+ggtitle("Bottle Distribution")+xlab("Neighborhood")+ylab("Density")

#letter
letter = read.csv("letter.csv",header = TRUE,stringsAsFactors = FALSE,as.is = TRUE)
letter = letter[,c(5:8)]
letter1 = dcast(letter[,c(1:2)],neighborhood~letter1)
letter2 = t(apply(letter1[,-1],1,function(x)as.numeric(x/sum(x))))
letter2 = as.data.frame(letter2)
neighborhood = as.character(letter1[,"neighborhood"])
letter2 = cbind(neighborhood,letter2)
colnames(letter2)=colnames(letter1)
letter3 = melt(letter2,id.var="neighborhood")
letter3[,1]=abbreTable[,2][match(abbreTable[,1],letter3[,1])]
letter4 = transform(letter3,neighborhood = factor(neighborhood,abbre))
letter4[,3]=as.numeric(format(round(letter4[,3]*100, 2), nsmall = 2))
plotLetter = ggplot(letter4[13:24,],aes(x = as.factor(neighborhood),y = value))+ geom_bar(aes(fill = variable),position = "dodge",stat="identity")
plotLetter1 = plotLetter+theme_economist()+theme(text = element_text(size = 20, family="Georgia", face="bold"),legend.position="none")+geom_text(data = letter4[c(13:24),],aes(label = value),vjust=-0.5,size = 7.5,family="Georgia")
plotLetter2 = plotLetter1+ggtitle("Return Rate of Dropped Letters on Each Block")+xlab("Neighborhood")+ylab("Percentage")

letterinfo = read.csv("letterinfo.csv",header = TRUE,stringsAsFactors = FALSE,as.is = TRUE)
letterinfo = letterinfo[,-6]
letterinfo1 = melt(letterinfo)
letterinfo1[,1]=abbreTable[,2][match(abbreTable[,1],letterinfo[,1])]
colnames(letterinfo1)=c("neighborhood","Type","value")
letterinfo1 = transform(letterinfo1,neighborhood = factor(neighborhood,abbre))

plotletterinfo = ggplot(letterinfo1,aes(x = as.factor(neighborhood),y = value))+ geom_bar(aes(fill = Type),position = "dodge",stat="identity")
plotLetterinfo1 = plotletterinfo+theme_economist()+theme(text = element_text(size = 20, family="Georgia", face="bold"))+geom_text(data = letterinfo1,aes(label = value,group = Type),size = 4,vjust=-0.5,position = position_dodge(width = 1.05),family="Georgia")
plotLetterinfo2 = plotLetterinfo1+ggtitle("Return Rate of Different Types of Dropped Letters on Each Block")+xlab("Neighborhood")+ylab("Percentage")
