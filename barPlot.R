if (!require(ggplot2)){
  install.packages(ggplot2)
}
if (!require (ggthemes)){
  install.packages(ggthemes)
}

x_lab  =  
y_lab  =  
title  =  
name_order  =  
#Single Barplot for each group
singleBarPlot  =  function(data,name,value){
data = transform(data,name = factor(name,name_order)
plot1.0 = ggplot(data = data,aes(x = factor(name),y = as.numeric(value)))+geom_bar(stat = "identity")
plot1.1 = plot2+theme_economist()+theme(text  =  element_text(size  =  20, family = "Georgia", face = "bold"),legend.position = "none")+geom_text(data = lenStu2,aes(label = value),vjust = -0.5,size = 7.5,family = "Georgia")
plot1.2 = plot2.1+xlab(x_lab)+ylab(y_lab)+ggtitle(title)
return(plot1.2)
}
#Multiple Barplots for each group

mPlotFct = function(data,name,value,fill_variable,legend_name){
  data = transform(data,name = factor(name,name_order))
  plot1.0 = ggplot(data,aes(x = as.factor(name),y = as.numeric(value)))+ geom_bar(aes(fill = fill_variable),position  =  "dodge",stat = "identity")
  plot1.1 = plot1.0+theme_economist()+theme(text  =  element_text(size  =  20, family = "Georgia", face = "bold"))+geom_text(data = data,aes(label = value,group = fill_variable),size = 4,vjust = -0.5,position  =  position_dodge(width = 1.05),family = "Georgia")+scale_fill_discrete(labels  =  legend_name)
  plot1.2 = plot1.1+ggtitle(otherTable[g,2])+xlab(x_lab)+ylab(y_lab)
  return(plot1.2)
  
}