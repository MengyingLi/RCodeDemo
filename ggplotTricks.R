# How to draw the error bar on ggplot + ggplot template
ggplot(final_result, aes(x=Item, y=oddsRatio,, fill = Item)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=lowbound, ymax=highbound),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))+geom_hline(y=1,linetype="dashed",  # Dashed line
                                                        size = 1.5, colour = "blue") + geom_text(aes(label=oddsRatio),position=position_dodge(width=0.9), hjust = 2.5,  vjust = -0.75, size=7) +
  geom_text(aes(label=final_result2$P, y = 0.5),size = 7)+
  theme_economist()+theme(text  =  element_text(size  =  12, family = "Georgia", face = "bold")) + ggtitle("Odds Ratio to Determine the Association between Featured Feedback and Rating Behavior")+
  scale_y_continuous(breaks = seq(0,10,by=1)) +guides(fill=FALSE)+
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust = 0.5, size = 20)) + xlab("Indicators") + ylab("Odds Ratio")

# How to specify date object in ggplot2
p1 = ggplot(arpSpanCount,aes(x = Date,y = Frequency) ) + geom_bar(stat = "identity") + xlab("Chrome Install Date") + ylab ("Number of PCs") +
  scale_x_date(breaks = date_breaks("week"),
               labels = date_format("%d-%b"),
               minor_breaks = "1 day")


# Set up manual aesthetics
ggplot(filter(fullData,value<=50000 & value >=30000), aes(x=variable,y=value, color = color,alpha = color, size = color,group = CensusID))+
  
  scale_color_manual(values = c("1" ="#FFFF00", "2" = "#31a354")) + scale_alpha_manual(values = c("1" = 1,"2" = 0.2)) +
  scale_size_manual(values = c("1" = 1.5, "2" = 0.2)) + geom_line()  + facet_grid(Model~group) 

scale_y_continuous(breaks = c(20000,40000,60000)) +scale_color_manual(values=c(rep("#31a354",82),"#9999CC"))

# ggpairs
ggpairs(selectedUSData_Concise, diag=list(continuous="density", discrete="bar"), axisLabels="show")