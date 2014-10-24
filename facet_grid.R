setwd("E:/StatsCommunication/hw4a")
data = read.csv("Engrade_sample.csv",header = TRUE,as.is = TRUE,stringsAsFactors = FALSE)
data[data$gender=="F",]$gender = "Female"
data[data$gender=="M",]$gender = "Male"
data[data$subject==1,]$subject = "Math"
data[data$subject==5,]$subject = "Science"
data[data$subject==12,]$subject = "English"
library(dplyr)
indata = select(data,subject,gradelevel,gender,sbm10,attd10,PF)

library(ggplot2)
library(ggthemes)

#define global variables
x_lab = "Assignment Submission Rate"
y_lab = "Attendance Rate"
subtitle = c("6th Grade Level", "7th Grade Level", "8th Grade Level")


#generate a plot function for all the plot
grid_plot = function(m){
  p <- ggplot(filter(indata,gradelevel==m),aes(x = attd10,y = sbm10)) +
    geom_point(aes(color = factor(PF)),alpha = 0.8) +
    theme_igray()+ggtitle(paste(m,"th Grade Level",sep = ""))+labs(x = x_lab,y = y_lab) +
    ylim(50,100)+
    theme(text = element_text(size = 10, face="bold"))
  
  plot <- p+
    facet_grid(subject~gender)+
    scale_color_discrete(name="Result",labels = c("Fail","Pass"))+
    theme(strip.background = element_rect( fill = "pink",
                                          size = 3))
  return(plot)
}

#put all the plots on a facet grid
main = textGrob("
              How Attendance Rate and Submission Rate Affect a Student's Final Performance
              Relationship between Final Success of a Student and his Attendance & Submission rate across Genders, Grade Levels and Subjects
              ",just="center",gp = gpar(fontsize = 20,fontface = 2))

do.call(grid.arrange,c(plots,list(ncol = 3, main = main)))

grid.arrange(plots[[1]],plots[[2]],plots[[3]],ncol = 3)

plots = lapply(6:8,grid_plot)
myargs <- list(ncol = 3)]
env <- new.env()

do.call(grid.arrange,plots)
grid.arrange(plots)



p6 <- ggplot(filter(indata,gradelevel==6),aes(x = attd10,y = sbm10)) +
      geom_point(aes(color = factor(PF)),alpha = 0.8) +
      theme_igray()+ggtitle(subtitle[1])+labs(x = x_lab,y = y_lab) +
      theme(text = element_text(size = 20, face="bold"))

plot6 <- p6+facet_grid(subject~gender)+
        scale_color_discrete(name="Result",labels = c("Fail","Pass"))+
  theme(strip.background = element_rect(colour = "purple", fill = "pink",
                                        size = 3, linetype = "dashed"))

p7 <- ggplot(filter(indata,gradelevel==7),aes(x = attd10,y = sbm10)) +
  geom_point(aes(color = factor(PF)),alpha = 0.8) +
  theme_igray()+ggtitle(subtitle[2])+labs(x = x_lab,y = y_lab) +
  theme(text = element_text(size = 20, face="bold"))

plot7 <- p7+facet_grid(subject~gender)+
  scale_color_discrete(name="Result",labels = c("Fail","Pass"))+
  theme(strip.background = element_rect(colour = "purple", fill = "pink",
                                        size = 3, linetype = "dashed"))

p8 <- ggplot(filter(indata,gradelevel==8),aes(x = attd10,y = sbm10)) +
  geom_point(aes(color = factor(PF)),alpha = 0.8) +
  theme_igray()+ggtitle(subtitle[3])+labs(x = x_lab,y = y_lab) +
  theme(text = element_text(size = 20, face="bold"))

plot8 <- p8+facet_grid(subject~gender)+
  scale_color_discrete(name="Result",labels = c("Fail","Pass"))+
  theme(strip.background = element_rect(colour = "purple", fill = "pink",
                                        size = 3, linetype = "dashed"))



        
plot2_6 = p+facet_grid(subject~gender)
g = list(plot1,plot2)


library(grid)
pushViewport(viewport(layout = grid.layout(1, 2)))
print(plot1, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(plot2, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))

library(gridExtra)
grid.arrange(plot6,plot7, plot8,ncol = 3, main = "Main Title")
