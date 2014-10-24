data = read.table('modifyWV.csv',header = TRUE,as.is = TRUE,stringsAsFactors = FALSE,sep = ",")

performance = data[,5:85]
performance = data.matrix(performance)
index = sort(c(seq(11,89,by = 3),seq(12,90,by = 3)))
performance2 = data[,index]
performance2 = apply(performance2,c(1,2),as.numeric)

teacherid = unique(data$teacherid)
set.seed(50)
col_teacher<-rgb(runif(50),runif(50),runif(50)) 
teacherid = cbind(teacherid,col_teacher)

schoolid = unique(data$schoolid)
set.seed(50)
col_school<-c(brewer.pal(12,"Set3"),brewer.pal(3,"Set1"))[1:14]
schoolid = cbind(schoolid,col_school)

subject = unique(data$subject)
col_subject = brewer.pal(11,'Paired')
subject = cbind(subject,col_subject)

gender = unique(data$gender)
col_gender = c("blue","pink","white")
gender = cbind(gender,col_gender)

ethnicity = unique(data$ethnicity)
col_ethnicity = brewer.pal(8,'Accent')
ethnicity = cbind(ethnicity,col_ethnicity)

PF = unique(data$PF)
col_PF = c("green","red")
PF = cbind(PF,col_PF)

data = merge(data,gender,by = 'gender')
data = merge(data,ethnicity,by = 'ethnicity')
data = merge(data,subject,by = 'subject')
data = merge(data,schoolid,by = 'schoolid')
data = merge(data,teacherid,by = 'teacherid')
data = merge(data,PF,by = 'PF')

my_palette <- colorRampPalette(c("#ffffcc","#ffeda0",	"#fed976",	"#feb24c",	"#fd8d3c",	"#fc4e2a",	"#e31a1c",	"#bd0026",	"#800026"))(1000)
row_annotation <- data[,c('col_school','col_teacher','col_subject','col_gender','col_ethnicity','col_PF')]
row_annotation<-t(row_annotation)
rownames(row_annotation) <- c("School", "Teacher","Subject","Gender","Ethnicity","PF")


par(lend  =  1)           # square line ends for the color legend
legend("topright",      # location of the legend on the heatmap plot
       legend  =  c("category1", "category2", "category3"), # category labels
      col  =  c("gray", "blue", "black"),  # color key
      lty =  1,             # line style
       lwd  =  10            # line width
          )
par(lend  =  1)           # square line ends for the color legend

layout(1:2, heights = c(1, 5))

# Legend panel
par(mar = rep(0,4))
plot(0, 0, type = "n", ann = FALSE, axes = FALSE)
legend("center", c("5-10 cm", "15-20 cm", "25-30 cm"), horiz = TRUE,
       lty = 2:4, col = 1:3)

# Plot panel
par(mar = c(5,4,0,2))
plot(1:20, cumsum(rnorm(20)))
library(GMD)
heatmap.3(performance1,Colv = NA,RowSideColors = row_annotation,col = my_palette,main = "Hierarchical Clustering Heatmap (Assignment Score)",labRow = FALSE)
heatmap.3((performance2),hclustfun = myclust, distfun = mydist,Colv  = NA,RowSideColors = row_annotation,col = my_palette,main = "Hierarchical Clustering Heatmap (Assn+Sbm)",labRow = FALSE)

mydist = function(c) {dist(c,method = "euclidean")}
myclust = function(c) {hclust(c,method = "ward.D2")}

par(lend  =  2)           # square line ends for the color legend
legend("topleft",
       legend  =  c("Male", "Female"), # category labels
       col  =  c("blue", "pink"),  # color key
       lty =  1,             # line style
       lwd  =  10            # line width
)
