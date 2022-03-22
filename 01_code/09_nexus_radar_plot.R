rm(list=ls())

#library
library(fmsb)

#directory
wd = list()
wd$main = 'F:/RS_FEWS/'
wat_data=read.table(paste0(wd$main,'02_data/water_pump.txt'),skip = 1)
energy_data=read.table(paste0(wd$main,'02_data/energy.txt'),skip = 1)
row_crop_data=read.table(paste0(wd$main,'02_data/row_crops.txt'),skip = 1)
tree_crop_data=read.table(paste0(wd$main,'02_data/tree_crops.txt'),skip = 1)
tot_crop_data=read.table(paste0(wd$main,'02_data/tot_crops.txt'),skip = 1)
setwd(wd$main)

#input
year_selected=c(2007,2005,2001)
sub_selected=16-12

#preprocess
temp=data.frame(matrix(0,nrow=3,ncol=7))
for (i in 1:length(year_selected)) {
  temp[i,1]=year_selected[i]
  
  id_wat=which(wat_data[,1]==year_selected[i])
  temp[i,2]=(wat_data[id_wat,sub_selected]-1)*100
  
  id_energy=which(energy_data[,1]==year_selected[i])
  temp[i,3]=(energy_data[id_energy,sub_selected]-1)*100
  
  id_row=which(row_crop_data[,1]==year_selected[i])
  temp[i,4]=(row_crop_data[id_row,sub_selected]-1)*100
  
  id_tree=which(tree_crop_data[,1]==year_selected[i])
  temp[i,5]=(tree_crop_data[id_tree,sub_selected]-1)*100
  
  id_tot_crop=which(tot_crop_data[,1]==year_selected[i])
  temp[i,6]=(tot_crop_data[id_tot_crop,sub_selected]-1)*100
  
  temp[i,7]=temp[i,4]/temp[i,5]
  
}
colnames(temp)=c('Year','Water','Energy','Row_crop','Tree_crop','Crop','Tree_frac')

#radar plot
# Create data: note in High school for several students
data=temp
rownames(data)=data[,1]

# To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each topic to show on the plot!
data=rbind(rep(30,length(year_selected)) , rep(-30,length(year_selected)) , data[,c(2,3,6)])
data

# Plot 1: Default radar chart proposed by the library:
radarchart(data,axistype=1 , pcol=c('red','black','blue'),plty=1,plwd=2,
           #custom the grid
           # cglcol="grey", cglty=2, axislabcol="grey", caxislabels=seq(1.15,1.2,.01), cglwd=0.8,
           cglcol="grey", cglty=2, axislabcol="grey", caxislabels=seq(-30,10,30), cglwd=0.8,
           #custom labels
           vlcex=1.5 )
alegend(x=0.7, y=1, legend = rownames(data[-c(1,2),]), bty = "n", pch=20 , col=c('red','black','blue') , text.col = "grey", cex=1.2, pt.cex=3)

