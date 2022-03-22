# clear workspace
rm(list=ls())

# load required libraries
library(raster)
library(rgdal)

# set working directory 
setwd("F:/RS_FEWS/")

# load the shapefile
load('03_shape/rim_shape.rda') 

plot(1, type="n", xlab="", ylab="", xlim=c(-124, -117), ylim=c(35, 42))

temp=0
for (i in 1:46) {
  plot(shape_shift[[i]],add=T)
  # plot(shape_shift[[i]],labels=c(1))
  # text(shape_shift[[i]]$OBJECTID)
  text(shape_shift[[i]],labels=i, cex= 1.2)
  # temp=c(temp,area(shape_shift[[i]]))
}
temp=temp[-1]

# get the list of files in the folder
files = list.files(full.names = TRUE, pattern = ".nc")

# create a raster stacks
data = stack(files)

# create the indices for stackApply
# index_stack = rep(c(1:21),each=12)

# convert the monthly stack into annual stack
# data = stackApply(data,index_stack,sum,na.rm=TRUE)

# rotate the raster brick
data = flip(t(flip(data,direction="y")),direction='y')

# extract the data for the hydroSHEDS basins
data_final=matrix(NA,nrow=46,ncol=252)
for (i in 1:210) {
  temp=extract(data,shape_shift[[i]],mean,na.rm=TRUE)
  data_final[i,]=temp
}

data_final = round(data_final,2)
data_final[data_final<0]=NA
data_final[data_final>40000]=NA

# create the final dataset with proper column names
subregs=1:46
ids=paste0('sub_',subregs)
data_final = cbind(ids,data_final)
colnames(data_final) = c("subregs_ID",paste0("Y",rep(1998:2018,each=12)))
temp=data.frame(data_final[,2:ncol(data_final)])
dim(temp)
head(temp)

dymon=read.csv('02_data/dyMon.csv')[,2]
for (i in 1:length(dymon)) {
  temp[,i]=as.numeric(as.character(temp[,i]))*24*dymon[i]
}

head(temp)

# write the data into a file
write.table(data.frame(data_final[,1],temp),"04_results/01_P-TRMM_3B43_CV_headwaterwat.txt",row.names = FALSE,quote = FALSE)






