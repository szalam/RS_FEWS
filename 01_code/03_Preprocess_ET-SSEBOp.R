# clear workspace
rm(list=ls())

# load required libraries
library(raster)

# set working directory 
setwd("F:/RS_FEWS/")

# load the shapefile
load('03_shape/subregion_shape.rda') 

shp=shape_shift

# get the list of files in the folder
files = list.files(".",full.names = TRUE, pattern = ".tif")
length(files)
files

# create a vector of indices for stackApply
 # index_stack = rep(c(1:length(files)),each=12)

# convert the monthly stack into annual stack
# data = stackApply(data,index_stack,fun=sum,na.rm=TRUE)
data_final = NULL
for (i in 1:length(shp)){
  # for (i in 1:15){
  print(i)
  data_temp2=NULL
  for (ss in 1:length(files)) {
    data=raster(files[ss])
    # data=stack(data,temp)
    #index_start = 1 + (i-1)*10
    #index_end = index_start + 9
    if(extent(shp[[i]])@ymax>extent(data)@ymax){
      data_temp=NA
      data_final = rbind(data_final,data_temp)
    }else{
      rast_temp = crop(data,extent(shp[[i]])+.5)
      # plot(rast_temp[[1]])
      rast_temp[rast_temp>30000]=NA
      crs(rast_temp) = crs(shp)
      # extract the data for the hydroSHEDS basins
      # data_temp = extract(data,shp[i,],mean,na.rm=TRUE)
      data_temp= extract(rast_temp,shp[[i]],mean,na.rm=TRUE)
      data_temp = round(data_temp,2)
      data_temp2=c(data_temp2,data_temp)
      
    }
  }
  data_temp2=data.frame(t(as.data.frame(data_temp2)))
  data_final = rbind(data_final,data_temp2)
}

# data_final = round(data_final,2)
subregs=1:21
ids=paste0('sub_',subregs)
data_final = cbind(ids,data.frame(data_final))

# create the final dataset with proper column names
# data_final = cbind(shp@data$HYBAS_ID,data_final)
colnames(data_final) = c("HydroSHEDS_ID",paste0("Y",rep(2003:2018,each=12),'.',rep(1:12)),
                         paste0("Y",rep(2019,times=3),'.',rep(1:3)))

# write the data into a file
write.table(data_final,"F:/RS_FEWS/04_results/03_ET-SSEBOp.txt",row.names = FALSE,quote = FALSE)
