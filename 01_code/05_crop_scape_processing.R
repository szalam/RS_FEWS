rm(list=ls())

#library
library(raster)

#working directory
wd = list()
wd$main = 'F:/RS_FEWS/'
wd$output = 'F:/RS_FEWS/04_results/'
wd$cropscape = 'directory for cropscape data'
setwd(wd$cropscape)

#files are too big. Not uploaded in github. Can be found here
files=list.files(pattern = '.tif')
files=c("CDL_2007_clip_20190415132249_1136514398.tif",
        "CDL_2008_clip_20190413023300_72247386.tif",
        "CDL_2009_clip_20190415132412_2019877209.tif",
        'CDL_2010_clip_20190415132830_1963444618.tif',
        "CDL_2011_clip_20190413015240_1095434829.tif",
        "CDL_2012_clip_20190413011104_1078156209.tif",
        "CDL_2013_clip_20190413010949_149869713.tif",
        "CDL_2014_clip_20190413010753_1410344636.tif" ,
        "CDL_2015_clip_20190413010712_1659131611.tif",
        "CDL_2016_clip_20190413002546_598990627.tif",
        "CDL_2017_clip_20190413002253_1182527336.tif",
        "CDL_2018_clip_20190413023300_72247386.tif")

# files="CDL_2018_clip_20190413023300_72247386.tif"
years=c(2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018)
# years=c(2018)
length(files)

# data=raster(files[1])
# plot(data)

# load the shapefile
load(paste0(wd$main,'04_shape/subregion_shape.rda'))

shp = shape_shift
coord1=crs(shp[[1]])

all_freq=list()
for(n in 1:length(years))
{
  # n=13
  data=raster(files[n])
  data
  
  crs_data=crs(data)
  freq_list=list()

  for (i in 1:21) {
  data2=data
  temp2=spTransform(shape_shift[[i]],crs_data)
  temp2
  
  # plot(temp2,add=T)
  temp3=crop(data2,temp2)
  temp4=mask(temp3,temp2)
  # plot(temp4)
  freq_list[[i]]=freq(temp4)
  # get the list of files in the folder
  # files = list.files(".",full.names = TRUE, pattern = ".nc")
  
  # create a raster stacks
  # data = stack(files)
  # freq(data)
  
  # create a vector of indices for stackApply
  # index_stack = rep(c(1:length(files)),each=12)
  
  # convert the monthly stack into annual stack
  # data = stackApply(data,index_stack,fun=sum,na.rm=TRUE)
  
  # extract the data for the hydroSHEDS basins
  # data_final = extract(data,shp,mean,na.rm=TRUE)
  # data_final = round(data_final,2)
  # data_final=matrix(NA,nrow=21,ncol=180)
  
  # temp=crop(data,shape_shift[[1]])
  # temp=extract(data,shape_shift[[i]],mean,na.rm=TRUE)
  # data_final[i,]=temp
  write.csv(freq_list[[i]],paste0(wd$output,'subregion_',i,'_',years[n],'.csv'))
  }
 all_freq[[n]]=freq_list 
}