rm(list = ls())

#directory
wd=list()
wd = list()
wd$main = 'F:/RS_FEWS/'
wd$data = 'F:/RS_FEWS/04_results/'
wd$output = 'F:/RS_FEWS/04_results/'
wd$crop_names='F:/RS_FEWS/02_data/'
wd$output_sumofarea = 'F:/RS_FEWS/04_results/'

#input
years=c(2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018)

#load crop names
crop_info=read.csv(paste0(wd$crop_names,'cropScape_areas.csv'))
head(crop_info)

crop_info_main=c(as.character(crop_info[2:nrow(crop_info),2]),as.character(crop_info[1,2]))
tail(crop_info_main)
crop_info_category=c(as.character(crop_info[2:nrow(crop_info),3]),as.character(crop_info[1,3]))
temp_area_stor=NULL

for (i in 1:21) {
  data_all=data.frame(matrix(-999,nrow = 255,ncol = (length(years)+3)))
  data_all[,1]=c(1:254,-999)
  data_all[,2]=crop_info_main
  data_all[,3]=crop_info_category
  dim(data_all)
  k=4
  for (j in years) {
    temp=read.csv(paste0(wd$data,'subregion_',i,'_',j,'.csv'))
    for (nn in 1:nrow(temp)) {
      id_temp=which(temp[nn,2]==data_all[,1])
      data_all[id_temp,k]=temp[[nn,3]]
    }
    k=k+1
  }
  colnames(data_all)=c('crop_id','crop_name','crop_category',years) 
  data_all_final=data_all[which(data_all$crop_category== 'C' | data_all$crop_category== 'T' | data_all$crop_category== 'R'),]
  
  # data_all_final=data_all[which(data_all$crop_category== 'T'),]
  
  data_all_final[data_all_final==-999]='NA'
  # write.csv(data_all_final,paste0(wd$output,'subregion_',i,'_summary.csv'))
  
  k=4
  temp_area=NULL
  for (gkl in 1:length(years)) {
    temp=as.numeric(data_all_final[,k])
    temp_area=cbind(temp_area,sum(temp,na.rm = T))
    k=k+1
  }
  
  id_2007=which(years==2007)
  temp_area[1,id_2007]= temp_area[1,id_2007]*56*56/1000000 # 56 is the resolution in m, 1000000 to convert to km2
  temp_area[,-id_2007]=temp_area[,-id_2007]*30*30/1000000# 30 is the resolution in m, 1000000 to convert to km2
  
  temp_area=data.frame(paste0('subregion_',i),temp_area)
  temp_area_stor=rbind(temp_area_stor,temp_area)
 
}

#export data
write.csv(temp_area_stor,paste0(wd$output_sumofarea,'subregion_sumofarea_summary_both_tree_and_row.csv'))
