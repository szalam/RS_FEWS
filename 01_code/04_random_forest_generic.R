#clear working directory
rm(list=ls())

#library
library(hydroGOF)
library(randomForest)
library(Metrics)

#directory locations
wd=list()
wd$main='F:/RS_FEWS/'
wd$data='F:/RS_FEWS/02_data/'
wd$output='F:/RS_FEWS/04_results/'

#set directory
setwd(wd$main)


#importing processed data
#import response variable]
pump=read.csv(paste0(wd$data,'C2VSIM_pumping_1950-2009_fineGrid.csv'))
ppt=read.csv(paste0(wd$data,'TRMM_precipitation_CV.csv'))
ppt_headwat=read.csv(paste0(wd$data,'TRMM_precipitation_headwat.csv'))
et=read.csv(paste0(wd$data,'SSEBOp_ET_CV.csv'))
et_headwat=read.csv(paste0(wd$data,'GLEAM_ET_headwat.csv'))
pet=read.csv(paste0(wd$data,'GLEAM_PET_CV.csv'))
ndvi=read.csv(paste0(wd$data,'AVHRR_NDVI_CV.csv'))

#volumetric conversion from height
sub_area=read.csv(paste0(wd$data,'subregion_area.csv'))
headwat_area=read.csv(paste0(wd$data,'headwatershed_area.csv'))

#model development
rand_for_model=function(ppt,et,pet,ndvi,ppt_headwat,et_headwat,per_start,per_end,sub_select)
{

  for (i in 1:21) {
    ppt=ppt[(which(ppt[,2]==per_start)[1]:(which(ppt[,2]==per_end)[length(which(ppt[,2]==per_end))])),]
    ppt[,3+i]=ppt[,3+i]*sub_area[i,2]/1000 # dividing by 1000 to convert mm to m
    et=et[(which(et[,2]==per_start)[1]:(which(et[,2]==per_end)[length(which(et[,2]==per_end))])),]
    et[,3+i]=et[,3+i]*sub_area[i,2]/1000 # dividing by 1000 to convert mm to m
    # pet=pet[(which(pet[,2]==per_start)[1]:(which(pet[,2]==per_end)[length(which(pet[,2]==per_end))])),]
    # pet[,3+i]=pet[,3+i]*sub_area[i,2]/1000 # dividing by 1000 to convert mm to m
    # ndvi=ndvi[(which(ndvi[,2]==per_start)[1]:(which(ndvi[,2]==per_end)[length(which(ndvi[,2]==per_end))])),]
  
    # ppt_fut=ppt_b[(which(ppt_b[,2]==per_start_fut)[1]:(which(ppt_b[,2]==per_end_fut)[length(which(ppt_b[,2]==per_end_fut))])),]
    # ppt_fut[,3+i]=ppt_fut[,3+i]*sub_area[i,2]/1000 # dividing by 1000 to convert mm to m
    # et_fut=et_b[(which(et_b[,2]==per_start_fut)[1]:(which(et_b[,2]==per_end_fut)[length(which(et_b[,2]==per_end_fut))])),]
    # et_fut[,3+i]=et_fut[,3+i]*sub_area[i,2]/1000 # dividing by 1000 to convert mm to m
    # pet_fut=pet_b[(which(pet_b[,2]==per_start_fut)[1]:(which(pet_b[,2]==per_end_fut)[length(which(pet_b[,2]==per_end_fut))])),]
    # pet_fut[,3+i]=pet_fut[,3+i]*sub_area[i,2]/1000 # dividing by 1000 to convert mm to m
    # ndvi_fut=ndvi_b[(which(ndvi_b[,2]==per_start_fut)[1]:(which(ndvi_b[,2]==per_end_fut)[length(which(ndvi_b[,2]==per_end_fut))])),]
    # 
  }
  
  #pumping convert acre ft to m3
  if(per_start<2014)
  {
    pump=pump[(which(pump[,2]==per_start)[1]:(which(pump[,2]==per_end)[length(which(pump[,2]==per_end))])),]
    pump[,4:25]=pump[,4:25]*1.23348e-6 #convert to km3
  }else{
    pump=pump[(per_end-per_start)*12,]
    pump[,4:25]=pump[,4:25]*1.23348e-6 #convert to km3
  }
  
  #headwatershed values conversion
  # for (i in 1:46) {
  #   ppt_headwat=ppt_headwat[(which(ppt_headwat[,2]==per_start)[1]:(which(ppt_headwat[,2]==per_end)[length(which(ppt_headwat[,2]==per_end))])),]
  #   ppt_headwat[,3+i]=ppt_headwat[,3+i]*headwat_area[i,2]/1000 # dividing by 1000 to convert mm to m
  #   et_headwat=et_headwat[(which(et_headwat[,2]==per_start)[1]:(which(et_headwat[,2]==per_end)[length(which(et_headwat[,2]==per_end))])),]
  #   et_headwat[,3+i]=et_headwat[,3+i]*headwat_area[i,2]/1000 # dividing by 1000 to convert mm to m
  # }
  # 
  # headwater subregion linkage file load
  sub_headwat_linkage=read.csv('C:/sarfaraz/project_RS_FEWs/random_forest/data/generic_data/headwatershed_linkage.csv')
  
  
  # create model for Tulare region, subregion ranges from 14 to 21
  
  #check linkage file and combine the headwater dataset
  # headwat_numbers=(which(sub_headwat_linkage[sub_select,1:8]==0)[1]-1)-1
  # headwat_ids=(sub_headwat_linkage[sub_select,2:(headwat_numbers+1)])
  
  # et_headwat_temp=NULL
  # ppt_headwat_temp=NULL
  # 
  # for (tt in 1:length(headwat_ids)) {
  #   if(length(headwat_ids)==1){
  #     et_headwat_temp=cbind(et_headwat_temp,et_headwat[,(3+c(headwat_ids[tt]))])
  #     ppt_headwat_temp=cbind(ppt_headwat_temp,ppt_headwat[,(3+c(headwat_ids[tt]))])
  #     break
  #   }
  #   et_headwat_temp=cbind(et_headwat_temp,et_headwat[,(3+c(headwat_ids[1,tt]))])
  #   ppt_headwat_temp=cbind(ppt_headwat_temp,ppt_headwat[,(3+c(headwat_ids[1,tt]))])
  # }
  # et_headwat_temp=data.frame(rowSums(et_headwat_temp))
  # ppt_headwat_temp=data.frame(rowSums(ppt_headwat_temp))
  # 
  comb=data.frame(pump=(pump[,3+sub_select]),
                  ppt=(ppt[,3+sub_select]),
                  et=(et[,3+sub_select])
                  # ndvi=(ndvi[,3+sub_select]),pet=(pet[,3+sub_select]),
                  # ppt_headwat=c(ppt_headwat_temp),
                  # et_headwat=c(et_headwat_temp)
  )
  # colnames(comb)[6:7]=c('ppt_headwat','et_headwat')
  return(comb)        
}


sub_select=21
comb=rand_for_model(ppt,et,pet,ndvi,ppt_headwat,et_headwat,per_start=2003,per_end=2014,sub_select=sub_select)
comb2=rand_for_model(ppt,et,pet,ndvi,ppt_headwat,et_headwat,per_start=2014,per_end=2018,sub_select=sub_select)

#checking the random forest with all dataset

# fit <- randomForest(pump ~ qin + ppt+et+crop_area,   data=comb)
# fit <- randomForest(pump ~  ppt+et+ndvi+pet+ppt_headwat+et_headwat,   data=comb)
# fit <- randomForest(pump ~  ppt+et,   data=comb)
fit=lm(pump~ppt+et,data=comb)
# fit <- randomForest(pump ~  ppt+et+ndvi+pet+ppt_et_headwat,   data=comb)
fit
summary(fit)
print(fit) # view results 

# #checking the important predictors
# importance(fit) # importance of each predictor
# varImpPlot(fit,type=2)


#Separating data into training and validation period 
# Training Set : Validation Set = 50 : 30 (random)
# set.seed(100)
# train <- sample(nrow(comb), 0.7*nrow(comb), replace = FALSE)

r=6
comb[,2]=c(rep(0,times=r),comb[1:(nrow(comb)-r),2])
comb=comb[(r+1):nrow(comb),]
# id_temp=c(1:(nrow(comb)-r))#
id_temp=c(1:36,61:84,121:nrow(comb))#
train <- (id_temp)
TrainSet <- comb[train,]
ValidSet <- comb[-train,]
comb_backup=comb[(r+1):nrow(comb),]


comb2[,2]=c(rep(0,times=r),comb2[1:(nrow(comb2)-r),2])
comb2=comb[(r+1):nrow(comb2),]
comb2_backup=comb2[(r+1):nrow(comb2),]#from the next year

#random forest model setup
model1 <- randomForest(pump ~ ppt+et,   data=TrainSet)
# model1=lm(pump~ppt+et,data=TrainSet)
model1


# Predicting on train set
predTrain <- predict(model1, TrainSet)
plot(predTrain,TrainSet[,1])
plot(1:length(predTrain),predTrain)
lines(1:length(predTrain),predTrain)
lines(1:length(predTrain),TrainSet[,1],col='red')
summary(model1)
model1

#Kling-Gupta efficiency calculation
#model for validation period
predValid <- predict(model1, ValidSet)

#KGE and RMSE export 
combkge=c(KGE(predTrain, TrainSet[,1]),KGE(predValid, ValidSet[,1]))
comb_rmse=c(rmse(predTrain, TrainSet[,1]),rmse(predValid, ValidSet[,1]))
comb_metrics=rbind(combkge,comb_rmse)
write.table(comb_metrics,paste0(wd$output,'randomForest_perfMetrics_subregion_',sub_select,'_v3.txt'))

#predicting historical values
pred_hist=predict(model1,comb_backup,type='class')

#predicting for future values
pred_fut <- predict(model1, comb2_backup, type = "class")

#exporting to csv file
all_pump=data.frame(years=rep(2004:2018,each=12),mon=rep(1:12,times=(2018-2004+1)),pumping=c(pred_hist,pred_fut))
write.csv(all_pump,paste0(wd$output,'randomForest_subregion_',sub_select,'_v3_upto2018.csv'))
