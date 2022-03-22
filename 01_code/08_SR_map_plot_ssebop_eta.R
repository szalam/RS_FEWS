rm(list=ls())

#library. few of the libraries are not used in the code, but was used at some initial experiments
library(gtools)
library(raster)
library(rgdal)
library(plyr)
library(maptools)
library(ggplot2)
library(maps)
library(gridExtra)
library(wesanderson)
library(ggthemes)
library(stringr)
library(dplyr)
library(ggpubr)
library(RColorBrewer)

#directory
wd=list()
wd$main='F:/RS_FEWS'
wd$data='F:/RS_FEWS/02_data/'
wd$figure = 'F:/RS_FEWS/05_figure/'
setwd(wd$main)

#list files
files_data=list.files(wd$data)
files_data=c("et-SSEBOp.csv")

#preprocess
p=stor_changes_from_base=gw_frac_stor_entire_cv=list()
for (gk in 1:length(files_data)) {
  rm(list= ls()[!(ls() %in% c('wd','per_type','files_data','gk','p','stor_changes_from_base','gw_frac_stor_entire_cv'))])

  per_type=gk+2
  data_read=read.csv(paste0(wd$data,'/',files_data[gk]),header=T,fill = T)
  
  dir_shp = paste0(wd$main,'03_shape/')
  
  # Reading subregion shapefiles and storing in variable ----- 
  shape_file=mixedsort(list.files(path=dir_shp,pattern=glob2rx('*subregion*.shp')))
  shape_file
  shape_read=list()
  shape_shift=list()
  
  load(paste0(dir_shp,'shapefiles.rda'))
  
  gg=list()
  gg=shape_shift
  
  tt=gg[[1]]
  for (i in 2:length(gg)) {
    tt=rbind(tt,gg[[i]])
  }
  plot(tt)
  
  # temp=sample(1:1000, 21, replace=TRUE)
  data_read=data_read[data_read$Year>2006 & data_read$Year<2019,]
  data2=aggregate(data_read[,4:24],by=list(data_read$Year),FUN=sum)
  data3=colMeans(data2)
  
  # temp=data_read[1:nrow(data_read),per_type]
  mm=tt
  mm$temp=c(data3[2:length(data3)])
  temp=data3[2:length(data3)]
  
  mm$temp
  kk='S1'
  for (i in 2:21) {
   kk=c(kk,paste0('S',i)) 
  }
  
  mm$region=kk
  map=fortify(mm,Region='region')
  ditch_the_axes <- theme(
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.title = element_blank()
  )

states <- map_data("state")
ca_df <- subset(states, region == "california")
counties <- map_data("county")
ca_county <- subset(counties, region == "california")
ca_base =
  ggplot(data = ca_df, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color = "black", fill = "white")+theme_bw()

ca_base2= 
ggplot(data = ca_df, mapping = aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3) 


ca_base + theme_bw()
ca_base + theme_bw() + 
  geom_polygon(data = ca_county, fill = NA, color = "white") +
  geom_polygon(color = "black", fill = NA)  # get the state border back on top


temp2=data.frame(cbind(c(temp),mm$SubRegion_))
# temp2[,1]=as.numeric(temp2[,1])
colnames(temp2)=c('value','id')
head(temp2)
stor_changes_from_base[[gk]]=temp2
nn=fortify(mm,Region='SubRegion_')
head(nn)
nn$id=as.numeric(nn$id)+1
temp2$id=unlist(temp2$id)
cacopa <- inner_join(nn, temp2, by = "id")
cacopa$value=unlist(cacopa$value)

ca_base +
  geom_polygon(data = cacopa, aes(fill=value),  color = "white")+
  geom_polygon(color = "black", fill = NA) +
  theme_bw()+ #scale_fill_gradient2_tableau(palette = "Orange-Blue Diverging")
  #scale_fill_brewer(palette="Dark2")
  scale_fill_gradient2_tableau(palette = "Orange-Blue Diverging")+
  # scale_fill_gradient2(high="firebrick" )
  theme(text=element_text(size=14,color='black'),legend.position = ('right'))+
  xlab("Lon")+ylab("Lat")+
  theme(axis.text.x = element_text(size = 13, color="black"),
        axis.text.y = element_text(size = 13,color="black"))

ewbrks <- seq(-124,-119,2)
nsbrks <- seq(35,41,2)
ewlbls <- unlist(lapply(ewbrks, function(x) ifelse(x < 0, paste0(-x, "°W"), ifelse(x > 0, paste0(-x, "°W"),x))))
nslbls <- unlist(lapply(nsbrks, function(x) ifelse(x < 0, paste0(x, "°N"), ifelse(x > 0, paste0(x, "°N"),x))))


cv_base =
  ggplot(data = cacopa, mapping = aes(x = long, y = lat, group = group)) +
  coord_fixed(1.2)

  # colors= rev(brewer.pal(20,"RdYlGn"))
  # colors= rev(brewer.pal(10,"RdYlGn"))
  colors= (brewer.pal(10,"YlOrRd"))

  p[[gk]]=
    cv_base +
    geom_polygon(data = cacopa, aes(fill=value),  color = "black")+
    theme_classic()+ #scale_fill_gradient2_tableau(palette = "Orange-Blue Diverging")
    theme(text=element_text(size=25,color='black'),legend.position = ('right'))+
    # xlab("Lon")+ylab("Lat")+
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank())+
    theme(axis.text.x = element_text(size = 19, color="black"),
          axis.text.y = element_text(size = 19,color="black"))+
    theme(legend.key.height =  unit(1.7,"cm"))+
    theme(legend.key.width =  unit(.4,"cm"))+
    guides(guide_legend(title='GW Change (km3)'))+
    scale_x_continuous(breaks = ewbrks, labels = ewlbls)+
    scale_y_continuous(breaks = nsbrks, labels = nslbls)+
    labs(fill = expression(paste("ETa ",'(mm/year)')),size=4)+
      scale_fill_gradient2(midpoint=500, low="red", mid="white",
                            high="skyblue", space ="Lab" ,breaks=c(0,200,400,600,800,1000),
                                                limits = c(0, 1000))
   p[[gk]]
}

p[[1]]
#export figure
png(paste0(wd$figure,'eta_ssbbop_map.png'),width = 400,height=800,units = 'px')
ggarrange(p[[1]],
          ncol = 1, nrow = 1)
dev.off()
