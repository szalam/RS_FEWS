#===============================================================
#Extract ETa for specific crops
#===============================================================
rm(list = ls())

#library
library(raster)
library(rgdal)

#working directory
wd = list()
wd$main = 'F:/RS_FEWS/'
wd$shape = 'F:/RS_FEWS/03_shape/'

#read data
load(paste0(wd$shape,'subregion_shape.rda'))
bb=raster('m200301_modisSSEBopETv4_actual_mm.tif') #data not in github
crp=raster('CDL_2018_clip_20190413023300_72247386.tif') #data not in github

fineres <- res(crp)[1] # fine resolution (arc-seconds - "fine")
# coarseres <- res(bb)  #1/16 # coarse resolution (degrees - "coarse")

# coordinate of the ETa
coord1=crs(bb)
# coordinate of the cropscape
coord2=crs(crp)

# masking the ETa file with the shapefile
temp=spTransform(shape_shift[[13]],coord1)
temp2=crop(bb,temp)
temp3=mask(temp2,temp)
ETa_data=projectRaster(temp3,crs=coord2) #
coarseres <- res(ETa_data)
par(mfrow=c(1,2))
plot(ETa_data)
plot(temp3)
# masking the cropscape with shapefile
temp=spTransform(shape_shift[[13]],coord2)
temp2=crop(crp,temp)
cropScape_data=mask(temp2,temp)

r2 <- rasterToPolygons(ETa_data)
plot(r2)

land.cover <-cropScape_data#raster("land_cover_map.tif")

#make copy
land.cover.crop <- land.cover

r2.dat <- extract(land.cover.crop, r2)

l=g=0
for (i in 1:length(r2.dat)) {
  l=c(l,length(which(r2.dat[[i]]==2))) #almond 75, cotton 2
  g=c(g,length(r2.dat[[i]]))
}
l=l[-1]
g=g[-1]
frac=l/g*100

id=which(frac>=20)
length(id)

plot(ETa_data)

for (i in id) {
  plot(r2[i,],add=T)
}

# extract eta for selected pixels
eta_crop=0
eta_crop=extract(ETa_data,r2[id,])
gk=0
for(i in length(eta_crop))
{
  gk=c(gk,eta_crop[[i]])
}
gk=gk[-1]
mean(gk)
