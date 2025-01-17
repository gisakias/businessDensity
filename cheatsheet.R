'
getwd()
setwd("setwd("/media/geolinux/TECHSMITH/ΓΕΩΑΝΑ/businessDensity")")
'
library(remotes)
library(ggplot2)
library(curl)
library(dplyr)
library(sf)
library(terra)
library(devtools)
#devtools::install_github("dimitrisk/goal", quiet=T)
library(goal)
library(osmdata)
#remotes::install_github('ropensci/osmdata')
library(raster)
library(terra)
library(knitr)
library(ggplot2)
opts_chunk$set(cache=TRUE)
options(scipen=999)
#1
acto = sf::read_sf("data/Greece_wgs84.shp")  %>% st_transform("EPSG:4326")
#2
bb_mytilini = c(26.5392, 39.0806, 26.5689, 39.123)
bb_chios =  c(26.1092, 38.333, 26.1452, 38.4268)
bb_rodos =  c(28.193, 36.4093, 28.2441, 36.4617)
bb_chania = c(23.9639, 35.4894, 24.0611, 35.5322)
bb_patra =  c(21.6662, 38.1726, 21.7988, 38.2858)
bb_larisa = c(22.3707, 39.5915, 22.4737, 39.6595)

bb1 = bb_larisa
bb2 = bb_chania

#3
size = 0.002

#4
shops_all = osm.getPOI_usingbb(bb1, inkey ="shop" )
shops = osm.combineShops(shops_all)
shops

#5
mypol_bb = osm.osmdata_result_2_bbox_pol(shops_all) %>% st_transform("EPSG:4326") # get polygon of this bounding box.
pol = sf::st_intersection( mypol_bb, acto )  

acto = sf::read_sf("data/Greece_wgs84.shp")  %>% st_transform("EPSG:4326")

bb_mytilini = c(26.5392, 39.0806, 26.5689, 39.123)
bb_chios =  c(26.1092, 38.333, 26.1452, 38.4268)
bb_rodos =  c(28.193, 36.4093, 28.2441, 36.4617)
bb_chania = c(23.9639, 35.4894, 24.0611, 35.5322)
bb_patra =  c(21.6662, 38.1726, 21.7988, 38.2858)
bb_larisa = c(22.3707, 39.5915, 22.4737, 39.6595)

bb1 = bb_larisa
bb2 = bb_chania

size = 0.002

shops_all = osm.getPOI_usingbb(bb1, inkey ="shop" )
shops = osm.combineShops(shops_all)
shops

mypol_bb = osm.osmdata_result_2_bbox_pol(shops_all) %>% st_transform("EPSG:4326") # get polygon of this bounding box.
pol = sf::st_intersection( mypol_bb, acto )

plot(st_geometry(pol))
plot(st_geometry(shops), add=T)

r = rast( vect(pol), res=size ) %>% raster()
r

r = projectRaster(r, crs = "EPSG:2100")
shops = shops %>% st_transform("EPSG:2100")
pol= pol %>% st_transform("EPSG:2100")

freq1 = osm.getFrequency(shops, inword = "shop", removeNA = F)
freq1

print(freq1, n=30) # εκτυπωση 30 γραμμών

emporikes_epixeiriseis = c("clothes","supermarket","bakery","mobile_phone","computer","convenience","hairdresser","jewelry","shoes","florist","electronics","coffee")
shops_ena = shops[shops$shop %in% emporikes_epixeiriseis,]

table(shops_ena$shop) %>% as.data.frame() %>% arrange(desc(Freq))

nrow(shops_ena)

density1 = raster::rasterize(shops_ena, r, field=1, fun=sum) # sum
#presense1 = raster::rasterize(shops_ena, r, field=1) # presense was a comment

myDist = function(inPoints, selecttionColumn="", SelectionValue=""){
  if(nchar(selecttionColumn)>1){
    myFiltered = inPoints %>% filter(.data[[selecttionColumn]] == SelectionValue)
  }else{
    myFiltered = inPoints
  }
  jim = st_distance(myFiltered) %>% units::drop_units()  %>% as.matrix()
  jim[lower.tri(jim,diag=T)] = NA
  return(jim)
}

myDist(shops_ena )%>% as.vector( ) %>% summary()

myDist(shops_ena, selecttionColumn="shop", SelectionValue="supermarket") %>% as.vector( ) %>% summary()

plot(st_geometry(pol))
plot(st_geometry(shops_ena), add=T )
plot(density1, add=T)

table(density1[])

data_hist = as.data.frame(density1)
data_hist$Perioxi = "Larisa"

p = data_hist %>%
  ggplot( aes(x=layer, fill=Perioxi)) +
  ggtitle("Ιστόγραμμα συχνοτήτων (πλήθος επιχειρήσεων ανά κελί)")+
  geom_histogram(  alpha=0.6, position = 'identity') +
  xlab("Εμπορικές επιχειρήσεις")+ylab("Συχνότητα")+ xlim(0,50)

p

CellArea = res(density1) %>% prod() # Εναλλακτικός τρόπος υπολογισμού του cell size (m)
sqrt(CellArea) # μέγεθος πλευράς κελιού

length(density1[density1>0])

density1[density1>0]

length(density1[density1>0]) * CellArea

shops_all = osm.getPOI_usingbb(bb2, inkey ="shop" )
shops_chania = osm.combineShops(shops_all)

mypol_bb = osm.osmdata_result_2_bbox_pol(shops_all) %>% st_transform("EPSG:4326") # πολύγωνο Χανίων
pol_chania = sf::st_intersection( mypol_bb, acto )

r_chania = rast( vect(pol_chania), res=size ) %>% raster() # Άδειο raster για χρήση στη συνέχεια.

r_chania = projectRaster(r_chania, crs = "EPSG:2100")
shops_chania = shops_chania %>% st_transform("EPSG:2100")
pol_chania = pol_chania %>% st_transform("EPSG:2100")

#par(mar = rep(0, 4)) # Remove all margins
plot(st_geometry(pol_chania))
plot(st_geometry(shops_chania), add=T)

freq2 = osm.getFrequency(shops_chania, inword = "shop", removeNA = F)
freq2

print(freq2, n=30) # εκτύπωση 30 γραμμών

shops_duo = shops_chania[shops_chania$shop %in% emporikes_epixeiriseis,]

nrow(shops_duo)

myDist(shops_duo )%>% as.vector( ) %>% summary()

density2 = raster::rasterize(shops_duo, r_chania, field=1, fun=sum) # sum

CellArea2 = res(density2) %>% prod() # Εναλλακτικός τρόπος υπολογισμού του cell size (m)
sqrt(CellArea2) # μέγεθος πλευράς κελιού

length(density2[density2>0])

density2[density2>0]

length(density2[density2>0]) * CellArea2

plot(st_geometry(pol_chania))
plot(st_geometry(shops_duo), add=T, cex=0.5 )
plot(density2, add=T)

table(density2[])

hist(density2, xlim=c(0,50), main="Ιστόγραμμα συχνοτήτων (Χανιά)\n(πλήθος επιχειρήσεων ανά κελί)", ylab="Συχνότητα", xlab="Επιχειρήσεις")

data_hist2 = as.data.frame(density2) # Χανιά
data_hist2$Perioxi = "Chania"

data_all = rbind(data_hist, data_hist2) %>% as_tibble()
head(data_all)

data_all %>% group_by(Perioxi) %>%
  summarise(kelia_plithosa=n(),
            MO=mean(layer, na.rm=T),
            kelia_more_1 = sum( layer>1, na.rm=T ) ,
            kelia_more_2 = sum( layer>2, na.rm=T ) ,
            kelia_more_3 = sum( layer>3, na.rm=T )
  )

table(data_all$Perioxi)

p = data_all %>%
  ggplot( aes(x=layer, fill=Perioxi)) +
  geom_histogram(  alpha=0.8, position = 'identity') +
  xlim(0,30)
p

library(ggpubr)
gghistogram(
  data_all, x = "layer", alpha=0.6,
  add = "mean", rug = TRUE,
  fill = "Perioxi", palette = c("blue", "green"), add_density = TRUE
)














