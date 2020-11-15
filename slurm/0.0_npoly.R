#############################################
############# LOAD SOURCE CODE ##############
#############################################

library("tidyverse")
library("dplyr")
library("sf")
library("rgdal")
library("lwgeom")

#############################################
########## LOAD INTERSECTION DATA ###########
#############################################

setwd("/data/zool-zost/sjoh4959/projects/newproject/data")

melanesia <- sf::st_read(dsn = "/data/zool-zost/sjoh4959/projects/newproject/data/everythingisanisland.shp")
All_Resident <- sf::st_read(dsn = "/data/zool-zost/sjoh4959/projects/newproject/data/All_Species_Breeding.shp")


#############################################
########## CALCULATE INTERSECTION DATA ######
#############################################

calculateNpolygon <- function(dataset, landtype) {
  out <- list()
  for (i in dataset$SCINAME) {
    out[[i]] <- dataset %>% filter(SCINAME==i) %>% 
      st_buffer(dist = 0.0001, nQuadSegs=1, warn=F) %>% 
      st_intersection(landtype, proj4string = "+init=epsg:4326", warn=F) %>%
      st_cast("MULTIPOLYGON") %>%  st_cast("POLYGON", warn=F) %>% 
      mutate(npoly=length(geometry))
  }
  out
}


t <- calculateNpolygon(All_Resident, melanesia)

y=NULL
for (i in t) {
        tmp <- i %>% dplyr::select(SCINAME, npoly) %>%
        as.data.frame() %>%
        distinct(SCINAME, .keep_all = TRUE) %>% 
        dplyr::select(-geometry)
        y <- rbind(y,tmp)
	y
}

save(y, file="world_npoly_breeding.RData")
