#############################################
############# LOAD SOURCE CODE ##############
#############################################

library("tidyverse")
library("dplyr")
library("sf")
library("rgdal")
library("units")
library("lwgeom")

#############################################
########## LOAD INTERSECTION DATA ###########
#############################################

setwd("~/sjoh4959/projects/0.0_great-speciators/data")

land <- sf::st_read(dsn = "~/sjoh4959/projects/0.0_great-speciators/data/shapefiles/everythingisanisland.shp") %>% 
  mutate(area=st_area(.)/1000000) %>% drop_units() %>% filter(area < 587000)

resident <-
  sf::st_read(dsn = "~/sjoh4959/projects/0.0_great-speciators/data/shapefiles/All_Species_Resident.shp")
breeding <-
  sf::st_read(dsn = "~/sjoh4959/projects/0.0_great-speciators/data/shapefiles/All_Species_Breeding.shp")

calculateNpolygon <- function(dataset, landtype) {
  out <- list()
  for (i in dataset$SCINAME) {
    out[[i]] <- dataset %>% filter(SCINAME==i) %>% 
      st_buffer(dist = 0.0001, nQuadSegs=1) %>% 
      st_intersection(landtype, proj4string = "+init=epsg:4326") %>%
      st_cast("MULTIPOLYGON") %>%  st_cast("POLYGON", warn=F) %>% 
      mutate(npoly=length(geometry))
  }
  out
}

resident_npoly <- calculateNpolygon(resident, land)
y = NULL
for (i in resident_npoly) {
    tmp <- i %>% dplyr::select(SCINAME, npoly) %>%
      as.data.frame() %>%
      distinct(SCINAME, .keep_all = TRUE) %>%
      dplyr::select(-geometry)
    y <- rbind(y, tmp)
    resident_npoly <- y %>% rename(species_name = SCINAME)
    resident_npoly
  }

save(resident_npoly, file="~/sjoh4959/projects/0.0_great-speciators/data/npoly/world_npoly_resident.RData")

breeding_npoly <- calculateNpolygon(breeding, land)
y = NULL
for (i in breeding_npoly) {
    tmp <- i %>% dplyr::select(SCINAME, npoly) %>%
      as.data.frame() %>%
      distinct(SCINAME, .keep_all = TRUE) %>%
      dplyr::select(-geometry)
    y <- rbind(y, tmp)
    breeding_npoly <- y %>% rename(species_name = SCINAME)
    breeding_npoly
}


npoly <- full_join(breeding_npoly, resident_npoly, by="species_name") %>% mutate(npoly=rowSums(cbind(npoly.x, npoly.y), na.rm = TRUE)) %>% dplyr::select(species_name, npoly)

save(resident_npoly, file="~/sjoh4959/projects/0.0_great-speciators/data/npoly/world_npoly_resident.RData")
save(breeding_npoly, file="~/sjoh4959/projects/0.0_great-speciators/data/npoly/world_npoly_breeding.RData")

write.csv(npoly, "~/sjoh4959/projects/0.0_great-speciators/data/dfs/npoly_world.csv", row.names = F)
