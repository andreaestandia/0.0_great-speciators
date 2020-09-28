#############################################
############# LOAD SOURCE CODE ##############
#############################################

knitr::opts_knit$set(root.dir = rprojroot::find_rstudio_root_file())
knitr::opts_chunk$set(echo = TRUE, eval = FALSE)
options(scipen = 999)
source("~/data/zool-zost/sjoh4959/projects/0.0_great-speciators/src/0.0_great-speciators_source.R")

#############################################
########## LOAD INTERSECTION DATA ###########
#############################################

load("~/data/zool-zost/sjoh4959/projects/0.0_great-speciators/data/intersection2.RData")
sp0 <- sf::st_read(dsn = "~/data/zool-zost/sjoh4959/projects/0.0_great-speciators/data/All_Species_Resident_1.2.shp")
land <- sf::st_read(dsn = "~/data/zool-zost/sjoh4959/projects/0.0_great-speciators/data/everythingisanisland.shp")

sp0intersection <- calculateArea(sp0, land)

#############################################
########## CALCULATE INTERSECTION DATA ######
#############################################

resident_distr <- data.frame()
for (i in list(c(sp0intersection, sp1intersection, sp1.1intersection, sp1.2intersection, sp1.3intersection, sp1.4intersection))) {
  resident_distr <- rbind(calculateDistribution(i))
}

save(resident_distr, file="distr_index.RData")
