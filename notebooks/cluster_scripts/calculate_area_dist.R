#############################################
############# LOAD SOURCE CODE ##############
#############################################

knitr::opts_knit$set(root.dir = rprojroot::find_rstudio_root_file())
knitr::opts_chunk$set(echo = TRUE, eval = FALSE)
options(scipen = 999)
source("/data/zool-zost/sjoh4959/projects/0.0_great-speciators/src/0.0_great-speciators_source.R")

#############################################
########## LOAD INTERSECTION DATA ###########
#############################################

load(file.path(data_path,"intersection.RData"))
load(file.path(data_path,"intersection2.RData"))


#############################################
########## CALCULATE INTERSECTION DATA ######
#############################################

resident_distr <- data.frame()
for (i in list(c(sp0intersection, sp1intersection, sp1.1intersection, sp1.2intersection, sp1.3intersection, sp1.4intersection))) {
  resident_distr <- rbind(calculateDistribution(i))
}

save(resident_distr, file=file.path(data_path,"distr_index.RData"))
