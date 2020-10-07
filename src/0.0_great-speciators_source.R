
# 0.0_candidate-gene-analysis_source.R
#
# This script provides the source code necessary to 
# check whether recorders are affecting egg laying 
# and make plots to this effect
# The functions below are very specific to this task and
# are very 'dirty'.
# 
# Copyright (c) Andrea Estandia, 2020, except where indicated
# Date Created: 2020-04-30


# --------------------------------------------------------------------------
# REQUIRES
# --------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(tidyverse)
  library(brms)
  library(modelr)
  library(scales)
  library(devtools)
  library(janitor)
  library(lme4)
  library(ggrepel)
  library(RColorBrewer)
  library(patchwork)
  library(knitr)
  library(cowplot)
  library(genepop)
  library(dplyr)
  library(ggsci)
  library(patchwork)
  library(clickR)
  library(gginnards)
  library(ggforce)
  library(wesanderson)
  library(stringr)
  library(ghibli)
  library(ggannotate)
  library(BAMMtools)
  library(sf)
  library(rgdal)
  library(foreach)
  library(XML)
  library(letsR)
  library(coda)
  library(progress)
  library(parallel)
  library(foreach)
  library(doParallel)
  library(magicfor)
})


# --------------------------------------------------------------------------
# PATHS
# --------------------------------------------------------------------------

data_path <- file.path(getwd(), "data", "raw")
figures_path <- file.path(getwd(), "reports", "plots")

if (!dir.exists(data_path)) {
  dir.create(data_path, recursive = TRUE)
}

if (!dir.exists(figures_path)) {
  dir.create(figures_path, recursive = TRUE)
}

# --------------------------------------------------------------------------
# FUNCTIONS
# --------------------------------------------------------------------------
#calculateArea goes through every species in a shapefile and intersects their distribution with the
#chosen land (everythingisanisland.shp corresponds to every closed polygon on earth, southpacific to the southpacific). It calculates the area of distribution
#within each polygon and returns this. If you run sum(df$area) you can get the total area of distribution of that species.
#If you do length(df$anycolumn) you'd get the number of polygons where the species occurs 
calculateArea <- function(dataset, landtype) {
    out <- list()
    for (i in dataset$SCINAME) {
        out[[i]] <- dataset %>% filter(SCINAME==i) %>% 
        st_buffer(dist = 0.0001, nQuadSegs=1) %>% 
        st_intersection(landtype, proj4string = "+init=epsg:4326") %>% 
        mutate(area=st_area(.)/1000000)
    }
    out
}

#calculateDistribution will calculate the distribution index and it will get ready a dataframe 
#to join the main subset 
calculateDistribution <- function(file) {
  list_area <- purrr::map(file, "area")
  #Remove names of species with value 0 (i.e. that have no intersection with the map provided)
  list_area <- list_area[lapply(list_area, length) > 0]
  #Calculate index
  y = NULL
  for (sp in list_area) {
      tmp <- sum(1 / sp) * length(sp)
      y <- rbind(y, tmp)
  }
  y <- as.data.frame(y)
  names <- as.data.frame(names(list_area))
  x <- cbind(y, names)
  colnames(x) <- c("distr_index", "species_name")
  rownames(x) <- seq(1:length(x$distr_index))
  x <- x %>% unique() %>% arrange(desc(distr_index))
  x
}

range01 <- function(x, ...){(x - min(x, ...)) / (max(x, ...) - min(x, ...))}
