
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
  library(tidybayes)
  library(brms)
  library(cmdstanr)
  library(phyr)
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
  library(sjPlot)
  library(coda)
  library(progress)
  library(parallel)
  library(foreach)
  library(doParallel)
  library(magicfor)
  library(viridis)
  library(gghighlight)
  library(units)
})

text_size = 11
# --------------------------------------------------------------------------
# PATHS
# --------------------------------------------------------------------------

data_path <- file.path(getwd(), "data")
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
'%!in%' <- function(x,y)!('%in%'(x,y))
stdize = function(x, ...) {(x - min(x, ...)) / (max(x, ...) - min(x, ...))}
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
      tmp <- abs(sum(1 / log(sp)) * log(length(sp)))
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

calculateBioclimate <- function(dataset, var) {
  result = NULL
  for (species in dataset) {
    tmp <- crop(var, species) %>% mask(species)
    dataset <-
      as.data.frame(values(tmp)) %>% drop_na()
    colnames(dataset) <- "species"
    result[[as.character(unique(species$SCINAME))]] <-
      dataset %>% summarise(
        max = max(species),
        min = min(species),
        median = median(species)
      )
  }
  var_name <- var@data@names
  result_var <-
    data.frame(matrix(unlist(result), nrow = length(result), byrow = T))
  colnames(result_var) <- c(paste0(var_name,"_mean"), paste0(var_name,"_sd"),  paste0(var_name,"_max"),  paste0(var_name,"_min"),  paste0(var_name,"_median"))
  rownames(result_var) <- names(result)
  final_result <- result_var %>% rownames_to_column(var = "species_name")
  print(final_result)
}

calculateMedianDist <- function(dataset) {
  median=data.frame(matrix(ncol=1, nrow=1))
  colnames(median)<-"median_dist_poly"
  for (i in dataset) {
    tmp <- i %>%
      st_cast("MULTIPOLYGON") %>%
      st_cast("POLYGON", warn=F) %>%
      st_centroid() %>%
      st_distance() %>%
      median() %>%
      drop_units()
    median <- as.data.frame(rbind(median,tmp))
  }
  
  median <- median[-1,] %>% as.data.frame()
  colnames(median) <- "median_dist_poly"
  median <- cbind(median,names(dataset)) %>%
    mutate(species_name=names(dataset)) %>%
    dplyr::select(median_dist_poly,species_name) %>%
    distinct(species_name, .keep_all = TRUE)
  median
}

calculateMaxDistPoly <- function(dataset) {
  max=data.frame(matrix(ncol=1, nrow=1))
  colnames(max)<-"max_dist_poly"
  for (i in dataset) {
    tmp <- i %>%
      st_cast("MULTIPOLYGON") %>%
      st_cast("POLYGON", warn=F) %>%
      st_centroid() %>%
      st_distance() %>%
      max() %>%
      drop_units()
    max <- as.data.frame(rbind(max,tmp))
  }
  
  max <- max[-1,] %>% as.data.frame()
  colnames(max) <- "max_dist_poly"
  max <- cbind(max,names(dataset)) %>%
    mutate(species_name=names(dataset)) %>%
    dplyr::select(max_dist_poly,species_name) %>%
    distinct(species_name, .keep_all = TRUE)
  max
}

calculateCentrality<- function(dataset) {
  var=NULL
  mean=NULL
  median=NULL
  for (i in dataset$SCINAME) {
    tmp <- dataset %>% filter(SCINAME==i) %>%  
      st_cast("MULTIPOLYGON") %>%  
      st_cast("POLYGON", warn=F) %>% 
      st_centroid() %>% 
      st_distance() %>%
      drop_units() %>% 
      na_if(0) %>%
      analyze.stuff::colMins(na.rm=T) %>% 
      as.numeric() %>% 
      var()
    
    var <- as.data.frame(rbind(var,tmp))
  }
  var_x <- cbind(var,dataset$SCINAME) %>%  
    mutate(variance=V1) %>% 
    mutate(species_name=dataset$SCINAME) %>% 
    dplyr::select(variance,species_name) %>% 
    distinct(species_name)
  
  for (i in dataset$SCINAME) {
    tmp <- dataset %>% filter(SCINAME==i) %>%  
      st_cast("MULTIPOLYGON") %>%  
      st_cast("POLYGON", warn=F) %>% 
      st_centroid() %>% 
      st_distance() %>%
      drop_units() %>% 
      na_if(0) %>%
      analyze.stuff::colMins(na.rm=T) %>% 
      as.numeric() %>% 
      median()
    
    median <- as.data.frame(rbind(median,tmp))
  }
  median_x <- cbind(median,dataset$SCINAME) %>%  
    mutate(median=V1) %>% 
    mutate(species_name=dataset$SCINAME) %>% 
    dplyr::select(median,species_name) %>% 
    distinct(species_name)
  
  for (i in dataset$SCINAME) {
    tmp <- dataset %>% filter(SCINAME==i) %>%  
      st_cast("MULTIPOLYGON") %>%  
      st_cast("POLYGON", warn=F) %>% 
      st_centroid() %>% 
      st_distance() %>%
      drop_units() %>% 
      na_if(0) %>%
      analyze.stuff::colMins(na.rm=T) %>% 
      as.numeric() %>% 
      mean()
    
    mean <- as.data.frame(rbind(mean,tmp))
  }
  mean_x <- cbind(mean,dataset$SCINAME) %>%  
    mutate(mean=V1) %>% 
    mutate(species_name=dataset$SCINAME) %>% 
    dplyr::select(mean,species_name) %>% 
    distinct(species_name)
  
  centrality <- mean_x %>% full_join(median_x, by="species_name") %>% full_join(var_x, by ="species_name")
  return(centrality)
  return(mean)
  return(median)
  return(var)
}

calculateVariance<- function(dataset) {
  var=NULL
  for (i in dataset$SCINAME) {
    tmp <- dataset %>% filter(SCINAME==i) %>%  
                  st_cast("MULTIPOLYGON") %>%  
                  st_cast("POLYGON", warn=F) %>% 
                  st_centroid() %>% 
                  st_distance() %>%
                  drop_units() %>% 
                  na_if(0) %>%
                  colMeans(na.rm=T) %>% 
                  as.numeric() %>% 
                  var()

    var <- as.data.frame(rbind(var,tmp))
  }
  var <- cbind(var,dataset$SCINAME) %>%  
    mutate(variance=V1) %>% 
    mutate(species_name=dataset$SCINAME) %>% 
    dplyr::select(variance,species_name)
  return(var)
}

calculateMean<- function(dataset) {
  mean=NULL
  for (i in dataset$SCINAME) {
    tmp <- dataset %>% filter(SCINAME==i) %>%  
      st_cast("MULTIPOLYGON") %>%  
      st_cast("POLYGON", warn=F) %>% 
      st_centroid() %>% 
      st_distance() %>%
      drop_units() %>% 
      na_if(0) %>%
      colMeans(na.rm=T) %>% 
      as.numeric() %>% 
      mean()
    
    mean <- as.data.frame(rbind(mean,tmp))
  }
  mean <- cbind(mean,dataset$SCINAME) %>%  
    mutate(mean=V1) %>% 
    mutate(species_name=dataset$SCINAME) %>% 
    dplyr::select(mean,species_name) %>% 
    distinct(species_name, .keep_all = TRUE)
  mean
}


calculateMedian <- function(dataset) {
  median=NULL
  for (i in dataset$SCINAME) {
    tmp <- dataset %>% filter(SCINAME==i) %>%  
      st_cast("MULTIPOLYGON") %>%  
      st_cast("POLYGON", warn=F) %>% 
      st_centroid() %>% 
      st_distance() %>%
      drop_units() %>% 
      na_if(0) %>%
      robustbase::colMedians(na.rm=T) %>% 
      as.numeric() %>% 
      median()
    
    median <- as.data.frame(rbind(median,tmp))
  }
  median <- cbind(median,dataset$SCINAME) %>%  
    mutate(median=V1) %>% 
    mutate(species_name=dataset$SCINAME) %>% 
    dplyr::select(median,species_name) %>% 
    distinct(species_name, .keep_all = TRUE)
  median
}

calculateMax <- function(dataset) {
  max=NULL
  for (i in dataset$SCINAME) {
    tmp <- dataset %>% filter(SCINAME==i) %>%  
      st_cast("MULTIPOLYGON") %>%  
      st_cast("POLYGON", warn=F) %>% 
      st_centroid() %>% 
      st_distance() %>%
      max() %>% 
      drop_units() 
    max <- as.data.frame(rbind(max,tmp))
  }
  
  max <- cbind(max,dataset$SCINAME) %>%  
    mutate(max=V1) %>% 
    mutate(species_name=dataset$SCINAME) %>% 
    dplyr::select(max,species_name) %>% 
    distinct(species_name, .keep_all = TRUE)
  max
}

calculateMin <- function(dataset) {
  min=NULL
  for (i in dataset$SCINAME) {
    tmp <- dataset %>% filter(SCINAME==i) %>%
      st_cast("MULTIPOLYGON") %>%
      st_cast("POLYGON", warn=F) %>%
      st_centroid() %>%
      st_distance() %>%
      sd() %>%
      drop_units()
    min <- as.data.frame(rbind(min,tmp))
  }
  
  min <- cbind(min,dataset$SCINAME) %>%
    mutate(min=V1) %>%
    mutate(species_name=dataset$SCINAME) %>%
    dplyr::select(min,species_name) %>%
    distinct(species_name, .keep_all = TRUE)
  min
}


range01 <- function(x){(x-min(x))/(max(x)-min(x))}
stdize = function(x, ...) {(x - min(x, ...)) / (max(x, ...) - min(x, ...))}
