
library("tidyverse")
library("dplyr")
library("plyr")
library("sf")
library("rgdal")
library("units")
library("lwgeom")
library("raster")

setwd("/data/zool-zost/sjoh4959/projects/newproject/data")

load("resident_npoly.RData")

bio <- getData("worldclim", var = "bio", res = 2.5, path="/home/zoo/sjoh4959/Documents/projects/0.0_great-speciators/data/biovariables")
alt <- getData("worldclim", var = "alt", res = 2.5)

resident_npoly_complete <- list()
for (i in resident_npoly){
  if(length(i$OBJECTID)>0){
    resident_npoly_complete[[as.character(unique(i$SCINAME))]] <- i
  }
}

result = NULL
for (species in resident_npoly_complete) {
  tmp <- crop(bio$bio1, species) %>% mask(species)
  dataset <-
    as.data.frame(values(tmp)) %>% drop_na()
  colnames(dataset) <- "species"
  result[[as.character(unique(species$SCINAME))]] <-
    dataset %>% summarise(
      ann_mean_temp_mean = mean(species),
      ann_mean_temp_sd = sd(species),
      ann_mean_temp_max = max(species),
      ann_mean_temp_min = min(species),
      ann_mean_temp_median = median(species)
    )
}

bio1 <-
  data.frame(matrix(unlist(result), nrow = length(result), byrow = T))
colnames(bio1) <- colnames(result$`Dulus dominicus`)
rownames(bio1) <- names(result)
bio1 <- bio1 %>% rownames_to_column(var = "species_name")

result = NULL
for (species in resident_npoly_complete) {
  tmp <- crop(bio$bio2, species) %>% mask(species)
  dataset <-
    as.data.frame(values(tmp)) %>% drop_na()
  colnames(dataset) <- "species"
  result[[as.character(unique(species$SCINAME))]] <-
    dataset %>% summarise(
      mean_diurnal_range_mean = mean(species),
      mean_diurnal_range_sd = sd(species),
      mean_diurnal_range_max = max(species),
      mean_diurnal_range_min = min(species),
      mean_diurnal_range_median = median(species)
    )
}

bio2 <-
  data.frame(matrix(unlist(result), nrow = length(result), byrow = T))
colnames(bio2) <- colnames(result$`Dulus dominicus`)
rownames(bio2) <- names(result)
bio2 <- bio2 %>% rownames_to_column(var = "species_name")

result = NULL
for (species in resident_npoly_complete) {
  tmp <- crop(bio$bio3, species) %>% mask(species)
  dataset <-
    as.data.frame(values(tmp)) %>% drop_na() 
  colnames(dataset) <- "species"
  result[[as.character(unique(species$SCINAME))]] <-
    dataset %>% summarise(
      isothermality_mean = mean(species),
      isothermality_sd = sd(species),
      isothermality_max = max(species),
      isothermality_min = min(species),
      isothermality_median = median(species)
    )
}

bio3 <-
  data.frame(matrix(unlist(result), nrow = length(result), byrow = T))
colnames(bio3) <- colnames(result$`Dulus dominicus`)
rownames(bio3) <- names(result)
bio3 <- bio3 %>% rownames_to_column(var = "species_name")

result = NULL
for (species in resident_npoly_complete) {
  tmp <- crop(bio$bio4, species) %>% mask(species)
  dataset <-
    as.data.frame(values(tmp)) %>% drop_na() 
  colnames(dataset) <- "species"
  result[[as.character(unique(species$SCINAME))]] <-
    dataset %>% summarise(
      temp_seasonality_mean = mean(species),
      temp_seasonality_sd = sd(species),
      temp_seasonality_max = max(species),
      temp_seasonality_min = min(species),
      temp_seasonality_median = median(species)
    )
}

bio4 <-
  data.frame(matrix(unlist(result), nrow = length(result), byrow = T))
colnames(bio4) <- colnames(result$`Dulus dominicus`)
rownames(bio4) <- names(result)
bio4 <- bio4 %>% rownames_to_column(var = "species_name")

result = NULL
for (species in resident_npoly_complete) {
  tmp <- crop(bio$bio5, species) %>% mask(species)
  dataset <-
    as.data.frame(values(tmp)) %>% drop_na() 
  colnames(dataset) <- "species"
  result[[as.character(unique(species$SCINAME))]] <-
    dataset %>% summarise(
      max_temp_warm_mean = mean(species),
      max_temp_warm_sd = sd(species),
      max_temp_warm_max = max(species),
      max_temp_warm_min = min(species),
      max_temp_warm_median = median(species)
    )
}

bio5 <-
  data.frame(matrix(unlist(result), nrow = length(result), byrow = T))
colnames(bio5) <- colnames(result$`Dulus dominicus`)
rownames(bio5) <- names(result)
bio5 <- bio5 %>% rownames_to_column(var = "species_name")

result = NULL
for (species in resident_npoly_complete) {
  tmp <- crop(bio$bio6, species) %>% mask(species)
  dataset <-
    as.data.frame(values(tmp)) %>% drop_na() 
  colnames(dataset) <- "species"
  result[[as.character(unique(species$SCINAME))]] <-
    dataset %>% summarise(
      min_temp_cold_mean = mean(species),
      min_temp_cold_sd = sd(species),
      min_temp_cold_max = max(species),
      min_temp_cold_min = min(species),
      min_temp_cold_median = median(species)
    )
}

bio6 <-
  data.frame(matrix(unlist(result), nrow = length(result), byrow = T))
colnames(bio6) <- colnames(result$`Dulus dominicus`)
rownames(bio6) <- names(result)
bio6 <- bio6 %>% rownames_to_column(var = "species_name")

result = NULL
for (species in resident_npoly_complete) {
  tmp <- crop(bio$bio7, species) %>% mask(species)
  dataset <-
    as.data.frame(values(tmp)) %>% drop_na() 
  colnames(dataset) <- "species"
  result[[as.character(unique(species$SCINAME))]] <-
    dataset %>% summarise(
      temp_annual_range_mean = mean(species),
      temp_annual_range_sd = sd(species),
      temp_annual_range_max = max(species),
      temp_annual_range_min = min(species),
      temp_annual_range_median = median(species)
    )
}

bio7 <-
  data.frame(matrix(unlist(result), nrow = length(result), byrow = T))
colnames(bio7) <- colnames(result$`Dulus dominicus`)
rownames(bio7) <- names(result)
bio7 <- bio7 %>% rownames_to_column(var = "species_name")

result = NULL
for (species in resident_npoly_complete) {
  tmp <- crop(bio$bio8, species) %>% mask(species)
  dataset <-
    as.data.frame(values(tmp)) %>% drop_na() 
  colnames(dataset) <- "species"
  result[[as.character(unique(species$SCINAME))]] <-
    dataset %>% summarise(
      mean_temp_wet_mean = mean(species),
      mean_temp_wet_sd = sd(species),
      mean_temp_wet_max = max(species),
      mean_temp_wet_min = min(species),
      mean_temp_wet_median = median(species)
    )
}

bio8 <-
  data.frame(matrix(unlist(result), nrow = length(result), byrow = T))
colnames(bio8) <- colnames(result$`Dulus dominicus`)
rownames(bio8) <- names(result)
bio8 <- bio8 %>% rownames_to_column(var = "species_name")

result = NULL
for (species in resident_npoly_complete) {
  tmp <- crop(bio$bio9, species) %>% mask(species)
  dataset <-
    as.data.frame(values(tmp)) %>% drop_na() 
  colnames(dataset) <- "species"
  result[[as.character(unique(species$SCINAME))]] <-
    dataset %>% summarise(
      mean_temp_dry_mean = mean(species),
      mean_temp_dry_sd = sd(species),
      mean_temp_dry_max = max(species),
      mean_temp_dry_min = min(species),
      mean_temp_dry_median = median(species)
    )
}

bio9 <-
  data.frame(matrix(unlist(result), nrow = length(result), byrow = T))
colnames(bio9) <- colnames(result$`Dulus dominicus`)
rownames(bio9) <- names(result)
bio9 <- bio9 %>% rownames_to_column(var = "species_name")

result = NULL
for (species in resident_npoly_complete) {
  tmp <- crop(bio$bio10, species) %>% mask(species)
  dataset <-
    as.data.frame(values(tmp)) %>% drop_na() 
  colnames(dataset) <- "species"
  result[[as.character(unique(species$SCINAME))]] <-
    dataset %>% summarise(
      mean_temp_warm_mean = mean(species),
      mean_temp_warm_sd = sd(species),
      mean_temp_warm_max = max(species),
      mean_temp_warm_min = min(species),
      mean_temp_warm_median = median(species)
    )
}

bio10 <-
  data.frame(matrix(unlist(result), nrow = length(result), byrow = T))
colnames(bio10) <- colnames(result$`Dulus dominicus`)
rownames(bio10) <- names(result)
bio10 <- bio10 %>% rownames_to_column(var = "species_name")

result = NULL
for (species in resident_npoly_complete) {
  tmp <- crop(bio$bio11, species) %>% mask(species)
  dataset <-
    as.data.frame(values(tmp)) %>% drop_na()
  colnames(dataset) <- "species"
  result[[as.character(unique(species$SCINAME))]] <-
    dataset %>% summarise(
      mean_temp_cold_mean = mean(species),
      mean_temp_cold_sd = sd(species),
      mean_temp_cold_max = max(species),
      mean_temp_cold_min = min(species),
      mean_temp_cold_median = median(species)
    )
}

bio11 <-
  data.frame(matrix(unlist(result), nrow = length(result), byrow = T))
colnames(bio11) <- colnames(result$`Dulus dominicus`)
rownames(bio11) <- names(result)
bio11 <- bio11 %>% rownames_to_column(var = "species_name")

result = NULL
for (species in resident_npoly_complete) {
  tmp <- crop(bio$bio12, species) %>% mask(species)
  dataset <-
    as.data.frame(values(tmp)) %>% drop_na() 
  colnames(dataset) <- "species"
  result[[as.character(unique(species$SCINAME))]] <-
    dataset %>% summarise(
      ann_prec_mean = mean(species),
      ann_prec_sd = sd(species),
      ann_prec_max = max(species),
      ann_prec_min = min(species),
      ann_prec_median = median(species)
    )
}

bio12 <-
  data.frame(matrix(unlist(result), nrow = length(result), byrow = T))
colnames(bio12) <- colnames(result$`Dulus dominicus`)
rownames(bio12) <- names(result)
bio12 <- bio12 %>% rownames_to_column(var = "species_name")

result = NULL
for (species in resident_npoly_complete) {
  tmp <- crop(bio$bio13, species) %>% mask(species)
  dataset <-
    as.data.frame(values(tmp)) %>% drop_na() 
  colnames(dataset) <- "species"
  result[[as.character(unique(species$SCINAME))]] <-
    dataset %>% summarise(
      prec_wet_mean = mean(species),
      prec_wet_sd = sd(species),
      prec_wet_max = max(species),
      prec_wet_min = min(species),
      prec_wet_median = median(species)
    )
}

bio13 <-
  data.frame(matrix(unlist(result), nrow = length(result), byrow = T))
colnames(bio13) <- colnames(result$`Dulus dominicus`)
rownames(bio13) <- names(result)
bio13 <- bio13 %>% rownames_to_column(var = "species_name")

result = NULL
for (species in resident_npoly_complete) {
  tmp <- crop(bio$bio14, species) %>% mask(species)
  dataset <-
    as.data.frame(values(tmp)) %>% drop_na() 
  colnames(dataset) <- "species"
  result[[as.character(unique(species$SCINAME))]] <-
    dataset %>% summarise(
      prec_dry_mean = mean(species),
      prec_dry_sd = sd(species),
      prec_dry_max = max(species),
      prec_dry_min = min(species),
      prec_dry_median = median(species)
    )
}

bio14 <-
  data.frame(matrix(unlist(result), nrow = length(result), byrow = T))
colnames(bio14) <- colnames(result$`Dulus dominicus`)
rownames(bio14) <- names(result)
bio14 <- bio14 %>% rownames_to_column(var = "species_name")

result = NULL
for (species in resident_npoly_complete) {
  tmp <- crop(bio$bio15, species) %>% mask(species)
  dataset <-
    as.data.frame(values(tmp)) %>% drop_na() 
  colnames(dataset) <- "species"
  result[[as.character(unique(species$SCINAME))]] <-
    dataset %>% summarise(
      prec_seasonality_mean = mean(species),
      prec_seasonality_sd = sd(species),
      prec_seasonality_max = max(species),
      prec_seasonality_min = min(species),
      prec_seasonality_median = median(species)
    )
}

bio15 <-
  data.frame(matrix(unlist(result), nrow = length(result), byrow = T))
colnames(bio15) <- colnames(result$`Dulus dominicus`)
rownames(bio15) <- names(result)
bio15 <- bio15 %>% rownames_to_column(var = "species_name")

result = NULL
for (species in resident_npoly_complete) {
  tmp <- crop(bio$bio16, species) %>% mask(species)
  dataset <-
    as.data.frame(values(tmp)) %>% drop_na() 
  colnames(dataset) <- "species"
  result[[as.character(unique(species$SCINAME))]] <-
    dataset %>% summarise(
      prec_wet_quarter_mean = mean(species),
      prec_wet_quarter_sd = sd(species),
      prec_wet_quarter_max = max(species),
      prec_wet_quarter_min = min(species),
      prec_wet_quarter_median = median(species)
    )
}

bio16 <-
  data.frame(matrix(unlist(result), nrow = length(result), byrow = T))
colnames(bio16) <- colnames(result$`Dulus dominicus`)
rownames(bio16) <- names(result)
bio16 <- bio16 %>% rownames_to_column(var = "species_name")

result = NULL
for (species in resident_npoly_complete) {
  tmp <- crop(bio$bio17, species) %>% mask(species)
  dataset <-
    as.data.frame(values(tmp)) %>% drop_na() 
  colnames(dataset) <- "species"
  result[[as.character(unique(species$SCINAME))]] <-
    dataset %>% summarise(
      prec_dry_quarter_mean = mean(species),
      prec_dry_quarter_sd = sd(species),
      prec_dry_quarter_max = max(species),
      prec_dry_quarter_min = min(species),
      prec_dry_quarter_median = median(species)
    )
}

bio17 <-
  data.frame(matrix(unlist(result), nrow = length(result), byrow = T))
colnames(bio17) <- colnames(result$`Dulus dominicus`)
rownames(bio17) <- names(result)
bio17 <- bio17 %>% rownames_to_column(var = "species_name")

result = NULL
for (species in resident_npoly_complete) {
  tmp <- crop(bio$bio18, species) %>% mask(species)
  dataset <-
    as.data.frame(values(tmp)) %>% drop_na()
  colnames(dataset) <- "species"
  result[[as.character(unique(species$SCINAME))]] <-
    dataset %>% summarise(
      prec_warm_quarter_mean = mean(species),
      prec_warm_quarter_sd = sd(species),
      prec_warm_quarter_max = max(species),
      prec_warm_quarter_min = min(species),
      prec_warm_quarter_median = median(species)
    )
}

bio18 <-
  data.frame(matrix(unlist(result), nrow = length(result), byrow = T))
colnames(bio18) <- colnames(result$`Dulus dominicus`)
rownames(bio18) <- names(result)
bio18 <- bio18 %>% rownames_to_column(var = "species_name")

result = NULL
for (species in resident_npoly_complete) {
  tmp <- crop(bio$bio19, species) %>% mask(species)
  dataset <-
    as.data.frame(values(tmp)) %>% drop_na() 
  colnames(dataset) <- "species"
  result[[as.character(unique(species$SCINAME))]] <-
    dataset %>% summarise(
      prec_cold_quarter_mean = mean(species),
      prec_cold_quarter_sd = sd(species),
      prec_cold_quarter_max = max(species),
      prec_cold_quarter_min = min(species),
      prec_cold_quarter_median = median(species)
    )
}

bio19 <-
  data.frame(matrix(unlist(result), nrow = length(result), byrow = T))
colnames(bio19) <- colnames(result$`Dulus dominicus`)
rownames(bio19) <- names(result)
bio19 <- bio19 %>% rownames_to_column(var = "species_name")

result = NULL
for (species in resident_npoly_complete) {
  tmp <- crop(alt, species) %>% mask(species)
  dataset <-
    as.data.frame(values(tmp)) %>% drop_na() 
  colnames(dataset) <- "species"
  result[[as.character(unique(species$SCINAME))]] <-
    dataset %>% summarise(
      prec_cold_quarter_mean = mean(species),
      prec_cold_quarter_sd = sd(species),
      prec_cold_quarter_max = max(species),
      prec_cold_quarter_min = min(species),
      prec_cold_quarter_median = median(species)
    )
}

alt <-
  data.frame(matrix(unlist(result), nrow = length(result), byrow = T))
colnames(alt) <- colnames(result$`Dulus dominicus`)
rownames(alt) <- names(result)
alt <- alt %>% rownames_to_column(var = "species_name")

total <-
  join_all(list(bio1,bio2,bio3,bio4, bio5, bio6, bio7, bio8, bio9, bio10,bio11,bio12,bio13,bio14,bio15,bio16,bio17,bio18,bio19,alt), by='species_name', type='full')

write.csv(total, "bioclimate.csv", row.names = F)
p <- read_csv(file.path(getwd(), "dataset1.csv"))






plot1 <- dataset %>% 
  filter(gs_index_md <= 1) %>%
  distinct(tree_name, .keep_all = TRUE) %>% 
  ggplot(aes(
    x=occurrences,
    y=gs_index_md, size=npoly
  ))+geom_jitter(alpha=0.5, col="#3ea6c2")+theme(
    plot.title = element_text(family = "Ubuntu", size=text_size),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(size= text_size, angle = 45, vjust = 0.9,hjust=1, face= "italic", family="Ubuntu"),
    axis.text.y = element_text(family="Ubuntu"),
    axis.title.x = element_text(family="Ubuntu"),
    axis.title.y = element_text(family="Ubuntu"),
    legend.position = "none",
    axis.ticks.y = element_blank()
  )+labs(title="M&D GSI (#subspecies/#islands)", y="GSI", x="# subspecies")

plot2 <- dataset %>% 
  filter(gs_norm != 1) %>%
  distinct(tree_name, .keep_all = TRUE) %>% 
  ggplot(aes(
    x=occurrences,
    y=gs_estandia, size=npoly
  ))+geom_jitter(alpha=0.5, col="#8e789e")+theme(
    plot.title = element_text(family = "Ubuntu", size=text_size),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(size= text_size, angle = 45, vjust = 0.9,hjust=1, face= "italic", family="Ubuntu"),
    axis.text.y = element_text(family="Ubuntu"),
    axis.title.x = element_text(family="Ubuntu"),
    axis.title.y = element_text(family="Ubuntu"),
    #legend.position = "none",
    axis.ticks.y = element_blank()
  )+labs(title="Estandia GSI (#subspecies/log(#islands))", y="GSI", x="# subspecies")

plot_comparison <- ggarrange(plot1+plot2, common.legend = TRUE, legend="left")

ggsave(
  plot = plot_comparison,
  filename = "plot_comparison.png",
  path = figures_path,
  device = "png",
  scale=1,
  width = 35,
  height = 15,
  dpi = 400,
  units = "cm"
)

plot3 <- dataset %>% 
  #filter(gs_norm != 1) %>%
  #distinct(tree_name, .keep_all = TRUE) %>% 
  filter(gs_index_md <= 1) %>%
  distinct(tree_name, .keep_all = TRUE) %>% 
  ggplot(aes(
    x=npoly,
    y=gs_index_md, size=occurrences
  ))+geom_jitter(alpha=0.5, col="#3ea6c2")+theme(
    plot.title = element_text(family = "Ubuntu", size=text_size),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(size= text_size, angle = 45, vjust = 0.9,hjust=1, face= "italic", family="Ubuntu"),
    axis.text.y = element_text(family="Ubuntu"),
    axis.title.x = element_text(family="Ubuntu"),
    axis.title.y = element_text(family="Ubuntu"),
    #legend.position = "none",
    axis.ticks.y = element_blank()
  )+labs(title="M&D GSI (#subspecies/#islands)", y="GSI", x="# islands")

plot4 <- dataset %>% 
  filter(gs_norm != 1) %>%
  distinct(tree_name, .keep_all = TRUE) %>% 
  #filter(gs_index_md <= 1) %>%
  #distinct(tree_name, .keep_all = TRUE) %>% 
  ggplot(aes(
    x=npoly,
    y=gs_norm, size=occurrences
  ))+geom_jitter(alpha=0.5, col="#3ea6c2")+theme(
    plot.title = element_text(family = "Ubuntu", size=text_size),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(size= text_size, angle = 45, vjust = 0.9,hjust=1, face= "italic", family="Ubuntu"),
    axis.text.y = element_text(family="Ubuntu"),
    axis.title.x = element_text(family="Ubuntu"),
    axis.title.y = element_text(family="Ubuntu"),
    legend.position = "none",
    axis.ticks.y = element_blank()
  )+labs(title="Estandia GSI (#subspecies/log(#islands))", y="GSI", x="# islands")

dataset4 %>% 
  distinct(tree_name, .keep_all = TRUE) %>% 
  ggplot(aes(
    x=alt_mean,
    y=gs_md, size=gs_estandia
  ))+geom_jitter(alpha=0.5, col="#8e789e")+geom_smooth(method="lm")+theme(
    plot.title = element_text(family = "Ubuntu", size=text_size),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(size= text_size, angle = 45, vjust = 0.9,hjust=1, face= "italic", family="Ubuntu"),
    axis.text.y = element_text(family="Ubuntu"),
    axis.title.x = element_text(family="Ubuntu"),
    axis.title.y = element_text(family="Ubuntu"),
    #legend.position = "none",
    axis.ticks.y = element_blank()
  )+labs(title="Maximum distance between polygons (range size)", 
         y="# islands\n", x="\n# subspecies")


p <- read.csv(file.path(getwd(), "dataset1.csv")) %>% filter(gs_index<=4.3) 

l <- read_csv(file.path(data_path,"dfs/diet_realm_Pigot2020.csv"))

dataset_realm <- left_join(dataset,l, by="species_name")

p <- p %>% filter(tree_name!="NA") 

names <- as.character(unique(p$tree_name))


tree <- read.tree("/home/zoo/sjoh4959/Documents/programs/bamm-master/build/hackett.tre")

tree <- keep.tip(tree, names)
p<- p %>% mutate(gs_norm=stdize(gs_index, na.rm=T)) 

p <- p %>% distinct(tree_name, .keep_all = TRUE)
matrix <- as.matrix(p$gs_norm)
rownames(matrix) <- p$tree_name
obj<-contMap(tree,matrix)

plotTree.barplot(tree,matrix, tip.label=NULL)
dotTree(tree,p$gs_norm)
is.na(p$gs_norm)

clade_member <- clade.members.list(tree, tips = FALSE, tip.labels = TRUE)
clade_member

plot(obj, type="fan", show.tip.label=F)
arc.cladelabels(text="clade A",node=598)

plot(obj)

lastPP<-get("last_plot.phylo",envir=.PlotPhyloEnv)
xlim<-lastPP$x.lim
ylim<-lastPP$y.lim
plot(obj,xlim=xlim,ylim=ylim,ftype="off")
lastPP<-get("last_plot.phylo",envir=.PlotPhyloEnv)
tips <- c("Zosterops_lateralis", "Coracina_tenuirostris", "Nectarinia_aspasia",
          "Turdus_poliocephalus", "Accipiter_novaehollandiae", "Coereba_flaveola", "Hypothymis_azurea",
          "Tanysiptera_galatea", "Lalage_maculosa", "Petroica_multicolor", "Gallirallus_philippensis",
          "Certhidea_olivacea", "Dicaeum_trigonostigma", "Uria_aalge")

xpos<-lastPP$xx[sapply(tips,function(x,y) which(y==x),
                       y=obj$tree$tip.label)]
ypos<-lastPP$yy[sapply(tips,function(x,y) which(y==x),
                       y=obj$tree$tip.label)]
text(xpos,ypos,gsub("_"," ",tips),pos=4,font=0.5)
 
dataset4 <- read_csv("/home/zoo/sjoh4959/Documents/projects/0.0_great-speciators/dataset4.csv")
