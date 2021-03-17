source("./src/0.0_great-speciators_source.R")

tree_vig <- read.tree(file.path(data_path, "raw/bamm/birds_hackett.tre"))
cons_vig = ape::consensus(tree_vig, p=0.5)

total_world <- read_csv(file.path(data_path, "dfs/worldd.csv"))


total_world_model <-
  total_world %>% 
  filter(nsubsp / npoly <= 1) %>% #remove sympatric subspeciation events
  mutate(nsubsp = ifelse(nsubsp !=0, nsubsp, 1)) #if the number of subspecies is 0, treat them as having 1 

total_world_model$altitude_range <- 
  as.numeric(total_world_model$altitude_range) #technical problems :)

#total_world_model2 <- 
#  total_world_model %>% 
#  mutate(rel_island_size = log(area)/log(npoly)) %>% 
#  mutate(rel_island_size=na_if(rel_island_size, Inf)) %>% 
#  distinct(species_name, .keep_all = TRUE) %>% 
#  drop_na(rel_island_size,nsubsp,npoly)
#total_world_model2 <- total_world_model2 %>%  
#  mutate(rel_island_size=na_if(rel_island_size, -Inf))
  
#total_world_model2$rel_island_size <- 
#  stdize(total_world_model2$rel_island_size, na.rm=TRUE)

df_world <- 
  total_world_model %>% 
  distinct(species_name, .keep_all = TRUE) %>% 
  filter(tree_name %in% tree_vig$tip.label) %>%  
  mutate(failures=npoly-nsubsp) #failures is needed in phyr but not in brms, mute if using brms

df_world_2 <- 
  df_world %>% 
  mutate(log_median=log(median)) %>% 
  mutate(log_max=log(max_dist_poly)) %>% 
  mutate(log_area=log(area)) %>% 
  drop_na(altitude_range) %>% 
  mutate(altitude_range=na_if(altitude_range, -Inf)) %>% 
  mutate(log_max=na_if(log_max, -Inf)) %>% 
  mutate(log_median=na_if(log_median, -Inf))
  #drop_na(altitude_range) #%>% 
  #mutate(rel_median_npoly=log(median)/log(npoly)) %>%
  #mutate(rel_median_npoly=na_if(rel_median_npoly, -Inf)) %>% 
  #drop_na(rel_median_npoly)

list_var <- c(
  "hwi",
  "PC1",
  "precip_range",
  "altitude_range",
  "temperature_range",
  "log_max",
  "log_median",
  "netdiv"
)

lst=NULL
for (variable in list_var) {
  lst[[variable]] <- df_world_2 %>% 
    dplyr::select(variable) %>%  
    stdize(.,na.rm=TRUE)
}

standarised <- do.call(cbind, unname(lst))
colnames(standarised) <- paste0("norm_", colnames(standarised))

df_world_final <- cbind(df_world_2, standarised)

phylo_world <- keep.tip(tree_vig,df_world_final$tree_name)

phylo_world2 = compute.brlen(phylo_world) # computes branch lengths, needed for next steps
world_phylo <- vcv.phylo(phylo_world2, cor = T)

mod_world_logit <-
  brm(
    nsubsp |
      trials(npoly) ~ norm_hwi * norm_PC1 + norm_precip_range * norm_altitude_range*norm_temperature_range + norm_log_max + norm_log_median + norm_netdiv + Realm +
      diet + habitat + migratory_status_3 + (1 |gr(tree_name, cov = world_phylo)),
    data = df_world_final,
    data2 = list(world_phylo = world_phylo),
    chains = 2,
    iter = 4000,
    inits = "0",
    threads = threading(30),
    backend = "cmdstanr",
    control = list(adapt_delta = 0.95),
    family = zero_inflated_binomial("logit")
  )
  
save(mod_world_logit, file="mod_world_logit.RData")
