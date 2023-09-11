# load all geopkgs
# merge them in a single df
# two columns, train, test, and cordillera name




pacman::p_load(sf, sp, glmnet, ranger, kknn, gbm, spaMM, gstat, automap,
               spgwr, spdep, caret , Metrics, mccr, geoR, raster, rgeos,
               sperrorest, ggplot2, gridExtra, tidyverse)

lits_names <- c("Blanca",
                "Central",
                "Huallanca",
                "Huayhuasha",
                "Huaytapallana",
                "LaRaya",
                "LaViuda",
                "Raura",
                "Urubamba",
                "Vilcabamba",
                "Vilcanota")



# CORE FUNCTION

merge_gpkg <- function(aoi_n){
  
  lits_name <- lits_names[aoi_n]
  pattern <- paste0(lits_name,".*\\.gpkg$")
  
  points <- list.files(path = "./resultados/output/test_samples/",
                       recursive = TRUE,
                       full.names = TRUE,
                       pattern = pattern)
  print(points)
  point_list <- lapply(points, FUN = sf::st_read)
  ######
  ###### CHECK CRS
  ######
  for (i in 1:length(point_list)) {
    crs <- st_crs(point_list[[i]])$input
    print(crs)
  }
  for (i in 1:length(point_list)) {
    # Transform the CRS of each sf object to the target CRS
    point_list[[i]] <- st_transform(point_list[[i]], st_crs("EPSG:4326"))
    # Print the CRS of the transformed sf object
    print(st_crs(point_list[[i]])$input)
  }
  
  ## add columns
  # # test
  test <- point_list[[1]]
  test$aoi <- rep(lits_name, nrow(test))
  test$type <- rep("test", nrow(test))
  test <- test %>% dplyr::select("aoi","type")
  # train
  train <-  point_list[[2]]
  train$aoi <- rep(lits_name, nrow(train))
  train$type <- rep("train", nrow(train))
  train <- train %>% dplyr::select("aoi","type")
  # merge 
  merged <- rbind(test, train)
  return(merged)
  
  
}


# # LAPPLY

Blanca <- merge_gpkg(aoi_n = 1)
Central <- merge_gpkg(aoi_n = 2)
Huallanca <- merge_gpkg(aoi_n = 3)
Huayhuash <- merge_gpkg(aoi_n = 4)
Huaytapallana <- merge_gpkg(aoi_n = 5)
Raura <- merge_gpkg(aoi_n = 8)
Urubamba <- merge_gpkg(aoi_n = 9)
Vilcabamba <- merge_gpkg(aoi_n = 10)
VIlcanota <- merge_gpkg(aoi_n = 11)



merged <- rbind(Blanca, Central, Huallanca, 
                Huayhuash, Huaytapallana,
                Raura, Urubamba, Vilcabamba,
                VIlcanota)

output_file <- "./resultados/output/test_samples/all_sampling_points.gpkg"
st_write(merged, output_file, driver = "GPKG")



# # LAPPLY
# 
# # args <- c(1,2,3,4,5,6,7,8,9,10,11)
# 
# args <-  as.list(seq(lits_names))
# 
# # merged_list <- lapply(args, aoi_n merge_gpkg)
# 
# 
# output <- lapply(args, function(x) do.call(merge_gpkg, x))
# 



