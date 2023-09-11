
pacman::p_load(sf, sp, automap, raster, tidyverse)

dir <- "D:/DOCS/CIENCIA DE SUELOS/R_SUELOS/glacier_mlMODIS/data/inaigem/glaciares_peru.shp"

glacier_polygons <- sf::read_sf(dir)
glacier_polygons$area_km2 <- st_area(glacier_polygons) / 10^6

areas_totales <- glacier_polygons %>% 
  dplyr::group_by(Cordillera) %>% 
  dplyr::summarize(t_area_km2 = sum(area_km2))


writexl::write_xlsx(areas_totales, "./areas_totales.xlsx")
