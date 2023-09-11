 

library(sf)
library(rnaturalearthdata)

d <- sf::read_sf("./data/inaigem/glaciares_peru.shp")
 aoi <- c("Blanca","Central","Huallanca",
          "Huayhuash","Huaytapallana","La Raya",
          "La viuda","Raura","Urubamba",
          "Vilcabamba","Vilcanota")

 d <- d %>% filter(Cordillera %in% aoi)
 
 # cordillera_grouped <- group_by(d, Cordillera)
 # cordillera_merged <- st_combine(cordillera_grouped)
 # 
 
 d_ <- d %>% 
   group_by(Cordillera) %>%
   summarise(geometry = sf::st_union(geometry)) %>%
   ungroup()
 
mv_simpl <- st_simplify(d_, preserveTopology = FALSE, dTolerance = 400)
mv_simpl_buf <- mv_simpl %>% st_buffer(15000) #%>% plot()



st_write(mv_simpl_buf, 
            paste0("./data/cordilleras_simplified_buffered_.gpkg"),
            driver="GPKG") 
 


mv_simpl_buf %>% plot()
