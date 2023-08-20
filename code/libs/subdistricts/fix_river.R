
require(dplyr, quietly = TRUE, warn.conflicts = FALSE)
require(ggplot2, quietly = TRUE, warn.conflicts = FALSE)
require(maps,quietly = T)
require(mapdata,quietly = T)

require(scales,quietly = T)  #for transparency

source("./code/libs/lib_roads.R")
source("./code/libs/lib_islands.R")
source("./code/libs/lib_estates.R")
source("./code/libs/lib_subdistricts.R")
source("./code/libs/lib_subdistrict_estates.R")



fix_river <- function() {
  
  df_geo <- subdistrict_estate_geodata()
  df_estates <- subdistrict_estates()
  
  df_est_rvr <-  df_estates %>% filter(grepl("^River", estate)) 
  geom_rvr <- df_est_rvr %>% pull(geom)
  
  df_geo_rvr <- df_geo %>% filter(geom == geom_rvr)
  
  df_river_rd_geo1 <-  entire_road_geo("^River Rd")  %>% 
    arrange(rev(row_number()))
  
  ###########################################
  ##
  ##    these are the two geometries ...
  ##        separated by part ... 
  ##        they are in different subdistricts
  
  max_geom <- max(df_estates$geom)
  
  next_geom <- max_geom + 1
  
  start1 <- 1
  split1 <- 10
  
  nr1 = split1 - start1 + 1
  
  df_geo_rvr1 <- df_geo_rvr %>% 
    slice(start1:split1) %>% 
    mutate(geom = next_geom) 
  
  
  start2 <- 21
  split2 <- 29
  
  df_geo_rvr2 <- df_geo_rvr %>% 
    slice(start2:split2) %>% 
    mutate(geom = next_geom) 
  
  df_geo_river_rd <- df_river_rd_geo1 %>% 
    slice(97:167) %>% 
    mutate(geom = next_geom) 
  
  df_geo_rvr1 <- bind_rows(df_geo_rvr2,df_geo_rvr1,df_geo_river_rd)
  
  ggplot()+
    geom_polygon(data = df_geo_rvr1, mapping = aes(x=x,y=y, fill = geom, group = geom))  +
    geom_point(color = "yellow") +
    coord_map()
  
  lats <- df_geo_rvr1 %>% pull(y)
  lons <- df_geo_rvr1 %>% pull(x)
  
  df_est_rvr1 <- df_est_rvr %>% 
    mutate(geom = next_geom) %>% 
    mutate(subdist_estate = "River (A)") %>% 
    mutate(center_lat = (min(lats) + max(lats))/2) %>% 
    mutate(center_lon = (min(lons) + max(lons))/2) 
  
  ##########  part 2 #################################
  
  next_geom <- next_geom + 1
  
  start2a <- split1
  split2a <- start2
  
  df_geo_river_rd <- df_geo_river_rd  %>% 
    arrange(rev(row_number()))  %>% 
    mutate(geom = next_geom) 
    
  df_geo_rvr2 <- bind_rows(df_geo_river_rd,
                           df_geo_rvr %>% 
                             slice(start2a:split2a) %>% 
                             mutate(geom = next_geom)) 
  
  
  ggplot(df_geo_rvr2, aes(x=x,y=y))+
    geom_polygon()  +
    geom_point(color = "yellow") +
    coord_map()

  # ggplot(bind_rows(df_geo_rvr1,df_geo_rvr2), aes(x=x,y=y))+
  #   geom_polygon(aes(group = geom, fill = geom))  +
  #   geom_point(color = "yellow") +
  #   coord_map()
  
  lats <- df_geo_rvr2 %>% pull(y)
  lons <- df_geo_rvr2 %>% pull(x)
  
  df_est_rvr2 <- df_est_rvr %>% 
    mutate(geom = next_geom) %>% 
    mutate(subdist_estate = "River (B)") %>% 
    mutate(center_lat = (min(lats) + max(lats))/2) %>% 
    mutate(center_lon = (min(lons) + max(lons))/2)   
  
  ##  these are the other geometries
  
  df_geo_not_rvr1 <- df_geo %>% 
    filter( geom < geom_rvr)
  
  df_geo_not_rvr2 <- df_geo %>% 
    filter(geom > geom_rvr)
  
  
  # ========= rebuild =======================
  
  ## put them together without the originals
  
  df_geo <- rbind(df_geo_not_rvr1, df_geo_rvr1, df_geo_rvr2, df_geo_not_rvr2)
  
  save_subdistrict_estate_geodata(df_geo)
  
  # remove this estate ... to be added as two different
  df_estates <- df_estates %>% filter(geom!=geom_rvr) %>% 
    bind_rows(df_est_rvr2, df_est_rvr1) %>% 
    arrange(geom)
  
  save_subdistrict_estates(df_estates)
  
}

