

fix_carlton_north <- function() {
  
  df_geo <- subdistrict_estate_geodata()
  df_estates <- subdistrict_estates()
  
  # df_est_web <-  df_estates %>% filter(grepl("Whim.*B", subdist_estate)) 
  # geom_web <- df_est_web %>% pull(geom)
  # df_geo_web <- df_geo %>% filter(geom == geom_web)
  # 
  # 
  df_est_cn <-  df_estates %>% filter(grepl("Carlton N", estate)) 
  geom_cn <- df_est_cn %>% pull(geom)
  
  df_geo_cn <- df_geo %>% filter(geom == geom_cn)
  ###########################################
  ##
  ##    these are the two geometries ...
  ##        separated by part ... 
  ##        they are in different subdistricts
  
  max_geom <- max(df_estates$geom)
  
  next_geom <- max_geom + 1
  
  start1 <- 20
  split1 <- 57
  
  nr1 = split1 - start1 + 1
  
  df_geo_cn1 <- df_geo_cn %>% 
    slice(start1:split1) %>% 
    mutate(geom = next_geom) 
  
  # 
  # ggplot(bind_rows(df_geo_cn1,df_geo_web), aes(x=x,y=y, fill = geom, group = geom))+
  #   geom_polygon()  +
  #   geom_point(color = "yellow") +
  #   coord_map()

  lats <- df_geo_cn1 %>% pull(y)
  lons <- df_geo_cn1 %>% pull(x)
  
  df_est_cn1 <- df_est_cn %>% 
    mutate(geom = next_geom) %>% 
    mutate(subdist_estate = "Carlton North (A)") %>% 
    mutate(center_lat = (min(lats) + max(lats))/2) %>% 
    mutate(center_lon = (min(lons) + max(lons))/2) 
  
  ##########  part 2 #################################
  
  next_geom <- next_geom + 1
  
  start2a <- split1
  split2a <- nrow(df_geo_cn)
  
  start2b <- 1
  split2b <- start1
  
  df_geo_cn2a <- df_geo_cn %>% 
    slice(start2a:split2a) %>% 
    mutate(geom = next_geom) 
  
  df_geo_cn2b <- df_geo_cn %>% 
    slice(start2b:(split2b)) %>% 
    mutate(geom = next_geom)  
  
  df_geo_cn2 <- bind_rows(df_geo_cn2b, df_geo_cn2a)
  
  # ggplot(df_geo_cn2, aes(x=x,y=y))+
  #   geom_polygon()  +
  #   geom_point(color = "yellow") +
  #   coord_map()
  # 
  # ggplot(bind_rows(df_geo_cn1,df_geo_cn2), aes(x=x,y=y))+
  #   geom_polygon(aes(group = geom, fill = geom))  +
  #   geom_point(color = "yellow") +
  #   coord_map()
  
  lats <- df_geo_cn2 %>% pull(y)
  lons <- df_geo_cn2 %>% pull(x)
  
  df_est_cn2 <- df_est_cn %>% 
    mutate(geom = next_geom) %>% 
    mutate(subdist_estate = "Carlton North (B)") %>% 
    mutate(center_lat = (min(lats) + max(lats))/2) %>% 
    mutate(center_lon = (min(lons) + max(lons))/2)   
  
  ##  these are the other geometries
  
  df_geo_not_cn1 <- df_geo %>% 
    filter( geom < geom_cn)
  
  df_geo_not_cn2 <- df_geo %>% 
    filter(geom > geom_cn)
  
  
  # ========= rebuild =======================
  
  ## put them together without the originals
  
  df_geo <- rbind(df_geo_not_cn1, df_geo_cn1, df_geo_cn2, df_geo_not_cn2)
  
  save_subdistrict_estate_geodata(df_geo)
  
  # remove this estate ... to be added as two different
  df_estates <- df_estates %>% filter(geom!=geom_cn) %>% 
    bind_rows(df_est_cn2, df_est_cn1) %>% 
    arrange(geom)

  save_subdistrict_estates(df_estates)
  
}

