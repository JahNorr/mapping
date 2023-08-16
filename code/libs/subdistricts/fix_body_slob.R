

fix_body_slob <- function() {
  
  require(dplyr)
  
  df_geo <- subdistrict_estate_geodata()
  df_estates <- subdistrict_estates()
  
  df_est_bs <-  df_estates %>% filter(estate == "Body Slob") 
  geom_bs <- df_est_bs %>% pull(geom)
  
  ###########################################
  ##
  ##    these are the two geometries ...
  ##        separated by part ... 
  ##        they are in different subdistricts
  
  max_geom <- max(df_estates$geom)
  next_geom <- max_geom + 1
  
  df_geo_bslob1 <- df_geo %>% 
    filter( geom == geom_bs & part == 1) %>%
    mutate(geom = next_geom)
  
  lats <- df_geo_bslob1 %>% pull(y)
  lons <- df_geo_bslob1 %>% pull(x)
  
  df_est_bslob1 <- df_est_bs %>% 
    mutate(geom = next_geom) %>% 
    mutate(subdist_estate = "Body Slob (A)") %>% 
    mutate(center_lat = (min(lats) + max(lats))/2) %>% 
    mutate(center_lon = (min(lons) + max(lons))/2) 
  
  next_geom <- next_geom + 1
  
  df_geo_bslob2 <- df_geo %>% 
    filter( geom == geom_bs & part ==2)  %>%
    mutate(geom = next_geom) 
  
  lats <- df_geo_bslob2 %>% pull(y)
  lons <- df_geo_bslob2 %>% pull(x)
  
  df_est_bslob2 <- df_est_bs %>% 
    mutate(geom = next_geom) %>% 
    mutate(subdist_estate = "Body Slob (B)") %>% 
    mutate(center_lat = (min(lats) + max(lats))/2) %>% 
    mutate(center_lon = (min(lons) + max(lons))/2) 
  
  ##  these are the other geometries
  
  df_geo_not_bslob1 <- df_geo %>% 
    filter( geom < geom_bs)
  
  df_geo_not_bslob2 <- df_geo %>% 
    filter(geom > geom_bs)
  
  
  # ========= rebuild =======================
  
  ## put them together without the originals
  
  df_geo <- rbind(df_geo_not_bslob1, df_geo_bslob1, df_geo_bslob2, df_geo_not_bslob2)
  
  save_subdistrict_estate_geodata(df_geo)
  
  # remove this estate ... to be added as two different
  df_estates <- df_estates %>% filter(geom!=geom_bs) %>% 
    bind_rows(df_est_bslob2, df_est_bslob1) %>% 
    arrange(geom)
  
  save_subdistrict_estates(df_estates)
  
  
  ##  just a test ... remove later
  
  geoms <- subdistrict_estates() %>% filter(grepl("Body Slob", subdist_estate)) %>% pull(geom)
  
  df_geo_test <- subdistrict_estate_geodata() %>% 
    filter( geom %in% geoms) 
  
  # ggplot() + 
  #   geom_polygon(data = df_geo_test, aes(x = x, y = y, group = interaction(geom), fill =  geom),
  #                colour = "black") 
}

