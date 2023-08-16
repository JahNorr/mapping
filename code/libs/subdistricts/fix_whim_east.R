

fix_whim_east <- function() {
  
  df_geo <- subdistrict_estate_geodata()
  df_estates <- subdistrict_estates()
  
  df_est_we <-  df_estates %>% filter(grepl("Whim E", estate)) 
  geom_we <- df_est_we %>% pull(geom)
  
  df_geo_we <- df_geo %>% filter(geom == geom_we)
  ###########################################
  ##
  ##    these are the two geometries ...
  ##        separated by part ... 
  ##        they are in different subdistricts
  
  max_geom <- max(df_estates$geom)
  
  next_geom <- max_geom + 1
  
  start1 <- 50
  split1 <- 120
  
  nr1 = split1 - start1 + 1
  
  df_geo_we1 <- df_geo_we %>% 
    slice(start1:split1) %>% 
    mutate(geom = next_geom) 
  
  
  ggplot(df_geo_we1, aes(x=x,y=y))+
    geom_polygon()  +
    geom_point(color = "yellow") +
    coord_map()

  lats <- df_geo_we1 %>% pull(y)
  lons <- df_geo_we1 %>% pull(x)
  
  df_est_we1 <- df_est_we %>% 
    mutate(geom = next_geom) %>% 
    mutate(subdist_estate = "Whim East (A)") %>% 
    mutate(center_lat = (min(lats) + max(lats))/2) %>% 
    mutate(center_lon = (min(lons) + max(lons))/2) 
  
  ##########  part 2 #################################
  
  next_geom <- next_geom + 1
  
  start2a <- split1
  split2a <- nrow(df_geo_we)
  
  start2b <- 1
  split2b <- start1
  
  df_geo_we2a <- df_geo_we %>% 
    slice(start2a:split2a) %>% 
    mutate(geom = next_geom) 
  
  df_geo_we2b <- df_geo_we %>% 
    slice(start2b:(split2b)) %>% 
    mutate(geom = next_geom)  
  
  df_geo_we2 <- bind_rows(df_geo_we2b, df_geo_we2a)
  
  # ggplot(df_geo_we2, aes(x=x,y=y))+
  #   geom_polygon()  +
  #   geom_point(color = "yellow") +
  #   coord_map()
  # 
  # ggplot(bind_rows(df_geo_we1,df_geo_we2), aes(x=x,y=y))+
  #   geom_polygon(aes(group = geom, fill = geom))  +
  #   geom_point(color = "yellow") +
  #   coord_map()
  
  lats <- df_geo_we2 %>% pull(y)
  lons <- df_geo_we2 %>% pull(x)
  
  df_est_we2 <- df_est_we %>% 
    mutate(geom = next_geom) %>% 
    mutate(subdist_estate = "Whim East (B)") %>% 
    mutate(center_lat = (min(lats) + max(lats))/2) %>% 
    mutate(center_lon = (min(lons) + max(lons))/2)   
  
  ##  these are the other geometries
  
  df_geo_not_we1 <- df_geo %>% 
    filter( geom < geom_we)
  
  df_geo_not_we2 <- df_geo %>% 
    filter(geom > geom_we)
  
  
  # ========= rebuild =======================
  
  ## put them together without the originals
  
  df_geo <- rbind(df_geo_not_we1, df_geo_we1, df_geo_we2, df_geo_not_we2)
  
  save_subdistrict_estate_geodata(df_geo)
  
  # remove this estate ... to be added as two different
  df_estates <- df_estates %>% filter(geom!=geom_we) %>% 
    bind_rows(df_est_we2, df_est_we1) %>% 
    arrange(geom)
  
  save_subdistrict_estates(df_estates)
  
}

