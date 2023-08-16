

fix_diamond_west <- function() {
  
  df_geo <- subdistrict_estate_geodata()
  df_estates <- subdistrict_estates()
  
  df_est_dw <-  df_estates %>% filter(grepl("Diamond W", estate)) 
  geom_dw <- df_est_dw %>% pull(geom)
  
  df_geo_dw <- df_geo %>% filter(geom == geom_dw)
  ###########################################
  ##
  ##    these are the two geometries ...
  ##        separated by part ... 
  ##        they are in different subdistricts
  
  max_geom <- max(df_estates$geom)
  
  next_geom <- max_geom + 1
  
  start1 <- 11
  split1 <- 69
  
  nr1 = split1 - start1 + 1
  
  df_geo_dw1 <- df_geo_dw %>% 
    slice(start1:split1) %>% 
    mutate(geom = next_geom) 

  
  # ggplot(df_geo_dw1, aes(x=x,y=y))+
  #   geom_polygon()  +
  #   geom_point(color = "yellow") +
  #   coord_map()
  # 
  lats <- df_geo_dw1 %>% pull(y)
  lons <- df_geo_dw1 %>% pull(x)
  
  df_est_dw1 <- df_est_dw %>% 
    mutate(geom = next_geom) %>% 
    mutate(subdist_estate = "Diamond West (A)") %>% 
    mutate(center_lat = (min(lats) + max(lats))/2) %>% 
    mutate(center_lon = (min(lons) + max(lons))/2) 
  
  ##########  part 2 #################################
  
  next_geom <- next_geom + 1
  
  start2 <- split1
  split2 <- nrow(df_geo_dw)
  
  nr2 = split2 - start2 + 1
  
  df_geo_dw2a <- df_geo_dw %>% 
    slice(start2:split2) %>% 
    mutate(geom = next_geom) 
  
  df_geo_dw2b <- df_geo_dw %>% 
    slice(1:(start1)) %>% 
    mutate(geom = next_geom)  
  
  df_geo_dw2 <- bind_rows(df_geo_dw2a, df_geo_dw2b)

  # ggplot(df_geo_dw2, aes(x=x,y=y))+
  #   geom_polygon()  +
  #   geom_point(color = "yellow") +
  #   coord_map()
  # 
  # ggplot(bind_rows(df_geo_dw1,df_geo_dw2), aes(x=x,y=y))+
  #   geom_polygon(aes(group = geom, fill = geom))  +
  #   geom_point(color = "yellow") +
  #   coord_map()

  lats <- df_geo_dw2 %>% pull(y)
  lons <- df_geo_dw2 %>% pull(x)
  
  df_est_dw2 <- df_est_dw %>% 
    mutate(geom = next_geom) %>% 
    mutate(subdist_estate = "Diamond West (B)") %>% 
    mutate(center_lat = (min(lats) + max(lats))/2) %>% 
    mutate(center_lon = (min(lons) + max(lons))/2)   
  
  ##  these are the other geometries
  
  df_geo_not_dw1 <- df_geo %>% 
    filter( geom < geom_dw)
  
  df_geo_not_dw2 <- df_geo %>% 
    filter(geom > geom_dw)
  
  
  # ========= rebuild =======================
  
  ## put them together without the originals
  
  df_geo <- rbind(df_geo_not_dw1, df_geo_dw1, df_geo_dw2, df_geo_not_dw2)
  
  save_subdistrict_estate_geodata(df_geo)
  
  # remove this estate ... to be added as two different
  df_estates <- df_estates %>% filter(geom!=geom_dw) %>% 
    bind_rows(df_est_dw2, df_est_dw1) %>% 
    arrange(geom)
  
  save_subdistrict_estates(df_estates)
  
}

