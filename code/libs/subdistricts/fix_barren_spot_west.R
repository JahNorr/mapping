

fix_barren_spot_west <- function() {
  
  df_geo <- subdistrict_estate_geodata()
  df_estates <- subdistrict_estates()
  
  df_est_bsw <-  df_estates %>% filter(grepl("Barren.*West", estate)) 
  geom_bsw <- df_est_bsw %>% pull(geom)
  
  df_geo_bsw <- df_geo %>% filter(geom == geom_bsw)
  ###########################################
  ##
  ##    these are the two geometries ...
  ##        separated by part ... 
  ##        they are in different subdistricts
  
  max_geom <- max(df_estates$geom)
  
  next_geom <- max_geom + 1
  
  start1 <- 23
  split1 <- 56
  
  nr1 = split1 - start1 + 1
  
  df_geo_bspotw1 <- df_geo_bsw %>% 
    slice(start1:split1) %>% 
    mutate(geom = next_geom) #%>% 
  # mutate(x0 = c(x[nr1],x[1:(nr1-1)]))%>% 
  # mutate(y0 = c(y[nr1],y[1:(nr1-1)]))%>% 
  # mutate(dx = x-x0)%>% 
  # mutate(dy = y-y0)%>% 
  # mutate(deg = atan(dy/dx)* 180 / pi)
  
  # df_geo_bspotw1
  # 
  # ggplot(df_geo_bspotw1, aes(x=x,y=y))+ 
  #   geom_polygon()  +
  #   geom_point(color = "yellow") +
  #   coord_map() 
  
  lats <- df_geo_bspotw1 %>% pull(y)
  lons <- df_geo_bspotw1 %>% pull(x)
  
  df_est_bspotw1 <- df_est_bsw %>% 
    mutate(geom = next_geom) %>% 
    mutate(subdist_estate = "Barren Spot West (A)") %>% 
    mutate(center_lat = (min(lats) + max(lats))/2) %>% 
    mutate(center_lon = (min(lons) + max(lons))/2) 
  
  ##########  part 2 #################################
  
  next_geom <- next_geom + 1
  
  start2 <- split1 + 1
  split2 <- nrow(df_geo_bsw)
  
  nr2 = split2 - start2 + 1
  
  df_geo_bspotw2 <- df_geo_bsw %>% 
    slice(start2:split2) %>% 
    mutate(geom = next_geom) #%>% 
  # mutate(x0 = c(x[nr2],x[1:(nr2-1)]))%>% 
  # mutate(y0 = c(y[nr2],y[1:(nr2-1)]))%>% 
  # mutate(dx = x-x0)%>% 
  # mutate(dy = y-y0)%>% 
  # mutate(deg = atan(dy/dx)* 180 / pi * sign(dy))
  
  # df_geo_bspotw2
  # 
  # ggplot(df_geo_bspotw2, aes(x=x,y=y))+ 
  #   geom_polygon()  +
  #   geom_point(color = "yellow") +
  #   coord_map()
  # 
  # ggplot(bind_rows(df_geo_bspotw1,df_geo_bspotw2), aes(x=x,y=y))+ 
  #   geom_polygon(aes(group = geom, fill = geom))  +
  #   geom_point(color = "yellow") +
  #   coord_map()
  
  lats <- df_geo_bspotw2 %>% pull(y)
  lons <- df_geo_bspotw2 %>% pull(x)
  
  df_est_bspotw2 <- df_est_bsw %>% 
    mutate(geom = next_geom) %>% 
    mutate(subdist_estate = "Barren Spot West (B)") %>% 
    mutate(center_lat = (min(lats) + max(lats))/2) %>% 
    mutate(center_lon = (min(lons) + max(lons))/2) 
  
  ##  these are the other geometries
  
  df_geo_not_bspotw1 <- df_geo %>% 
    filter( geom < geom_bsw)
  
  df_geo_not_bspotw2 <- df_geo %>% 
    filter(geom > geom_bsw)
  
  
  # ========= rebuild =======================
  
  ## put them together without the originals
  
  df_geo <- rbind(df_geo_not_bspotw1, df_geo_bspotw1, df_geo_bspotw2, df_geo_not_bspotw2)
  
  save_subdistrict_estate_geodata(df_geo)
  
  # remove this estate ... to be added as two different
  df_estates <- df_estates %>% filter(geom!=geom_bsw) %>% 
    bind_rows(df_est_bspotw2, df_est_bspotw1) %>% 
    arrange(geom)
  
  save_subdistrict_estates(df_estates)
  
}

