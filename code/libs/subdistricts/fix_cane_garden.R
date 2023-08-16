

fix_cane_garden <- function() {
  
  df_geo <- subdistrict_estate_geodata()
  df_estates <- subdistrict_estates()
  
  df_est_cg <-  df_estates %>% filter(grepl("Cane Ga", estate)) 
  geom_cg <- df_est_cg %>% pull(geom)
  
  df_geo_cg <- df_geo %>% filter(geom == geom_cg)
  ###########################################
  ##
  ##    these are the two geometries ...
  ##        separated by part ... 
  ##        they are in different subdistricts
  
  max_geom <- max(df_estates$geom)
  
  next_geom <- max_geom + 1
  
  start1 <- 1
  split1 <- 63
  
  nr1 = split1 - start1 + 1
  
  startx <- 136
  splitx <- nrow(df_geo_cg)
  
  nrx = splitx - startx + 1
  
  df_geo_cg1a <- df_geo_cg %>% 
    slice(start1:split1) %>% 
    mutate(geom = next_geom) #%>% 
    # mutate(x0 = c(x[nr1],x[1:(nr1-1)]))%>%
    # mutate(y0 = c(y[nr1],y[1:(nr1-1)]))%>%
    # mutate(dx = x-x0)%>%
    # mutate(dy = y-y0)%>%
    # mutate(deg = atan(dy/dx)* 180 / pi)
  
  df_geo_cg1b <- df_geo_cg %>% 
    slice(startx:splitx) %>% 
    mutate(geom = next_geom) #%>% 
    # mutate(x0 = c(x[nrx],x[1:(nrx-1)]))%>%
    # mutate(y0 = c(y[nrx],y[1:(nrx-1)]))%>%
    # mutate(dx = x-x0)%>%
    # mutate(dy = y-y0)%>%
    # mutate(deg = atan(dy/dx)* 180 / pi)
  
  df_geo_cg1 <- bind_rows(df_geo_cg1a, df_geo_cg1b)
  
  # ggplot(df_geo_cg1, aes(x=x,y=y))+
  #   geom_polygon()  +
  #   geom_point(color = "yellow") +
  #   coord_map()
  
  lats <- df_geo_cg1 %>% pull(y)
  lons <- df_geo_cg1 %>% pull(x)
  
  df_est_cg1 <- df_est_cg %>% 
    mutate(geom = next_geom) %>% 
    mutate(subdist_estate = "Cane Garden (A)") %>% 
    mutate(center_lat = (min(lats) + max(lats))/2) %>% 
    mutate(center_lon = (min(lons) + max(lons))/2) 
  
  ##########  part 2 #################################
  
  next_geom <- next_geom + 1
  
  start2 <- split1 + 1
  split2 <- startx
  
  nr2 = split2 - start2 + 1
  
  df_geo_cg2 <- df_geo_cg %>% 
    slice(start2:split2) %>% 
    mutate(geom = next_geom) # %>% 
  # mutate(x0 = c(x[nr2],x[1:(nr2-1)]))%>%
  # mutate(y0 = c(y[nr2],y[1:(nr2-1)]))%>%
  # mutate(dx = x-x0)%>%
  # mutate(dy = y-y0)%>%
  # mutate(deg = atan(dy/dx)* 180 / pi * sign(dy))
  
  df_geo_cg2

  # ggplot(df_geo_cg2, aes(x=x,y=y))+
  #   geom_polygon()  +
  #   geom_point(color = "yellow") +
  #   coord_map()
  # 
  # ggplot(bind_rows(df_geo_cg1,df_geo_cg2), aes(x=x,y=y))+
  #   geom_polygon(aes(group = geom, fill = geom))  +
  #   geom_point(color = "yellow") +
  #   coord_map()
  # 
  lats <- df_geo_cg2 %>% pull(y)
  lons <- df_geo_cg2 %>% pull(x)
  
  df_est_cg2 <- df_est_cg %>% 
    mutate(geom = next_geom) %>% 
    mutate(subdist_estate = "Cane Garden (B)") %>% 
    mutate(center_lat = (min(lats) + max(lats))/2) %>% 
    mutate(center_lon = (min(lons) + max(lons))/2) 
  
  ##  these are the other geometries
  
  df_geo_not_cg1 <- df_geo %>% 
    filter( geom < geom_cg)
  
  df_geo_not_cg2 <- df_geo %>% 
    filter(geom > geom_cg)
  
  
  # ========= rebuild =======================
  
  ## put them together without the originals
  
  df_geo <- rbind(df_geo_not_cg1, df_geo_cg1, df_geo_cg2, df_geo_not_cg2)
  
  save_subdistrict_estate_geodata(df_geo)
  
  # remove this estate ... to be added as two different
  df_estates <- df_estates %>% filter(geom!=geom_cg) %>% 
    bind_rows(df_est_cg2, df_est_cg1) %>% 
    arrange(geom)
  
  save_subdistrict_estates(df_estates)
  
}

