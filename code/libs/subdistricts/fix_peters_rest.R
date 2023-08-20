

fix_peters_rest <- function() {
  
  df_geo <- subdistrict_estate_geodata()
  df_estates <- subdistrict_estates()
  
  df_est_wr <-  df_estates %>% filter(grepl("Work.*Rest", estate)) 
  geom_wr <- df_est_wr%>% pull(geom)
  df_geo_wr <- df_geo %>% filter(geom == geom_wr)
 
  df_est_ch <-  df_estates %>% filter(grepl("Const.*Hill", estate)) 
  geom_ch <- df_est_ch%>% pull(geom)
  df_geo_ch <- df_geo %>% filter(geom == geom_ch)
  
  df_est_pr <-  df_estates %>% filter(grepl("Peters Rest", estate)) 
  geom_pr <- df_est_pr %>% pull(geom)
  
  df_geo_pr <- df_geo %>% filter(geom == geom_pr)
  ###########################################
  ##
  ##    these are the two geometries ...
  ##        separated by part ... 
  ##        they are in different subdistricts
  
  max_geom <- max(df_estates$geom)
  
  next_geom <- max_geom + 1
  
  start1 <- 18
  split1 <- 41
  
  nr1 = split1 - start1 + 1
  
  df_geo_petrst1 <- df_geo_pr %>% 
    slice(start1:split1) %>% 
    mutate(geom = next_geom)

  # 
  ggplot(bind_rows(df_geo_petrst1,df_geo_wr,df_geo_ch),
         aes(x=x,y=y, group = geom, fill = geom))+
    geom_polygon()  +
    geom_point(color = "yellow") +
    coord_map()
  
  lats <- df_geo_petrst1 %>% pull(y)
  lons <- df_geo_petrst1 %>% pull(x)
  
  df_est_petrst1 <- df_est_pr %>% 
    mutate(geom = next_geom) %>% 
    mutate(subdist_estate = "Peters Rest (A)") %>% 
    mutate(center_lat = (min(lats) + max(lats))/2) %>% 
    mutate(center_lon = (min(lons) + max(lons))/2) 
  
  ##########  part 2 #################################
  
  next_geom <- next_geom + 1
  
  start2a <- split1
  split2a <- nrow(df_geo_pr)
  
  nr2 = split2a - start2a + 1
  
  df_geo_petrst2a <- df_geo_pr %>% 
    slice(start2a:split2a) %>% 
    mutate(geom = next_geom) 
  
  start2b <- 1
  split2b <- start1
  
  nr2 = split2b - start2b + 1
  
  df_geo_petrst2b <- df_geo_pr %>% 
    slice(start2b:split2b) %>% 
    mutate(geom = next_geom) 
  
  df_geo_petrst2 <-  bind_rows(df_geo_petrst2a ,df_geo_petrst2b )
  
  lats <- df_geo_petrst2 %>% pull(y)
  lons <- df_geo_petrst2 %>% pull(x)
  
  df_est_petrst2 <- df_est_pr %>% 
    mutate(geom = next_geom) %>% 
    mutate(subdist_estate = "Peters Rest (B)") %>% 
    mutate(center_lat = (min(lats) + max(lats))/2) %>% 
    mutate(center_lon = (min(lons) + max(lons))/2) 
  
  ##  these are the other geometries
  
  df_geo_not_petrst1 <- df_geo %>% 
    filter( geom < geom_pr)
  
  df_geo_not_petrst2 <- df_geo %>% 
    filter(geom > geom_pr)
  
  
  # ========= rebuild =======================
  
  ## put them together without the originals
  
  df_geo <- rbind(df_geo_not_petrst1, df_geo_petrst1, df_geo_petrst2, df_geo_not_petrst2)
  
  save_subdistrict_estate_geodata(df_geo)
  
  # remove this estate ... to be added as two different
  df_estates <- df_estates %>% filter(geom!=geom_pr) %>% 
    bind_rows(df_est_petrst2, df_est_petrst1) %>% 
    arrange(geom)
  
  save_subdistrict_estates(df_estates)
  
}

