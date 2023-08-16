
fix_VI_Corp_land <- function() {
  
  df_geo <- subdistrict_estate_geodata()
  df_estates <- subdistrict_estates()
  
  
  df_vic <- df_estates %>% 
    filter(county_fips == "010") %>% 
    filter(grepl("^VI ", estate)) 
  
  geom_vic <- df_vic %>% pull(geom)
  
  df_geo_vic <- df_geo %>% 
    filter( geom == geom_vic) 
  #%>% 
  # slice(36:109)   
  
  df_geo_not_vic1 <- df_geo %>% 
    filter( geom < geom_vic)
  
  df_geo_not_vic2 <- df_geo %>% 
    filter(geom > geom_vic)
  
  max_geom <- max(df_estates$geom)
  
  # ========= new vics  =======================
  next_geom <- max_geom + 1
  
  df_geo_vic1 <- df_geo_vic %>% 
    slice(18:261) %>% 
    mutate(geom = next_geom)
  
  df_est_vic1 <- df_vic %>% 
    mutate(subdist_estate = "VI Corporation Land (A)", geom = next_geom) %>% 
    center_latlon(df_geo_vic1)
  
  next_geom <- next_geom + 1
  
  df_geo_vic2 <- df_geo_vic %>% 
    slice(262:304)  %>% 
    mutate(geom = next_geom)
  
  df_est_vic2 <- df_vic %>% 
    mutate(subdist_estate = "VI Corporation Land (B)", geom = next_geom)%>% 
    center_latlon(df_geo_vic2)
  
  next_geom <- next_geom + 1
  
  df_geo_vic3 <- df_geo_vic %>% 
    slice(312:530)  %>% 
    mutate(geom = next_geom)
  
  df_est_vic3 <- df_vic %>% 
    mutate(subdist_estate = "VI Corporation Land (C)", geom = next_geom)%>% 
    center_latlon(df_geo_vic3)
  
  next_geom <- next_geom + 1
  
  df_geo_vic4 <- df_geo_vic %>% 
    slice(552:601)  %>% 
    mutate(geom = next_geom)
  
  df_est_vic4 <- df_vic %>% 
    mutate(subdist_estate = "VI Corporation Land (D)", geom = next_geom)%>% 
    center_latlon(df_geo_vic4)
  
  df_geo <- rbind(df_geo_not_vic1, df_geo_vic1, df_geo_vic2, df_geo_vic3, df_geo_vic4, df_geo_not_vic2)
  
  save_subdistrict_estate_geodata(df_geo)
  
  
  df_estates <- df_estates %>% filter(geom != geom_vic) %>% 
    rbind(df_est_vic1, df_est_vic2, df_est_vic3, df_est_vic4)
  
  save_subdistrict_estates(df_estates)
}
