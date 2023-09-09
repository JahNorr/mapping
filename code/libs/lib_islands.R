

build_islands <- function() {
  
  df <- usvi::vi_islands %>% 
    select(island = Geographic.Area, county_fips = CountyCode, abbrev = Abbrev) %>% 
    mutate(county_fips = paste0("0", county_fips)) %>% 
    add_island_maplims()
  
  saveRDS(df, file = "./data/islands.rds")
}

islands <- function(isl = NULL) {
  
  
  df <- readRDS( file = "./data/islands.rds")
  
  if(!is.null(isl)) {
    
    if(orrr::is.integer_like(isl)) {
      
      fips <- paste0("0",as.integer(isl))
      df <- df %>% 
        filter(county_fips == fips) 
    } else  df <- df %>% filter(tolower(abbrev) == tolower(isl))
  }
  
  df
}

add_island_maplims <- function(df){
  
  require(dplyr)
  
  df_stx_lims <- data.frame(isl = "STX",
                            minlat = 17.67, maxlat = 17.8,
                            minlon = -64.90, maxlon = -64.56)
  
  df_stj_lims <- data.frame(isl = "STJ", 
                            minlat = 18.29, maxlat = 18.4,
                            minlon  = -64.81, maxlon = -64.64)
  
  df_stt_lims <- data.frame(isl = "STT", 
                            minlat = 18.28, maxlat = 18.416,
                            minlon = -65.09, maxlon = -64.81)
  
  df_lims <- rbind(df_stx_lims, df_stj_lims, df_stt_lims) 
  
  left_join(df,df_lims, by = c("abbrev" = "isl"))
}

island_maplims <- function(isl){
  
  require(dplyr, quietly = T, warn.conflicts = F)
  
 islands(isl) %>% select(matches("(lat|lon)$")) %>% unlist()
}