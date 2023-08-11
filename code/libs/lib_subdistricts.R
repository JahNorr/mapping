
require(dplyr, quietly = TRUE, warn.conflicts = FALSE)



init_subdistricts <- function() {
  
  df <- usvi::vi_subdistricts %>% 
    select(subdist_num, subdistrict = Geographic.Area, county_fips = CountyCode ) %>% 
    mutate(county_fips = paste0("0",county_fips)) %>% 
    mutate(subdistrict = gsub(" subdistrict","",  subdistrict)) %>% 
    mutate(minlat = 0) %>% 
    mutate(maxlat = 0)%>% 
    mutate(minlon = 0) %>% 
    mutate(maxlon = 0)
  
  saveRDS(df, file = "./data/subdistricts.rds")
}

subdistricts <- function() {
  
  readRDS(file = "./data/subdistricts.rds")
  
}


subdist_from_func <- function() {
  
  x <- as.character(sys.calls())[1]
  
  gsub(".*_([0-9]{1,2})\\(.*\\)$","\\1",x)
}

isl_from_func <- function() {
  
  x <- as.character(sys.calls())[1]
  
  gsub(".*_(st.)_([0-9]{1,2})\\(.*\\)$","\\1",x)
}



source_subd_builds <- function() {
    files <- list.files("./code/libs/subdistricts", full.names = TRUE)
    
    sapply(files, source)
}

build_subdistricts <- function() {

  source_subd_builds()
  
  df_subdistricts <- subdistricts()
  
  build_
}
  
