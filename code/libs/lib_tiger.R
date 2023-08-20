

require(dplyr, warn.conflicts = FALSE, quietly = TRUE)


tiger_url <- function(state, county) {
  
  
  st_fips <- fips::state_fips(state)  
  co_fips <- fips::county_fips (state, county)  
  
  state_name <- fips::fips_st_2020_txt %>% 
    filter(fips == {{st_fips}}) %>% 
    pull(state) %>% 
    toupper() %>% gsub(" ", "_", .)
  
  state <- fips::fips_st_2020_txt %>% filter(fips == {{st_fips}}) %>% pull(state_abb)
  
  url_fips <- paste0(st_fips, co_fips)
  
  paste0("https://www2.census.gov/geo/tiger/TIGER_RD18/STATE/",
         st_fips, "_", state_name, "/", url_fips, "/")
  
}


download.tiger <- function(state, county = NULL) {
  
  dest_folder <- shp_folder(state = state, county = county)
  
  url <- tiger_url(state = state, county = county)
  
  filenames  <-  url_files(url) %>% 
    grep("tl_",., value = TRUE)
  
  filenames
  
  if(!dir.exists(dest_folder)) dir.create(dest_folder,recursive = T)
  
  invisible(
    sapply(filenames, function(filename){
      
      download.file(url = paste0(url,filename), 
                    destfile = paste0(dest_folder,filename)
      )
    })
  )
  
}
