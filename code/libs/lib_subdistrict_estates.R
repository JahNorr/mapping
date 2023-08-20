
require(dplyr)


split_estates <- function(verbose = FALSE) {
  
  df_subd <- subdistricts() %>% 
    arrange(county_fips, subdist_num)
  
  co_fips <- df_subd %>% pull(county_fips) %>% unique() %>% sort()
  
  df_estates <- estate_data()
  
  f <- subdistricts_shp_filename()
  subdist_geo <- vect(f)
  
  f <- estates_shp_filename()
  estates_geo <- vect(f)
  
  df_subdistict_estates <- data.frame()
  
  invisible(
    sapply(co_fips, function(fips) {
      
      ## for each county (island) fips
      
      #browser()
      
      df_subds <- subdistricts() %>% filter(county_fips == fips)
      df_estates <- estate_data() %>% filter(county_fips == fips)
      
      mapply(function(subd, geom_subd) {
        
        ## for each subdistrict on that island
        
        #browser()      
        
        subd_geo <- subdist_geo[geom_subd]
        
        
        mapply(function(estate, geom_est) {
          
          ## try each estate on that island
          
          #browser()        
          
          df_est <- df_estates %>% filter(geom == geom_est)
          
          est_geo <- estates_geo[geom_est]
          
          area_est <- terra::expanse(est_geo)
          
          tryCatch({
            geo_intsect <- intersect(subd_geo, est_geo)
            
            df_geo_intsec <- geo_intsect %>% geom() %>% as.data.frame()
            
            area_intsect <- terra::expanse(geo_intsect)
            area_diff <- as.integer(area_est-area_intsect)
            
            if(verbose) {
              cat(fips, ":[", subd,"] :", stringr::str_pad(estate,25, side = "right"), 
                  "   Estate area:", sprintf("%8d",as.integer(area_est)), 
                  "   Intsct area:", sprintf("%8d",as.integer(area_intsect)), 
                  "   Diff:",sprintf("%8d",area_diff),"\n")
            }
            
            df_est_add <- df_est %>% 
              mutate( subdist_estate = estate) %>% 
              mutate(subdist_num = subd) %>% 
              mutate(area = area_intsect) %>% 
              mutate(split = area_diff>0) %>% 
              group_by(county_fips, estate) %>% 
              mutate(index = row_number())
            
            
            df_subdistict_estates <<- df_subdistict_estates %>% 
              bind_rows(df_est_add)
          },
          warning = function(w) {}
          )        
          
        }, df_estates$estate, df_estates$geom)
        
        
      }, df_subds$subdist_num, df_subds$geom    )
      
    })
  )
  
  df_subdistict_estates
  
  # 
  # save_subdistrict_estates(df_estates)
  # 
  # df_geo <- rbind(df_geo_not_est1,df_geo_not_est2)
  # save_subdistrict_estate_geodata(df_geo)
}

split_estate <- function(isl , subd , estate , part = "A" ) {
  
  df_geo <- subdistrict_estate_geodata()
  df_estates <- subdistrict_estates()
  
  next_geom <- max(df_estates$geom) + 1
  
  df_est <-  df_estates %>% filter(grepl({{estate}}, estate)) 
  geom_est <- df_est %>% pull(geom)
  
  df_estates <- df_estates %>% filter(geom!=geom_est)
  
  df_geo_not_est1 <- df_geo %>% 
    filter( geom < geom_est)
  
  df_geo_not_est2 <- df_geo %>% 
    filter(geom > geom_est)
  
  df_est_new <- data.frame()
  
  ipart <- which(toupper(letters) == toupper(part))
  isub <- 1
  
  sapply(subd, function(subdist){
    
    
    df_est1 <- df_est %>% 
      mutate(subdist_estate = paste0(estate, " (", toupper(letters[ipart]), ")")) %>% 
      mutate(geom = next_geom)
    
    df_est_new <<- df_est_new %>% bind_rows(df_est1)
    
    #################################################################
    ##
    ##    handle new geometry
    
    ##     add new geometry
    
    browser()
    df_geo_new <- estate_intersected(isl = isl, subd = subdist, estate = estate )%>% 
      mutate(geom = next_geom)
    
    ##  these are the other geometries
    
    df_geo_not_est1 <<- rbind(df_geo_not_est1, df_geo_new)
    
    
    ###################################################################
    
    df_estates <<- df_estates %>% 
      bind_rows(df_est1) %>% 
      arrange(geom)
    
    ipart <- ipart + 1
    next_geom <- next_geom + 1
    isub <- isub + 1
  })
  
  df_estates <- df_estates %>% bind_rows(df_est_new) %>% 
    arrange(geom)
  
  save_subdistrict_estates(df_estates)
  
  df_geo <- rbind(df_geo_not_est1,df_geo_not_est2)
  save_subdistrict_estate_geodata(df_geo)
}

estate_intersected <- function(isl, subd, estate) {
  
  co_fips <- islands() %>% filter(abbrev == toupper(isl)) %>% pull(county_fips)
  
  est_chk <- estate
  
  geom_est <- estate_data() %>% filter(county_fips  == co_fips & grepl({{est_chk}},estate)) %>% pull(geom)
  
  geom_subd <- subdistricts() %>% filter(county_fips  == co_fips & subdist_num == subd) %>% pull(geom)
  
  f <- subdistricts_shp_filename()
  geo1 <- vect(f)
  geo1 <- geo1[geom_subd]
  
  f <- estates_shp_filename()
  geo2 <- vect(f)
  geo2 <- geo2[geom_est]
  
  geo3 <- intersect(geo1,geo2)
  
  geo3 %>% geom() %>% as.data.frame()
  
}

build_subdistrict_estate_geodata <- function() {
  
  df <- as.data.frame(terra::geom(estate_geodata_raw())) 
  
  save_subdistrict_estate_geodata(df)
  
  
}


build_subdistrict_estates <- function() {
  
  
  df <- prepped_estate_data()  %>% 
    
    # this will hold the new name (saving the original in estate)
    
    mutate(subdist_estate = estate) %>% 
    
    ##  the subdistrict number 
    ##  (used with the county_fips to identify the subdistrict)
    
    mutate(subdist = NA)
  
  save_subdistrict_estates(df)
}

save_subdistrict_estate_geodata <- function(df) {
  
  saveRDS(df, "./data/subdistricts/subdistrict_estates_geo.rds")
}

save_subdistrict_estates <- function(df) {
  
  saveRDS(df, "./data/subdistricts/subdistrict_estates.rds")
}

subdistrict_estates <- function() {
  
  readRDS("./data/subdistricts/subdistrict_estates.rds")
}

subdistrict_estate_geodata <- function(df) {
  
  readRDS("./data/subdistricts/subdistrict_estates_geo.rds")
}
