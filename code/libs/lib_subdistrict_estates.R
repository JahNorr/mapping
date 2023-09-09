
require(dplyr)
require(terra)


split_estates <- function(verbose = FALSE) {
  
  df_subd <- subdistricts() %>% 
    arrange(county_fips, subdist_num)
  
  co_fips <- df_subd %>% pull(county_fips) %>% unique() %>% sort()
  
  df_isl_estates <- estate_data()
  next_geom <- max(df_isl_estates$geom) + 1
  
  f <- subdistricts_shp_filename()
  subdist_geo <- vect(f)
  
  f <- estates_shp_filename()
  estates_geo <- vect(f)
  
  
  df_all_subds <- subdistricts() 
  df_all_estates <- estate_data()
  
  ## we're going to rebuild subdistrict estates from scratch
  ##  start new blank data.frames
  
  df_subdistrict_estates <- data.frame()
  df_subdist_estates_geo <- data.frame()
  df_isl_estates_geo <- geom(estates_geo) %>% as.data.frame()
  
  invisible(
    
    # for each island (by fips)
    
    sapply(co_fips, function(fips) {
      
      ## for each county (island) fips
      
      ## get the subdistricts and estates for this island
      
      df_isl_subdists <- df_all_subds %>% filter(county_fips == fips)
      df_isl_estates <- df_all_estates %>% filter(county_fips == fips)
      
      
      mapply(function(estate, geom_est) {
        
        ## try each estate on that island
        
        df_est <- df_isl_estates %>% filter(geom == geom_est)
        
        if (verbose) cat(rep("=",50), "\n", estate, "\n", sep = "")
        
        est_geo <- estates_geo[geom_est]
        
        area_est <- terra::expanse(est_geo)
        
        est_index  <-  1
        
        mapply(function(subd, geom_subd) {
          
          ## for each subdistrict on that island
          
          if (verbose) cat(rep("",4), subd, " . ", sep = "")
          
          subd_geo <- subdist_geo[geom_subd]
          
          tryCatch({
            
            # get the intersection of the estate with the subdistrict
            #
            #  area_est = the area of the  estate 
            #  area_intsect = the area of the intersection of the estate and the subdistrict
            #
            #  area_diff = the difference between the entire estate an=rea and the intersection
            #  area_pct = pct of the area intersection with respect to the area of the estate
            #
            #  area diff = 0 means the pct = 100
            
            # if the area
            
            geo_intsect <- intersect(subd_geo, est_geo)
            
            df_geo_intsec <- geo_intsect %>% geom() %>% as.data.frame()
            
            # check if the area of the intersection is different from 
            #   the area of the estate ... if so, it should be split
            
            area_intsect <- terra::expanse(geo_intsect)
            area_diff <- as.integer(area_est-area_intsect)
            pct  <-  round(area_intsect/area_est*100, 2)
            
            if(pct < 0.1) {
            
              stop("0 intersect got through!")
            }
            
            # get logical for split ... if TRUE, the intersected area is different 
            #   from the entire area
            
            split <-  (area_diff > 0 && pct > 0.1)
            
            if(verbose) {
              cat("\n .. ",fips, ":[", subd,"] :", stringr::str_pad(estate,20, side = "right"), 
                  "   Estate area:", sprintf("%8d",as.integer(area_est)), 
                  "   Intsct area:", sprintf("%8d",as.integer(area_intsect)), 
                  "   Pct area:", sprintf("%3.1f", pct), 
                  "   Diff:",sprintf("%8d",area_diff),"\n", sep = "")
            }
            
            df_est_add <- df_est %>% 
              mutate(subdist_estate = estate) %>% 
              mutate(subdist_num = subd) %>% 
              mutate(area = round(area_intsect,1)) %>% 
              mutate(area_est = area_est) %>% 
              mutate(diff = area_diff) %>% 
              mutate(subd_pct = pct) %>% 
              relocate(area, .before = area_est)
            
            ## see if we need to remove the original and add the intersected
            
            if(split) {
              
              if(verbose) cat("     *** splitting *** ... \n")
              
              lats <- df_geo_intsec %>% pull(y)
              lons <- df_geo_intsec %>% pull(x)
              
              
              df_est_add <- df_est_add %>% 
                mutate(subdist_estate = 
                         paste0(subdist_estate, " (", 
                                toupper(letters[est_index]), ")")) %>% 
                mutate(geom = next_geom) %>% 
                mutate(center_lat = (min(lats) + max(lats))/2) %>% 
                mutate(center_lon = (min(lons) + max(lons))/2) 
              
              df_geo_intsec <- df_geo_intsec %>% 
                mutate(geom = next_geom)
              
              df_isl_estates_geo <<- df_isl_estates_geo %>% 
                bind_rows(df_geo_intsec)
              
              ## increment the next geometry to be used for new geometries
              
              next_geom <<- next_geom + 1
              est_index <<-  est_index + 1
              
            } else  {
              
              # do not split
              if(verbose) cat("     *** NOT splitting *** ... \n")
              
              df_geo_intsec <- df_geo_intsec %>% 
                mutate(geom = geom_est)
            }
            
            df_subdist_estates_geo <<- df_subdist_estates_geo %>% 
              bind_rows(df_geo_intsec)
            
            
            df_subdistrict_estates <<- df_subdistrict_estates %>% 
              bind_rows(df_est_add)
          },
          error = function(e) {
            if(verbose) cat(e$message,"\n")
          }
          ,
          warning = function(w) {
            if(verbose) cat(w$message,"\n")
            
          }
        ) # end of tryCatch       
          
          
          
        }, df_isl_subdists$subdist_num, df_isl_subdists$geom)   # mapply each subdistrict   
        
        if(verbose) cat("\n") 
              
      }, df_isl_estates$estate, df_isl_estates$geom)  # mapply each estate
      
    })   # sapply each island
  )
  
  ##    save the subdistrict estates
  
  save_subdistrict_estate_geodata(df_subdist_estates_geo)
  save_subdistrict_estates(df_subdistrict_estates)
  
}

split_estate <- function(isl , subd , estate , part = "A" ) {
  
  df_geo <- subdistrict_estate_geodata()
  df_sub_estates <- subdistrict_estates()
  
  next_geom <- max(df_sub_estates$geom) + 1
  
  df_est <-  df_sub_estates %>% filter(grepl({{estate}}, estate)) 
  geom_est <- df_est %>% pull(geom)
  
  df_sub_estates <- df_sub_estates %>% filter(geom!=geom_est)
  
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
    
    
    df_geo_new <- estate_intersected(isl = isl, subd = subdist, estate = estate )%>% 
      mutate(geom = next_geom)
    
    ##  these are the other geometries
    
    df_geo_not_est1 <<- rbind(df_geo_not_est1, df_geo_new)
    
    
    ###################################################################
    
    df_sub_estates <<- df_sub_estates %>% 
      bind_rows(df_est1) %>% 
      arrange(geom)
    
    ipart <- ipart + 1
    next_geom <- next_geom + 1
    isub <- isub + 1
  })
  
  df_sub_estates <- df_sub_estates %>% bind_rows(df_est_new) %>% 
    arrange(geom)
  
  save_subdistrict_estates(df_sub_estates)
  
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
  
  ## return the intersection of this estate with this subdistrict
  
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
