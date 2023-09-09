
require(dplyr)

rebuild_estate_data <- function() {
  
  build_estates()
  build_estate_geodata()
  
}

estate_data_raw <- function(year=2022) {
  
  estate_geodata_raw() %>% 
    as.data.frame()
  
}

estates_shp_filename <- function(year=2022) {
  
  shp_file <- paste0("./data_raw/tl_", year,"_78_estate.shp")
  
}


estate_geodata_raw <- function(year=2022) {
  
  shp_file <- paste0("./data_raw/tl_", year,"_78_estate.shp")
  
  terra::vect(shp_file) 
  
}

estate_data <- function() {
  
  readRDS(file = "./data/estates.rds")
  
}

build_estate_geodata <- function() {
  
  df <- as.data.frame(terra::geom(estate_geodata_raw())) 
  
  save_estate_geodata(df)
  
  # geo <- estate_geodata_raw()
  # geo@ptr$polygonsList()
  
}

estate_geodata <- function() {
  
  readRDS("./data/estates_geo.rds")
  
}

save_estate_geodata <- function(df) {
  
  saveRDS(df, "./data/estates_geo.rds")
  
}

estate_geo_info <- function(df) {
  
  df_geo <- estate_geodata() %>% 
    group_by(geom) %>% 
    summarise(minlat = min(y), maxlat = max(y),
              minlon = min(x), maxlon = max(x)) %>% 
    as.data.frame() %>% 
    mutate(area = terra::expanse(estate_geodata_raw(),unit = "km"))
  
  df %>% left_join(df_geo, by = "geom")
}

prepped_estate_data <- function() {
  
  estate_data_raw()  %>% 
    mutate(geom = row_number()) %>% 
    rename(estate = NAME) %>% 
    rename(state_fips = STATEFP)%>% 
    rename(county_fips = COUNTYFP)%>% 
    rename(estate_fips = ESTATEFP)%>% 
    rename(center_lon = INTPTLON)%>% 
    rename(center_lat = INTPTLAT) %>% 
    mutate(center_lon = as.numeric(center_lon))%>% 
    mutate(center_lat = as.numeric(center_lat))%>% 
    estate_geo_info() %>% 
    select(-NAMELSAD, -NAMELSAD, -LSAD, -CLASSFP, -MTFCC,
           -FUNCSTAT, -ALAND, -AWATER) 
  
}

build_estates <- function() {
  
  df <- prepped_estate_data() 
  
  save_estates(df)
  
}

save_estates <- function(df) {
  
  saveRDS(df, "./data/estates.rds")
}

estate_maplims <- function(estates) {
  
  est <- estate[1]
  lims <- estate_data() %>%  filter(estate %in% estates) %>% 
    select(matches("(min|max)(lat|lon)")) %>% 
    summarise(minlat = min(minlat),
              maxlat = max(maxlat),
              minlon = min(minlon),
              maxlon = max(maxlon)) %>% unlist()
  
  
  lims
}
##
##################################################################################

ggplot_estates <- function(isl=c("STX","STT","STJ"),estates = NULL, 
                           subdistricts = FALSE,
                           show_others = TRUE,
                           label = TRUE, 
                           label_color = "black",
                           label_others = FALSE,
                           title = "",
                           maplims = NULL, 
                           fill_on="#BBBBBBFF",
                           fill_off="#00000000",
                           verbose=FALSE, year = 2022) {
  
  #===============================================
  #
  # mapping
  #
  #
  require(ggplot2)
  
  ##############################################################
  ##
  ##      use ggplot2
  
  island <- islands(isl)
  fips<-island %>% pull(county_fips)
  
  if(subdistricts) {
    # est <- subdistrict_estate_geodata() 
    df_est <- subdistrict_estates() %>% 
      mutate(rn = row_number()) %>% 
      mutate(estate = subdist_estate)
    
    data <- subdistrict_estate_geodata() %>% 
      mutate(geom = factor(geom)) 
    
    
    
  } else {
    
    df_est <- estate_data() %>% 
      mutate(rn = row_number()) 
    
    data <- estate_geodata() %>% 
      mutate(geom = factor(geom)) 
    
  }
  
  
  if(is.null(estates)) {
    est_nums <- 1:nrow(df_est)
  } else {
    est_nums <- which_estates(df_est, estates, fips = fips)
  }
  
  # other_nums <- 1:nrow(df_est)
  # other_nums <- other_nums[-est_nums]
  
  if(is.null(maplims)) {
    if (subdistricts){
      maplims <- subdistrict_maplims(isl = isl, subds = subdistricts)
    } else if(show_others) {
      maplims <- island_maplims(isl)
    } else  {  
      maplims <- estate_maplims(names(est_nums))
    }
    
  }
  
  browser()
  
  show.axes<-FALSE
  
  if(!show_others) {
    data <- data %>% filter(geom %in% est_nums)
    
    df_est <- df_est %>% filter(rn%in%est_nums)
    
    fill_colors<-rep(fill_on,nrow(df_est))
    
  } else {
    
    fill_colors<-rep(fill_off,nrow(df_est))
    fill_colors[est_nums]<-fill_on
    if(!label_others) df_est <- df_est %>% 
      mutate(estates = ifelse(rn %in% est_nums,estate,"")) 
    
  }
  
  
  label_colors<-rep(label_color,nrow(df_est))
  
  gpl <- ggplot2::ggplot() + 
    geom_polygon(data = data, aes(x = x, y = y, group = interaction(geom, part), fill =  geom),
                 colour = "black") +
    
    coord_map(clip = "on") +
    
    scale_fill_manual(values=fill_colors, guide = "none") +
    
    ylim(maplims["minlat"],maplims["maxlat"]) +
    xlim(maplims["minlon"],maplims["maxlon"]) +
    
    theme(panel.background = element_blank(), 
          legend.background = element_blank(),
          legend.key = element_blank(),
          legend.position = "none"
    )
  
  if(label) {
    
    gpl <-gpl + geom_text(data = df_est, 
                          mapping = aes(x = center_lon, y=center_lat, 
                                        label = estate, color = fips), 
                          size = 2.9) +
      
      scale_color_manual(values=label_colors, guide = "none") 
    
  }
  
  if(nchar(title)>0) gpl <- gpl + ggtitle(title)
  
  gpl
  
}

###################  end ggplot_estates  #########################################
##
##################################################################################




download_estates <- function(year = 2022, destfile = NULL) {
  
  
  if(is.null(destfile)) destfile <- paste0("./data_raw/zipfiles/tl_", year, "_78_estate.zip")
  
  url <- paste0("https://www2.census.gov/geo/tiger/TIGER", year, "/ESTATE/tl_", year, "_78_estate.zip")
  
  download.file(url = url, destfile = destfile)
  
  unzip(destfile)
}

which_estates <- function(df, estates, fips, logical = FALSE) {
  
  yn <- 
    mapply(function(nm,fp) {
      x <- sapply(estates, function(est_in) {
        
        grepl(est_in,nm, ignore.case = TRUE) && fp == fips
        
      })
      any(x)
    }, df$estate, df$county_fips)
  
  if(logical) yn else which(yn)
  
}

