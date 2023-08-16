
require(dplyr, quietly = TRUE, warn.conflicts = FALSE)
require(ggplot2, quietly = TRUE, warn.conflicts = FALSE)
require(maps,quietly = T)
require(mapdata,quietly = T)

require(scales,quietly = T)  #for transparency

source("./code/libs/lib_islands.R")
source("./code/libs/lib_estates.R")
source("./code/libs/lib_estates_subdist.R")


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

build_subdistrict_estates <- function() {
  
  
  df <- prepped_estate_data()  %>% 
    
    # this will hold the new name (saving the original in estate)
    
    mutate(subdist_estate = estate) %>% 
    
    ##  the subdistrict number 
    ##  (used with the county_fips to identify the subdistrict)
    
    mutate(subdist = NA)
  
  save_subdistrict_estates(df)
}

build_subdistrict_estate_geodata <- function() {
  
  df <- as.data.frame(terra::geom(estate_geodata_raw())) 
  
  save_subdistrict_estate_geodata(df)
  
  
}

save_subdistrict_estates <- function(df) {
  
  saveRDS(df, "./data/subdistrict_estates.rds")
}

subdistrict_estates <- function() {
  
  readRDS("./data/subdistrict_estates.rds")
}

save_subdistrict_estate_geodata <- function(df) {
  
  saveRDS(df, "./data/subdistrict_estates_geo.rds")
}

subdistrict_estate_geodata <- function(df) {
  
  readRDS("./data/subdistrict_estates_geo.rds")
}


update_subdistricts <- function(isl, estates, subd) {
  
  df_est <- subdistrict_estates()
  
  fips <- islands(isl) %>% 
    pull(county_fips) 
  
  estates <- estates %>% paste0("^",.,"$")
  
  df <- df_est %>% select(estate = subdist_estate, county_fips) 
  
  estates <-  estates %>% 
    gsub("(","\\(", . , fixed = TRUE) %>% 
    gsub(")","\\)", ., fixed = TRUE)
  
  est_nums <- which_estates(df = df, estates = estates, fips = fips ,logical = T)
  
  df_est <- df_est %>% 
    mutate(subdist = ifelse(subdist == subd, NA, subdist)) %>% 
    mutate(subdist = replace(subdist, est_nums, subd))
  
  save_subdistrict_estates(df_est)
}

subdist_from_func <- function() {
  
  fname2 <- as.character(sys.calls())[1]
  fname1 <- as.character(sys.calls())[2]
  
  if(grepl("st[xjt]_[0-9]{1,2}",fname1)) fname <- fname1 else fname <- fname2 
  
  gsub(".*_([0-9]{1,2})\\(.*\\)$","\\1",fname)
}

isl_from_func <- function() {
  
  fname2 <- as.character(sys.calls())[1]
  fname1 <- as.character(sys.calls())[2]
  
  if(grepl("st[xjt]_[0-9]{1,2}",fname1)) fname <- fname1 else fname <- fname2 
  
  gsub(".*_(st.)_([0-9]{1,2})\\(.*\\)$","\\1",fname)
}



source_subd_builds <- function() {
    files <- list.files("./code/libs/subdistricts", full.names = TRUE)
    
    sapply(files, source)
}

# build_subdistricts <- function() {
# 
#   source_subd_builds()
#   
#   df_subdistricts <- subdistricts()
#   
#   build_
# }

overlapping_subdistricts <- function() {
  
  
}

map_subdistricts <- function(isl, label = TRUE) {
  
  require(dplyr)

  df_est <- subdistrict_estates()
  
  fips <- islands(isl) %>% 
    pull(county_fips) 
  
  maplims <- island_maplims(isl)
  
  print(ggplot_subdistricts(isl,df_estates, maplims = maplims, label = label, lbl_size = 2.0))
  
}

ggplot_subdistricts <- function(isl=c("STX","STT","STJ"),estates = NULL, 
                                label = TRUE, lbl_size = 2.9,
                                maplims = NULL, 
                                fill_on="#BBBBBBFF",
                                fill_off="#00000000",
                                verbose=FALSE, year = 2022) {
  
  #===============================================
  #
  # mapping
  #
  #

  
  
  ##############################################################
  ##
  ##      use ggplot2
  
  est <- subdistrict_estate_geodata() #%>% as.data.frame()
  
  # if(class(est) == "SpatVector")
  df_est <- subdistrict_estates() #else
  
  isls<-c("STX","STT","STJ")
  fips<-c("010","030","020")
  
  isl<-isl[1]  
  df_island <- islands(isl)
  
  fips <- df_island %>% pull(county_fips)
  
  est_nums<- df_est %>% filter(county_fips == {{fips}}) %>% pull(geom)
  
  if(is.null(maplims)) {
    
    maplims <- island_maplims(isl)
  }
  
  fill_colors <- palette.colors(palette = "ggplot2")
  fill_colors[1] <- "#aadddd"
  fill_colors[8] <- "#ffff22"
  fill_colors <- c(fill_colors,"#cc1144", "#11dddd","#aaffaa")
  
  show.axes<-FALSE
  
  data <- est %>% 
    left_join(df_est %>% select(geom,subdist), by = "geom") %>% 
    mutate(geom = factor(geom))%>% 
    mutate(subdist = factor(subdist))
  
  ## ==========================================================================
  ##
  ##    make the plot
  ##
  
  gpl <- ggplot2::ggplot() + 
    geom_polygon(data = data, aes(x = x, y = y, group = geom, fill =  subdist),
                 colour = "black") +
    coord_map() +
    
    scale_fill_manual(values=fill_colors, guide = "none") +
    
    ylim(maplims["minlat"],maplims["maxlat"]) +
    xlim(maplims["minlon"],maplims["maxlon"]) +
    
    theme(panel.background = element_blank(), 
          legend.background = element_blank(),
          legend.key = element_blank())
  
  if(label) gpl <- gpl +
    geom_text(data = df_est, mapping = aes(x = center_lon, y=center_lat, label = subdist_estate), size = lbl_size) 
  gpl 
}

###################  end ggplot_subdistricts  #########################################



rebuild_subdistrict_estates_all <- function() {
  
  source("./code/libs/subdistricts/sub_stx_01.R")
  source("./code/libs/subdistricts/sub_stx_02.R")
  source("./code/libs/subdistricts/sub_stx_03.R")
  source("./code/libs/subdistricts/sub_stx_05.R")  
  source("./code/libs/subdistricts/sub_stx_06.R")
  source("./code/libs/subdistricts/sub_stx_07.R")
  source("./code/libs/subdistricts/sub_stx_08.R")
  source("./code/libs/subdistricts/sub_stx_09.R")
  source("./code/libs/subdistricts/sub_stx_splitme.R")
  
  source("./code/libs/subdistricts/fix_body_slob.R")
  source("./code/libs/subdistricts/fix_barren_spot_west.R")
  source("./code/libs/subdistricts/fix_vi_corp_land.R")
  source("./code/libs/subdistricts/fix_cane_garden.R")
  source("./code/libs/subdistricts/fix_carlton_north.R")
  source("./code/libs/subdistricts/fix_diamond_west.R")
  source("./code/libs/subdistricts/fix_whim_east.R")
  source("./code/libs/subdistricts/fix_river.R")
  
  
  build_subdistrict_estates()
  build_subdistrict_estate_geodata()
  
  fix_body_slob()
  fix_VI_Corp_land()
  fix_barren_spot_west()
  fix_cane_garden()
  fix_diamond_west()
  fix_whim_east()
  fix_river()
  fix_carlton_north()
  
  build_sub_stx_01()
  build_sub_stx_02()
  build_sub_stx_03()
  build_sub_stx_07()
  build_sub_stx_08()
  build_sub_stx_09()
  build_sub_stx_05()
  build_sub_stx_06()
  build_sub_stx_99()
  
  map_subdistricts("stx")
}

center_latlon <- function(df_est, df_geo) {
  
  lats <- df_geo %>% pull(y)
  lons <- df_geo %>% pull(x)
  
  df_est %>% 
    mutate(center_lat = (min(lats) + max(lats))/2) %>% 
    mutate(center_lon = (min(lons) + max(lons))/2) 
}


