
require(dplyr, quietly = TRUE, warn.conflicts = FALSE)
require(ggplot2, quietly = TRUE, warn.conflicts = FALSE)
require(scales,quietly = T)  #for transparency

source("./code/libs/lib_shpfiles.R")
source("./code/libs/lib_islands.R")
source("./code/libs/lib_estates.R")
source("./code/libs/lib_subdistrict_estates.R")
source("./code/libs/lib_misc.R")

subdistricts_shp_filename <- function(year = 2022) {
  shp_filename(year = year, state = "VI", item = "cousub")
}

subdistrict_geodata_raw <- function(year=2022) {
  
  terra::vect(subdistricts_shp_filename()) 
  
}

build_subdistricts <- function() {
 
  init_subdistricts()
  build_subdistrict_geodata()
  add_subdistrict_limits()
}

init_subdistricts <- function() {
  
  shp_file <- subdistricts_shp_filename()
  
  df <- shp_data(shp_file)  %>% 
    mutate(geom = row_number())%>% 
    filter(LSAD == 24) %>% 
    mutate(center_lat = as.integer(INTPTLAT),
           center_lon = as.integer(INTPTLON)) %>% 
    select(subdistrict = NAME, geom, county_fips = COUNTYFP, state_fips = STATEFP, 
           subdistrict_fips = COUSUBFP, 
           center_lat ,
           center_lon) %>% 
    arrange(county_fips,subdistrict_fips) %>% 
    group_by(county_fips) %>% mutate(subdist_num =row_number()) %>% 
    as.data.frame() %>% 
    relocate(subdist_num, .after = subdistrict)
    
    save_subdistricts(df)
    
}

save_subdistricts <- function(df) {

  saveRDS(df, file = "./data/subdistricts/subdistricts.rds")
}

subdistricts <- function() {
  
  readRDS(file = "./data/subdistricts/subdistricts.rds")
  
}

build_subdistrict_geodata <- function() {
  
  geoms <- subdistricts() %>% pull(geom)
  
  df <- terra::geom(subdistrict_geodata_raw()) %>% 
    as.data.frame() %>% 
    filter(geom %in% geoms)
  
  save_subdistrict_geodata(df)
  
  
}

save_subdistrict_geodata <- function(df) {
  
  saveRDS(df, "./data/subdistricts/subdistricts_geo.rds")
}

subdistrict_geodata <- function() {
  
  readRDS("./data/subdistricts/subdistricts_geo.rds")
}


updte_subdistrict_estates <- function(isl, estates, subd) {
  
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

add_subdistrict_limits <- function() {
  
  df_subd <- subdistricts() %>% 
    subdistrict_geo_info()
  
  save_subdistricts(df_subd)
}

subdistrict_geo_info <- function(df) {
  
  geoms <- df %>% pull(geom)
  
  df_geo <- subdistrict_geodata() %>% 
    filter(geom %in% geoms) %>% 
    group_by(geom) %>% 
    summarise(minlat = min(y), maxlat = max(y),
              minlon = min(x), maxlon = max(x)) %>% 
    as.data.frame()
  
  df_raw <- subdistrict_geodata_raw() 
  
  df_geo <- df_geo %>% 
    mutate(area = terra::expanse(
      df_raw[geoms],
      unit = "km"))
  
  df %>% left_join(df_geo, by = "geom")
}

map_subdistricts <- function(isl = c("stx","stj","stt"), ...) {
  
  require(dplyr)
  
  print(ggplot_subdistricts(isl[1],  ...))
  
}

ggplot_subdistricts <- function(isl=c("STX","STT","STJ"), 
                                subdistricts = NULL, 
                                show_others = FALSE, 
                                label = TRUE, lbl_size = 3.2,
                                maplims = NULL, 
                                fill_on="#BBBBBBFF",
                                fill_off="#00000000",
                                outline = FALSE,
                                verbose=FALSE,
                                year = 2022, 
                                file = NULL, ...) {
  
  #===============================================
  
  isl<-isl[1]  
  
  fips <- islands(isl) %>% pull(county_fips)  
  
  ##############################################################
  ##
  ##      use ggplot2
  
  est <- subdistrict_estate_geodata() #%>% as.data.frame()
  subd_outlines <- subdistrict_geodata() #%>% as.data.frame()
  
  ## filter for islands (by fips) and, if subdistricts is not null, subdistrict
  
  df_est <- subdistrict_estates() %>% filter(county_fips == {{fips}}) #else
  
  if(!show_others && !is.null(subdistricts)) {
    
    df_est <- df_est %>% filter(subdist %in% subdistricts)
    
  }
  
  est_geoms <- df_est %>% pull(geom)
  est <- est %>% filter(geom %in% est_geoms)
  
  fill_colors <- palette.colors(palette = "ggplot2")
  fill_colors[1] <- "#aadddd"
  fill_colors[8] <- "#ffff22"
  fill_colors <- c(fill_colors,"#aaffaa","#cc1144", "#11dddd")
  
  show.axes<-FALSE
  
  data <- est %>% 
    left_join(df_est %>% select(geom,subdist), by = "geom") %>% 
    mutate(geom = factor(geom))%>% 
    mutate(subdist = factor(subdist))
  
  data_subd <- subd_outlines %>% 
    mutate(geom = factor(geom))
  
  if(is.null(maplims)) {
    if(is.null(subdistricts)) {
      maplims <- island_maplims(isl)
    } else {
      df <- df_est %>% filter(subdist %in% subdistricts) %>% 
        select(matches("(min|max)(lat|lon)"))
      
      maplims <- numeric()
      maplims["minlat"] <- min(df["minlat"],na.rm = T)
      maplims["minlon"] <- min(df["minlon"],na.rm = T)
      maplims["maxlat"] <- max(df["maxlat"],na.rm = T)
      maplims["maxlon"] <- max(df["maxlon"],na.rm = T)
    }
    
  }
  
  ## ==========================================================================
  ##
  ##    make the plot
  ##
  
  gpl <- ggplot2::ggplot() +
    
    coord_map() +
    
    ylim(maplims["minlat"],maplims["maxlat"]) +
    xlim(maplims["minlon"],maplims["maxlon"]) + 
    
    geom_polygon(data = data, aes(x = x, y = y,  group = interaction(geom, part), fill =  subdist),
                 colour = "black") +
    
    scale_fill_manual(values=fill_colors, guide = "none") 
    
    if(outline) {
      gpl <- gpl +
        geom_polygon(data = data_subd, aes(x = x, y = y, group = interaction(geom, part), fill =  NA),
                     colour = "red", fill = NA, size = 1.5)
    }
  
  gpl <- gpl + theme(panel.background = element_blank(), 
                     legend.background = element_blank(),
                     legend.key = element_blank())
  
  if(label) gpl <- gpl +
    geom_text(data = df_est, mapping = aes(x = center_lon, y=center_lat, label = subdist_estate), size = lbl_size) 
  
  if(!is.null(file)) {
    ggsave(filename = file,plot = gpl, ...)
  }
  
  
  gpl 
}

###################  end ggplot_subdistricts  #########################################



source_run_subd_builds <- function(prefix, order = NULL) {
  
  files <- list.files("./code/libs/subdistricts", full.names = TRUE) %>% 
    grep(prefix, ., value = TRUE)
  
  if(!is.null(order)) {
    files <- files[order]
  }
  
  invisible(sapply(files,function(file) {
    source(file)
    
    func <- readLines(file) %>%
      grep("[<]-.*function",., value = TRUE) %>%
      gsub("(.*) [<].*","\\1",.) %>%
      stringr::str_trim(.)
    
    # print(paste0("trying: ",func))
    
    do.call(func, args = list())
    rm(list = c(func), envir = .GlobalEnv)
    
  })
  )
}

source_subd_builds <- function(prefix = NULL) {
  
  files <- list.files("./code/libs/subdistricts", full.names = TRUE) 
  
  if(!is.null(prefix)) {
    files <- files %>% 
      grep(prefix, ., value = TRUE)
  }
  
  invisible(sapply(files, source))
}

remove_subd_sources <- function() {
  
  files <- list.files("./code/libs/subdistricts", full.names = TRUE) #%>% 
  #grep(prefix, ., value = TRUE)
  
  invisible(sapply(files,function(file) {
    #    source(file)
    
    func <- readLines(file) %>%
      grep("[<]-.*function",., value = TRUE) %>%
      gsub("(.*) [<].*","\\1",.) %>%
      stringr::str_trim(.)
    
    #    do.call(func, args = list())
    
    tryCatch(
      rm(list = c(func), envir = .GlobalEnv), 
      error = function(e) {}, 
      warning = function(w) {}
      
    )
  })
  )
}


rebuild_subdistrict_estates_all <- function() {
  
  init_subdistricts()
  build_subdistrict_estates()
  build_subdistrict_estate_geodata()
  
  source_run_subd_builds("fix_")
  
  df_subd_est <- subdistrict_estates() %>% 
    select(-matches("(min|max)(lat|lon)"), -area) %>% 
    estate_geo_info()
  
  #save_subdistrict_estates(df = df_subd_est) 
  
  source_run_subd_builds("stx_", order = c(1,2,3,7,8,9,5,6,4))
  
  map_subdistricts("stx")
}

center_latlon <- function(df_est, df_geo) {
  
  lats <- df_geo %>% pull(y)
  lons <- df_geo %>% pull(x)
  
  df_est %>% 
    mutate(center_lat = (min(lats) + max(lats))/2) %>% 
    mutate(center_lon = (min(lons) + max(lons))/2) 
}


