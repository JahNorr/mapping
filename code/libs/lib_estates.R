
estate_data <- function(year=2022, useOGR = TRUE) {
  
  shp_file <- paste0("./data_raw/tl_", year,"_78_estate.shp")
  
  # if (useOGR) rgdal::readOGR(shp_file,verbose = F) else 
  
  terra::vect(shp_file) 
  
}

ggplot_subdistricts <- function(isl=c("STX","STT","STJ"),estates = NULL, 
                                label = TRUE, lbl_size = 2.9,
                                maplims = NULL, color_on="#BBBBBBFF",color_off="#00000000",
                                verbose=FALSE, year = 2022) {
  
  #===============================================
  #
  # mapping
  #
  #
  require(maps,quietly = T)
  require(mapdata,quietly = T)
  #require(maptools,quietly = T)  #for shapefiles
  require(scales,quietly = T)  #for transparency
  require(rgdal,quietly = T)
  
  
  ##############################################################
  ##
  ##      use ggplot2
  
  shp_file <- "./data_raw/tl_2022_78_estate.shp"
  #est<-rgdal::readOGR(shp_file )
  
  est <- estate_data(useOGR = F) #%>% as.data.frame()
  
  # if(class(est) == "SpatVector")
  df_est <- est %>% as.data.frame() #else
  
  isls<-c("STX","STT","STJ")
  fips<-c("010","030","020")
  
  isl<-isl[1]  
  df_island <- islands() %>% filter(tolower(Abbrev) == tolower(isl))
  
  fips <- df_island %>% pull(CountyCode) %>% paste0("0", .)
  
  df_est <- est %>% as.data.frame() %>%  
    select(estate = NAME, fips = COUNTYFP, lat = INTPTLAT, lon = INTPTLON ) %>% 
    mutate(lat = as.numeric(lat)) %>% 
    mutate(lon = as.numeric(lon))
  
  est_nums<- df_estates %>% filter(est_fips == fips) %>% pull(geom)
  
  if(is.null(maplims)) {
    
    lim<-c(
      c(17.67,17.8,-64.90,-64.57),
      c(18.3,18.4,-65.047,-64.823),
      c(18.298,18.373,-64.81,-64.64)
    )
    
    limits<-matrix(lim,4,3)
    colnames(limits)<- isls
    rownames(limits)<- c("minlat","maxlat","minlon","maxlon")
    
    lim_tmp<-as.data.frame(limits[,isl])
    
    maplims<-numeric(4)
    names(maplims)<-rownames(limits)
    maplims["minlat"]<-min(lim_tmp["minlat",])
    maplims["minlon"]<-min(lim_tmp["minlon",])
    maplims["maxlat"]<-max(lim_tmp["maxlat",])
    maplims["maxlon"]<-max(lim_tmp["maxlon",])
  }
  
  colors <- palette.colors(palette = "ggplot2")
  colors[1] <- "#dddddd"
    #c("#dddddd","#aaffaa","#ffaaaa")
  
  show.axes<-FALSE
  
  data <- as.data.frame(terra::geom(est)) %>% 
    left_join(df_estates %>% select(geom,subdist), by = "geom") %>% 
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
    
    scale_fill_manual(values=colors, guide = "none") +
    
    ylim(maplims["minlat"],maplims["maxlat"]) +
    xlim(maplims["minlon"],maplims["maxlon"]) +
    
    theme(panel.background = element_blank(), 
          legend.background = element_blank(),
          legend.key = element_blank())
 
  if(label) gpl <- gpl +
    geom_text(data = df_est, mapping = aes(x = lon, y=lat, label = estate), size = lbl_size) 
  gpl 
}

###################  end ggplot_estates  #########################################
##
##################################################################################

ggplot_estates <- function(isl=c("STX","STT","STJ"),estates = NULL, maplims = NULL, color_on="#BBBBBBFF",color_off="#00000000",verbose=FALSE, year = 2022) {
  
  #===============================================
  #
  # mapping
  #
  #
  require(maps,quietly = T)
  require(mapdata,quietly = T)
  # require(maptools,quietly = T)  #for shapefiles
  require(scales,quietly = T)  #for transparency
  require(rgdal,quietly = T)
  
  
  ##############################################################
  ##
  ##      use ggplot2
  
  shp_file <- "./data_raw/tl_2022_78_estate.shp"
  #est<-rgdal::readOGR(shp_file )
  
  est <- estate_data(useOGR = F) #%>% as.data.frame()
  
  # if(class(est) == "SpatVector")
  df_est <- est %>% as.data.frame() #else
  
  islands<-c("St. Croix","St. Thomas","St. John")
  isls<-c("STX","STT","STJ")
  fips<-c("010","030","020")
  
  isl<-isl[1]
  isln<-which(isls==isl)
  
  island <- islands[isln]
  fips<-fips[isln]
  
  df_est <- est %>% as.data.frame() %>%  
    select(estate = NAME, fips = COUNTYFP, lat = INTPTLAT, lon = INTPTLON ) %>% 
    mutate(lat = as.numeric(lat)) %>% 
    mutate(lon = as.numeric(lon))
  
  est_nums<-
    which(
      mapply(function(nm,fp) {

        x <- sapply(estates, function(est_in) {
          grepl(est_in,nm) && fp == fips
        })

        x <- any(x)
      }, df_est$estate, df_est$fips)
      
    )
  
  if(is.null(maplims)) {
    
    lim<-c(
      c(17.67,17.8,-64.90,-64.57),
      c(18.3,18.4,-65.047,-64.823),
      c(18.298,18.373,-64.81,-64.64)
    )
    
    limits<-matrix(lim,4,3)
    colnames(limits)<- isls
    rownames(limits)<- c("minlat","maxlat","minlon","maxlon")
    
    lim_tmp<-as.data.frame(limits[,isl])
    
    maplims<-numeric(4)
    names(maplims)<-rownames(limits)
    maplims["minlat"]<-min(lim_tmp["minlat",])
    maplims["minlon"]<-min(lim_tmp["minlon",])
    maplims["maxlat"]<-max(lim_tmp["maxlat",])
    maplims["maxlon"]<-max(lim_tmp["maxlon",])
  }
  
  colors<-rep(color_off,length(est))
  
  colors[est_nums]<-color_on
  
  show.axes<-FALSE
  
  data <- as.data.frame(terra::geom(est)) %>% 
    mutate(geom = factor(geom))
  
  ggplot2::ggplot() + 
    geom_polygon(data = data, aes(x = x, y = y, group = geom, fill =  geom),
                 colour = "black") +
    coord_map() +
    
    geom_text(data = df_est, mapping = aes(x = lon, y=lat, label = estate), size = 2.9) +
    
    scale_fill_manual(values=colors, guide = "none") +
    
    ylim(maplims["minlat"],maplims["maxlat"]) +
    xlim(maplims["minlon"],maplims["maxlon"]) +
    
    theme(panel.background = element_blank(), 
          legend.background = element_blank(),
          legend.key = element_blank())
  
}

###################  end ggplot_estates  #########################################
##
##################################################################################

island_maplims <- function(isl){
  
  require(dplyr)
  
  if(orrr::is.integer_like(isl)) {
    
    fips <- as.integer(isl)
    isl <- islands() %>% 
      filter(CountyCode == fips) %>% 
      pull(Abbrev) %>% 
      tolower
    
    
  }

  df_stx_lims <- data.frame(isl = "stx", fips = "010",
                            minlat=17.67,maxlat = 17.8,
                            minlon = -64.90, maxlon = -64.57)
  
  df_stj_lims <- data.frame(isl = "stj",  fips = "020",
                            minlat=18.3,maxlat = 18.4,
                            minlon = -65.047, maxlon = -64.823)
  
  df_stt_lims <- data.frame(isl = "stt", fips = "030", 
                            minlat=18.298,maxlat = 18.373,
                            minlon = -64.81, maxlon = -64.64)
  
  df_lims <- rbind(df_stx_lims, df_stj_lims, df_stt_lims) %>% 
    filter(isl == {{isl}}) %>% 
    select(matches("lat|lon"))
  
  df_lims %>% unlist()
}
  
  
# 
# plot_estate<-function(isl=c("STX","STT","STJ"),estates = NULL,color_on="#BBBBBBFF",color_off="#00000000",verbose=FALSE, year = 2022) {
#   
#   #===============================================
#   #
#   # mapping
#   #
#   #
#   require(maps,quietly = T)
#   require(mapdata,quietly = T)
#   require(maptools,quietly = T)  #for shapefiles
#   require(scales,quietly = T)  #for transparency
#   require(rgdal,quietly = T)
#   
#   est<-estate_data(year)
#   
#   islands<-c("St. Croix","St. Thomas","St. John")
#   isls<-c("STX","STT","STJ")
#   fips<-c("010","030","020")
#   
#   isl<-isl[1]
#   isln<-which(isls==isl)
#   
#   island <- islands[isln]
#   fips<-fips[isln]
#   
#   df_est <- est %>% as.data.frame() %>%  select(NAME,COUNTYFP)
#   
#   est_nums<-
#     which(
#       mapply(function(nm,fp) {
#         
#         x <- sapply(estates, function(est_in) {
#           grepl(est_in,nm) && fp == fips
#         })
#         x <- any(x)
#       }, df_est$NAME, df_est$COUNTYFP)
#       
#     )
#   
#   
#   lim<-c(
#     c(17.729,17.731,-64.90,-64.57),
#     c(18.3,18.4,-65.047,-64.823),
#     c(18.298,18.373,-64.81,-64.64)
#   )
#   
#   limits<-matrix(lim,4,3)
#   colnames(limits)<- isls
#   rownames(limits)<- c("minlat","maxlat","minlon","maxlon")
#   
#   lim_tmp<-as.data.frame(limits[,isl])
#   
#   maplims<-numeric(4)
#   names(maplims)<-rownames(limits)
#   maplims["minlat"]<-min(lim_tmp["minlat",])
#   maplims["minlon"]<-min(lim_tmp["minlon",])
#   maplims["maxlat"]<-max(lim_tmp["maxlat",])
#   maplims["maxlon"]<-max(lim_tmp["maxlon",])
#   
#   
#   # colors<-alpha("darkgreen",0.1+df_fp$pop_val*0.9)
#   colors<-rep(color_off,length(est))
#   
#   colors[est_nums]<-color_on
#   
#   show.axes<-FALSE
#   
#   par() # set cex with par()
#   par(mar=c(3.0, 3.0, 1.5, 1.5),cex.lab=12.5, cex.main = 0.8)
#   
#   plot(est,axes=show.axes, main = island, 
#        xlim=c(maplims["minlon"],maplims["maxlon"]),
#        ylim=c(maplims["minlat"],maplims["maxlat"]), 
#        col=colors, border=TRUE)  
#   
#   text(est$INTPTLON, est$INTPTLAT,est$NAME, col = 'red')
# }

download_estates <- function(year = 2022, destfile = NULL) {
  
  
  if(is.null(destfile)) destfile <- paste0("./data_raw/zipfiles/tl_", year, "_78_estate.zip")
  
  url <- paste0("https://www2.census.gov/geo/tiger/TIGER", year, "/ESTATE/tl_", year, "_78_estate.zip")
  
  download.file(url = url, destfile = destfile)
  
  unzip(destfile)
}

islands <- function() {
  usvi::vi_islands
}

which_estates <- function(df, estates, fips) {

    yn <- which(
      mapply(function(nm,fp) {

        x <- sapply(estates, function(est_in) {
          grepl(est_in,nm) && fp == fips
        })
        x <- any(x)
      }, df$estate, df$fips)

    )
    
    yn

}
