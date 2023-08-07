
estate_data <- function(year=2022, useOGR = TRUE) {
  
  shp_file <- paste0("./data_raw/tl_", year,"_78_estate.shp")
  
  if (useOGR) rgdal::readOGR(shp_file,verbose = F) else terra::vect(shp_file) 
  
}

ggplot_estates<-function(isl=c("STX","STT","STJ"),estates = NULL,color_on="#BBBBBBFF",color_off="#00000000",verbose=FALSE, year = 2022) {
  
  #===============================================
  #
  # mapping
  #
  #
  require(maps,quietly = T)
  require(mapdata,quietly = T)
  require(maptools,quietly = T)  #for shapefiles
  require(scales,quietly = T)  #for transparency
  require(rgdal,quietly = T)
  
  est<-estate_data(year, useOGR = T)
  
  islands<-c("St. Croix","St. Thomas","St. John")
  isls<-c("STX","STT","STJ")
  fips<-c("010","030","020")
  
  isl<-isl[1]
  isln<-which(isls==isl)
  
  island <- islands[isln]
  fips<-fips[isln]
  
  df_est <- est %>% as.data.frame() %>%  select(NAME,COUNTYFP)
  
  est_nums<-
    which(
      mapply(function(nm,fp) {
        
        x <- sapply(estates, function(est_in) {
          grepl(est_in,nm) && fp == fips
        })
        x <- any(x)
      }, df_est$NAME, df_est$COUNTYFP)
      
    )

  lim<-c(
    c(17.729,17.731,-64.90,-64.57),
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
  
  
  # colors<-alpha("darkgreen",0.1+df_fp$pop_val*0.9)
  colors<-rep(color_off,length(est))
  
  colors[est_nums]<-color_on
  
  show.axes<-FALSE
  
  ggplot2::ggplot() + geom_polygon(data = est, aes(x = long, y = lat, group = group), 
                                   colour = "black", fill = NA) +
    coord_map() +
    
    ylim(maplims["minlat"],maplims["maxlat"]) + 
    xlim(maplims["minlon"],maplims["maxlon"]) +
    theme(panel.background = element_blank())
}

###################  end ggplot_estates  #########################################
##
##################################################################################

plot_estate<-function(isl=c("STX","STT","STJ"),estates = NULL,color_on="#BBBBBBFF",color_off="#00000000",verbose=FALSE, year = 2022) {
  
  #===============================================
  #
  # mapping
  #
  #
  require(maps,quietly = T)
  require(mapdata,quietly = T)
  require(maptools,quietly = T)  #for shapefiles
  require(scales,quietly = T)  #for transparency
  require(rgdal,quietly = T)
  
  est<-estate_data(year)
  
  islands<-c("St. Croix","St. Thomas","St. John")
  isls<-c("STX","STT","STJ")
  fips<-c("010","030","020")
  
  isl<-isl[1]
  isln<-which(isls==isl)
  
  island <- islands[isln]
  fips<-fips[isln]
  
  df_est <- est %>% as.data.frame() %>%  select(NAME,COUNTYFP)
  
  est_nums<-
    which(
      mapply(function(nm,fp) {
        
        x <- sapply(estates, function(est_in) {
          grepl(est_in,nm) && fp == fips
        })
        x <- any(x)
      }, df_est$NAME, df_est$COUNTYFP)
      
    )
  browser()
  
  
  lim<-c(
    c(17.729,17.731,-64.90,-64.57),
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
  
  
  # colors<-alpha("darkgreen",0.1+df_fp$pop_val*0.9)
  colors<-rep(color_off,length(est))
  
  colors[est_nums]<-color_on
  
  show.axes<-FALSE
  
  par() # set cex with par()
  par(mar=c(3.0, 3.0, 1.5, 1.5),cex.lab=12.5, cex.main = 0.8)
  
  plot(est,axes=show.axes, main = island, 
       xlim=c(maplims["minlon"],maplims["maxlon"]),
       ylim=c(maplims["minlat"],maplims["maxlat"]), 
       col=colors, border=TRUE)  
  
  text(est$INTPTLON, est$INTPTLAT,est$NAME, col = 'red')
}

download_estates <- function(year = 2022, destfile = NULL) {
  
  
  if(is.null(destfile)) destfile <- paste0("./data_raw/zipfiles/tl_", year, "_78_estate.zip")
  
  url <- paste0("https://www2.census.gov/geo/tiger/TIGER", year, "/ESTATE/tl_", year, "_78_estate.zip")
  
  download.file(url = url, destfile = destfile)
}

unzip(destfile)

