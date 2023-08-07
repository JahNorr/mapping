load("./data/vi_estates.RData")


vi_map_limits<-function(x) {
  if (x %in% c("St. Croix","STX","Croix"))  island<-"St. Croix"
  if (x %in% c("St. Thomas","STT","Thomas"))  island<-"St. Thomas"
  if (x %in% c("St. John","STJ","John"))  island<-"St. John"
  
      return (limits[,c(island)])
}

vi_county_to_island<-function(x) {
  if(x=="010") return("STX")
  if(x=="020") return("STJ")
  if(x=="030") return("STT")
  
}

vi_estate_data<-function() {
  df<-data.frame(estates$ESTATEFP,stringsAsFactors = FALSE)
  colnames(df)[1]<-"Estate.Code"
  df$Estate.Code<-as.character(df$Estate.Code)
  df$Estate.Code<-as.integer(df$Estate.Code)
  df$Estate.Name<-estates$NAME
  df$Island<-mapply(vi_county_to_island,estates$COUNTYFP)
  return  (df)
}

vi_estate_codes<-function() return (data.frame(estates$ESTATEFP))

show_island_plot<-function(island_name,colors,show.axes=FALSE, main=NULL) {
  maplims<-vi_map_limits(island_name)
  par(mar=c(3.0, 1.5, 3, 1.5))
  
#==============================================
#   try this later
#   density=6,angle=45,
#
  plot(estates,axes = show.axes,main = main ,xlim=c(maplims["minlon"],maplims["maxlon"]),ylim=c(maplims["minlat"],maplims["maxlat"]), col=colors, border=TRUE)  #plot the species range

}
#points(samps$lon, samps$lat, pch=19, col="red", cex=0.5)  #plot my sample sites