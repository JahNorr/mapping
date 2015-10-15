
resolve_color<-function(color) {
  
  if (class(color)=="matrix") {
    red<-color[1]
    green<-color[2]
    blue<-color[3]
    color<-rgb(red,green,blue,maxColorValue = 255)
  } else {
    if (color %in% colors()) {
      color<-resolve_color(col2rgb(color))
    }
  }
  return (color)
  
}