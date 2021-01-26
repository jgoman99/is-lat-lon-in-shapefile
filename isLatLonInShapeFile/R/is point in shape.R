#' Is a latitude and longitude point in a matrix
#'
#' @description This function takes a shape, then checks if the latitude and longitude pairs supplied to it are in the shape
#'
#' @details If the map show that your point should be inside the shapefile, but is returning false, it's probably because
#' there is no polygon over that area. Easiest way to fix this is to try a different shapefile of the same thing.
#'
#' The reason for this 'bug' is because the polygons are all surrounding the area where the point is, instead of encompassing
#' it. e.g. for a map of berlin inside the berlin wall, the polygons form the wall, while the inside of berlin is just
#' whitespace. This case will return false, even though you can tell that it is true.
#'
#' @param shp shape (S4)
#' @param lon Longitude (can be a vector)
#' @param lat Latitude (can be a vector)
#' @param toGraph Boolean to tell the function to also output a graph. Useful for debugging.
#' @param po
is_point_in_shape_file <- function(shp,lon,lat, toGraph = FALSE, pointSize = 5)
{

  shp <- spTransform(shp, CRS("+proj=longlat +datum=WGS84"))

  dat <- data.frame(Longitude = lon,
                    Latitude = lat,
                    names = "It's a point")

  dat_ggplot <- dat



  coordinates(dat) <- ~ Longitude+Latitude

  oldw <- getOption("warn")
  options(warn = -1)
  proj4string(dat) <- proj4string(shp)
  options(warn = oldw)

  if (toGraph==TRUE)
  {

    bg = "grey92"
    fort <- fortify(shp)
    g <-    ggplot() +
      geom_polygon(data = fort, aes(long, lat, group = group, fill = hole),
                   colour = alpha("darkred", 1/2), size = 0.7) +
      scale_fill_manual(values = c("skyblue", bg)) +
      theme(panel.background = element_rect(fill = bg),
            legend.position = "none") + geom_point(data=dat_ggplot, aes(x=lon,y=lat, colour="red", size =pointSize))
    print(g)

    print("Is there a big red dot in the middle of the map you're looking at, but the function is returning false?")
    print("Try ?is_point_in_shape_file for an explanation")

  }

  results = over(dat, shp)
  inside_vec = !is.na(results[1])
  return(inside_vec)

}
