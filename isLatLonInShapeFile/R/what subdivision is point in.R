#' Is a latitude and longitude point in a matrix
#'
#' @description This function takes a shape, the column name of the regions in the shape, and lon and lat
#' @details If the map show that your point should be inside the shapefile, but is returning false, it's probably because
#' there is no polygon over that area. Easiest way to fix this is to try a different shapefile of the same thing. Note: This
#' function probably won't work as you expect.
#'
#' @param shp shape (S4)
#' @param colname Column name of the layer
#' @param lon Longitude (can be a vector)
#' @param lat Latitude (can be a vector)
#' @export
what_subdivision_is_point_in <- function(shp,colname,lon,lat)
{
  shpnames = shp@data[colname]
  shpnames = shpnames[[1]]
  for (i in 1:length(shpnames))
  {
    shpname = shpnames[i]
    new_shp = shp[which(shp@data[colname]==shpname),]
    if (is_point_in_shape(new_shp,lon,lat))
    {
      return(shpname)
    }

  }
  return(NA)
}
