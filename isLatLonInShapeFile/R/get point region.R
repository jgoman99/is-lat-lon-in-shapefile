#' Is a latitude and longitude point in a matrix
#'
#' @description This function takes a shape, the region_name of the region you want to check eg. "NAME_4", vector of lon
#' vector of lat values, and returns the regions the lat/lon points are in
#'
#' @param shp shape (S4)
#' @param region_name String for region you want to check
#' @param lon Longitude (can be a vector)
#' @param lat Latitude (can be a vector)
#' @export
get_point_region_from_shape <- function(shp,region_name,lon,lat)
{
  shp_fort = broom::tidy(shp, region = region_name)
  center_tbl = shp_fort %>% group_by(id) %>% summarize(lat_mean = mean(lat), lon_mean=mean(long))
  regions = character(length = length(lon))

  for (i in 1:length(lon))
  {
    dist_tbl = center_tbl %>% rowwise() %>% mutate(dist = distHaversine(c(lat[i],lon[i]), c(lat_mean, lon_mean)))
    for (j in 1:nrow(dist_tbl))
    {
      print(j)
      region_id = dist_tbl$id[which(dist_tbl$dist==sort(dist_tbl$dist,decreasing = FALSE)[j])]
      new_shp = shp[which(as.data.frame(shp)[,region_name]==region_id),]
      if (is_point_in_shape(new_shp,lon[i],lat[i]))
      {
        regions[i] = region_id
        break

      }


    }
  }

  return(regions)

}
