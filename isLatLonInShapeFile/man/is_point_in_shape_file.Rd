% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/is point in shapefile.R
\name{is_point_in_shape_file}
\alias{is_point_in_shape_file}
\title{Is a latitude and longitude point in a matrix}
\usage{
is_point_in_shape_file(file_path, lon, lat, toGraph = FALSE, pointSize = 5)
}
\arguments{
\item{file_path}{Path to your shapefile}

\item{lon}{Longitude (can be a vector)}

\item{lat}{Latitude (can be a vector)}

\item{toGraph}{Boolean to tell the function to also output a graph. Useful for debugging.}

\item{pointSize}{numeric value that sets size of graph points.}
}
\description{
This function loads a shapefile, then checks if the latitude and longitude pairs supplied to it are in the shapefile.
}
\details{
If the map show that your point should be inside the shapefile, but is returning false, it's probably because
there is no polygon over that area. Easiest way to fix this is to try a different shapefile of the same thing.

The reason for this 'bug' is because the polygons are all surrounding the area where the point is, instead of encompassing
it. e.g. for a map of berlin inside the berlin wall, the polygons form the wall, while the inside of berlin is just
whitespace. This case will return false, even though you can tell that it is true.
}
