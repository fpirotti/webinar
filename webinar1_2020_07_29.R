#' To add functionalities you must load the specific libraries
#' REMEMBER to make sure that they are installed in your system
#' if not you can install in RStudio on menù => Tools => Install Packages
#' or directly with command 
#' install.packages("NAME OF PACKAGE")
library(sf)
library(mapview)
library(raster)
 
#' My custom CRS projection Lambert Conical Conformal NOT secant but tangent at lat=45.827 and lon=11.625
myproj <- "+proj=lcc +lat_1=45.827  +lat_2=45.827  +lat_0=45.827 +lon_0=11.625 +x_0=4000000 +y_0=2800000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

#' my points (in lat long) over the regular grid/lattice these points are 666.67 m apart
grid.points <- st_read( "data/webinar1_2020_07_29/points.shp" )

#' convert my points from lat long to my custom CRS projection
grid.points.myproj <-   grid.points %>% st_transform(myproj)

#' run this line to view
#'  mapview( grid.points.myproj )

#' trick to create a rhomboid with a certain distance from the point ("radius")
#' half the distance between points (333.33 m)
nodes.buffered<-st_buffer(grid.points.myproj, 333.33, nQuadSegs = 1)


#' More complex function the includes another function
st_bbox_by_feature = function(geom) {
  
  ## make sure that object "geom" becomes a geometry object;
  ## geom is your data set with all the single geometries/polygons
  geom2 = st_geometry(geom)
  
  #' Function to:
  #' (a) take a single geometry, 
  #' (b) create a bounding box with st_bbox and 
  #' (c) convert the bbox object to a new geometry (a square)
  f <- function(single.geom) { 
    st_as_sfc( st_bbox(single.geom,), crs=myproj)
  }
  
  #' This line calls the function above LOOPING over each single geometry (lapply)
  #'  in the geom2 object 
  do.call("c", lapply(geom2, f))
}

#' This line calls the above function
tiles <- st_bbox_by_feature( nodes.buffered )
#' Assign the CRS to the new dataset
tiles <- tiles %>% st_set_crs(myproj)

#' Convert to Latitude and Longitude for ingesting into Google Earth Engine
tiles.latlng <- tiles %>% st_transform("+init=epsg:4326")
#' Save it to a shapefile ... this shapefile will be uploaded to your 
#' Google Earth Engine space
st_write(tiles.latlng, "data/tiles.shp" )

############
#' DO THINGS IN GOOGLE EARTH
#' AND EXPORT THE RESULTS TO A SHAPEFILE CALLED
#' tiles_withData.shp
############

#' Read the file exported from GEE
grid.points.gee <- st_read( "data/tiles_withData.shp" )
#' Create a new attribute column with the interquartile range
#' (The difference between p75 and p25)
grid.points.gee$iqr<-grid.points.gee$p75 - grid.points.gee$p25

#' Draw some results!!!!
pal = mapviewPalette("mapviewSpectralColors")
mapview( grid.points.gee, col.regions = pal(100), zcol = "p50", 
         color=NULL, alpha.regions=0.8  ) 

mapview( grid.points.gee, col.regions = pal(100), zcol = "p10", 
         color=NULL, alpha.regions=0.8  ) 
  
mapview( grid.points.gee, col.regions = pal(100), zcol = "iqr", 
         color=NULL, alpha.regions=0.8  ) 
