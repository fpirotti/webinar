#' To add functionalities you must load the specific libraries
#' REMEMBER to make sure that they are installed in your system
#' if not you can install in RStudio on menù => Tools => Install Packages
#' or directly with command 
#' install.packages("NAME OF PACKAGE")
library(sf)
library(mapview)
library(raster)
library(RJSONIO)

## see GEE https://code.earthengine.google.com/?accept_repo=users/2020_Kanan/summer_webinar_series

## here we read the GeoJSON that was created from GEE
############ FUNCTIONS FIRST
#### TABLE RESHAPING #########
reshapeTable<-function(geojson){ 
  out<-list() 
  # iterate over files
  for( feature.name in names(geojson$features$properties) ){ 
    pp<-reshape2::melt( geojson$features$properties[[ feature.name ]] )
    out[[ feature.name ]] <- (pp$value)
  }
  out.table<-as.data.frame(out) 
  ## remove all things not "VV_?" or "VH_?"
  out.table[ , grep(value=T, 'angle_', names(out.table) )]<-NULL 
  out.table[ , grep(value=T, '_count', names(out.table) )]<-NULL 
  out.table$Orbit<-as.factor(out.table$Orbit)
  
  ## see paper
  out.table$VHRange_p75p25 <-out.table$VH_p75 - out.table$VH_p25
  out.table$VVRange_p75p25 <-out.table$VV_p75 - out.table$VV_p25
  
  out.table$VHRange_p90p10 <-out.table$VH_p90 - out.table$VH_p10
  out.table$VVRange_p90p10 <-out.table$VV_p90 - out.table$VV_p10
  
  for(i in c("10", "25", "50", "75", "90")){
    out.table[[sprintf("VVminusVH_p%s",i)]]<-out.table[[sprintf("VV_p%s",i)]]-out.table[[sprintf("VH_p%s",i)]]
    out.table[[sprintf("VVdivVH_p%s",i)]]<-out.table[[sprintf("VV_p%s",i)]]/out.table[[sprintf("VH_p%s",i)]]
    out.table[[sprintf("VVnormDiffVH_p%s",i)]]<- (out.table[[sprintf("VV_p%s",i)]]-out.table[[sprintf("VH_p%s",i)]])/ (out.table[[sprintf("VV_p%s",i)]]+out.table[[sprintf("VH_p%s",i)]])
  }
  
  # out.table.m <- reshape2::melt(out.table, id.vars=c( "Date" ,"Name","Orbit","Platform","OrbitPass" ))
  
  out.table$Date <- as.POSIXct.numeric( as.numeric(out.table$Date)/1000, origin = "1970-01-01", tz = "GMT")
  return(droplevels(out.table))
}


out.gee.final <-    fromJSON( "data/webinar2_2020_08_05/webinar2vaia.geojson" )  
out.table.final <-   xxx
out.table.final.m<-reshape2::melt(out.table.final, id.vars=c( "Date" ,"Name","Orbit","Platform","OrbitPass" ))



 mapview( grid.points.gee, col.regions = pal(100), zcol = "p10", 
         color=NULL, alpha.regions=0.8  ) 
  
mapview( grid.points.gee, col.regions = pal(100), zcol = "iqr", 
         color=NULL, alpha.regions=0.8  ) 
