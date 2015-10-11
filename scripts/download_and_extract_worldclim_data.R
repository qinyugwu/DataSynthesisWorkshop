### HEADER #####################################################################
##' @title Download and extract worldclim data
##'
##' @author Damien G. 
##' @contact damien.georges2 at gmail.com
##' 
##' @date 26/03/2015
##' 
##' @description This script will show how to :
##'   - download Worldclim Data from worldclim.org website
##'   - extract climatic variables for given sites from the downloaded files
##'   
##' @note This file have been prooduced in march 2015. It will work with the
##'   1.4 version of worldclim data. If you want to use a different version
##'   of the data, you will have to update downloading path, file names, ...
##' 
##' @log 
##' 
##' @licencing GPL
##'     Copyright (C) 2015  Damien G.
##' 
##'     This program is free software: you can redistribute it and/or modify
##'     it under the terms of the GNU General Public License as published by
##'     the Free Software Foundation, either version 3 of the License, or
##'     (at your option) any later version.
##' 
##'     This program is distributed in the hope that it will be useful,
##'     but WITHOUT ANY WARRANTY; without even the implied warranty of
##'     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##'     GNU General Public License for more details.
##' 
##'     You should have received a copy of the GNU General Public License
##'     along with this program.  If not, see <http://www.gnu.org/licenses/>.
## END OF HEADER ###############################################################

## Start of script --------------------------------------------------------
setwd("~/DataSynthesisWorkshop")
rm(list=ls())

# Download Worlclim data for current conditons ----------------------------

## select the output directory where worlclim data will be download
out.dir <- "~/Data/WorldClim_data"
## create the directory if it does not exist
dir.create(out.dir, showWarnings = FALSE, recursive = TRUE) 

## choose the resolution (if several are indicated several download will be done)
# wc.res = c("30s", "2-5m", "5m", "10m")
wc.res = "30s"

## choose the type of data wanted
# wc.dat = c("bio", "alt", "prec", "tmin", "tmax", "tmean")
# wc.dat = "bio"
wc.dat = "alt"

##' @note This part should be run only if row worldclim data are not downloaded
##' yet. If you just want to do data extraction, you should directly go to the 
##' next section.

## download curent worldclim data
for(wc.d in wc.dat){ ## loop over data type
  for(wc.r in wc.res){ ## loop over resolution
    ## create the sub directory
    out.subdir <- file.path(out.dir, wc.dat, wc.res)
    dir.create(out.subdir, showWarnings = FALSE, recursive = TRUE)
    ## create the file to download id
    wc.file <- paste(wc.d, "_", wc.r, "_esri.zip", sep="")
    ## download the file
    download.file(url = paste("http://biogeo.ucdavis.edu/data/climate/worldclim/1_4/grid/cur/",
                              wc.file, sep=""), 
                  destfile = file.path(out.subdir, 
                                       wc.file))
  } ## end loop over resolution
} ## end loop over data type


# Uncompress data ---------------------------------------------------------
library(plyr)

zip.files <- list.files(out.dir, recursive = TRUE, pattern = ".zip", 
                        full.names = TRUE)
l_ply(zip.files, function(x){
  unzip(zipfile = x, exdir = dirname(x), overwrite = TRUE)
})


# Extract data for our sites location ------------------------------------

## load the coordinates of points we want data for

##' @note : this part of the script is a bit more specific. Here we will take 
##' the TRY sites from ShrubHub repos. If you want to extract information for
##' your own points, you will have to adapt this part

#Load G-TREE site data
siteLatLong <- read.csv("Data/siteDataLatLong.csv", header = TRUE, sep = ";")

## define the path where G-TREE Git repos is installed
path.to.GTREE <- "~/DataSynthesisWorkshop"
output.dir <- "~/DataSynthesisWorkshop/Worldclim" ## the directory where extracted 
                                                ## var will be saved

##' @note This dataset seems to be the most recent version of TRY data available
##' on ShrubHub repos but it doesn't have any coordinates so we will take another
##' one..
# data <- read.csv(file.path(path.to.GTREE,
#                            "data/TRY_Proposal_276_DataRelease_2014_01_29.csv"))

data <- siteLatLong

head(data)

## keep only data having defined coordinates
data <- data[!(is.na(data[,"Lat"])|is.na(data[,"Long"])), ]

## to ensure the coherence of the coordinates btw datasets, we recommend to
## use a SP data.frame object here and to reproject coordinates if they are not
## in WGS84
data.sp <- SpatialPointsDataFrame(data[, c("Long", "Lat")],
                                  data[, - which(names(data) %in% c("Long", "Lat"))],
                                  proj4string = crs("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"))


## load the data we want to extract
library(raster)
wc.dir <- "~/Data/WorldClim_data" ## the out.dir variable of previous part
wc.res <- "30s" ## see in first part for available data
wc.dat <- "alt" ## see in first part for available data

## identification of files we want to load
wc.files <- list.files(file.path(wc.dir,
                                 file.path(wc.dat, wc.res, wc.dat)),
                       full.names = TRUE)
## remove all info directories
wc.files <- grep(pattern = "info$", x = wc.files, value = TRUE, invert = TRUE)
## reorder the files
wc.files <- wc.files[gtools::mixedorder(wc.files)]
## load the wc stack
wc.stk <- stack(wc.files)



# cas 1 : we extract only the exact cell value ----------------------------

## extract the wc data for our observations
wc.extr <- extract(x = wc.stk, y = data.sp)

## merge the wc data with our dat.sp object and save it
try.wc <-  data.frame(data.sp, wc.extr)
save(try.wc, file = file.path(output.dir,
                              "try_wc.RData"))

##' @note The key of this saved object is the "ObservationID". It will be the 
##' straitforward to associate TRY observations to wgs xlimatic data using
##' merge function.

# cas 2 : we extract the worldclim value according to a increasing buffer size method ----

##' @description Extract data from a regular raster stack (NA's coherent accross layers)
##' for a given points with according to an increasing buffer method 
##'
##' @param
##' stk: the raster stack you want to work with (have to be 
##'   referenced and consistent in NA's location)
##' coord: a 2 columns matrix containing the coordinates of points we want to 
##'   extract (should be expressed in the same coordinate referencial than the
##'   stk object)
##' buff.max: (integer) the maximal buffer concerned (in metters or in the stk unit)
##' buff.step: (integer) the step btw 2 tested buffers sizes
##' fun: (function) the function used to summurize the value from a buffer
##' 
##' @return
##' a data.frame containing extracted values for all points
##' 
##' @details The idea here is to try to extract the value of a raster in the exact
##' point location. If a value is associated to this point we keep this value else,
##' we extract the neighboor cells and extract the stat we want (e.g mean, median).
##' If a value is defined it is kept else we repeat the procedure with an extended
##' neibourhood
##' 
# test
# stk <- wc.stk
# coord <- coordinates(data.sp)
# buff.max = 20000
# buff.step = 1000
# fun = mean
extract.inc.buff <- function(stk,
                             coord,
                             buff.max = 20000,
                             buff.step = 1000,
                             fun = mean){
  ## keep only non duplicated coordinates
  coord.nd <- coord[!duplicated(coord), ]
  ## the first try is always with the exact coordinates
  out.nd <- extract(stk, coord.nd)
  
  ## check if some NA's have been extracted
  na.rows <- which(apply(out.nd, 1, function(x) any(is.na(x))))
  buff.cur <- 0
  
  ## if some NA's try to extract values according to a buffer
  while(length(na.rows) & buff.cur < buff.max ){
    ## print the summary of this iteration
    cat("\n> ", length(na.rows), "undefined with buffer", buff.cur, "km. Try a new one..")
    ## define the new buffer
    buff.cur <- buff.cur + buff.step
    ## try to extract on the new buffer
    out.nd[na.rows, ] <- extract(stk, coord.nd[na.rows, ], buffer = buff.cur, 
                                 fun = fun, na.rm = TRUE)
    ## update the na.rows ids
    na.rows <- which(apply(out.nd, 1, function(x) any(is.na(x))))
  }
  
  ## print the summary of final iteration
  cat("\n> ", length(na.rows), "undefined with buffer", buff.cur, 
      ". This values will stay undefined.")
  
  ## merge the out.nd and coord table
  out.nd <- data.frame(coord.nd, out.nd)
  out <- merge(coord, out.nd, 
               by.x = colnames(coord)[1:2],
               by.y = colnames(out.nd)[1:2],
               sort = FALSE)
  
  
}

## extract the wc data for our observations
wc.extr <- extract.inc.buff(stk = wc.stk,
                            coord = coordinates(data.sp),
                            buff.max = 20000,
                            buff.step = 1000,
                            fun = 'mean')

## merge the wc data with our dat.sp object and save it
try.wc <-  merge(data.frame(data.sp), wc.extr, 
                 by = c("Lon", "Lat"),
                 sort = FALSE)
save(try.wc, file = file.path(output.dir,
                              "try_wc.RData"))

##' @note The key of this saved object is the "ObservationID". It will be the 
##' straitforward to associate TRY observations to wgs xlimatic data using
##' merge function.

# Extra tests -------------------------------------------------------------

## some points coordinates should not have matching wc climate.. Here are some 
## tests to see if it is normal or not

# keep only long, lat and one bioclim variable
try.wc.na <- try.wc[, c("Lon", "Lat", "bio_1")]
# keep only undefined points
try.wc.na <- try.wc.na[is.na(try.wc.na[,"bio_1"]), ]
# remove duplicated coordinates
try.wc.na <- try.wc.na[!duplicated(try.wc.na), ]
# plot the undifined points 
# png(file.path(output.dir, "undefined_try_pts.png"))
plot(subset(wc.stk, "alt")) ## plot the altitude wc map
points(try.wc.na$Lon, try.wc.na$Lat)
# dev.off()



## End of script ----------------------------------------------------------
