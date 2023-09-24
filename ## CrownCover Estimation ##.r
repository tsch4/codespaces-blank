## CrownCover Estimation ##


## Author: Tim Schuett  ##

#------------------------------------------------------------------------------#

## Table of contents ##
# [A1-A3] Packages, files and paths
# [B1-B2] Check and handle point cloud data and normalize by DTM
# [C1-C3] Gap detection loop with plotting
# [D1-D3] Interpolation of CRS coordinates and vectorization of raster data
# [E] Gap area estimation


#------------------------------------------------------------------------------#
### [A1] load packages ###
#####
# list for needed packages
required_packages <- c( "lidR", "ggplot2", "plotly", "rgdal",
  "plyr", "dplyr", "readr", "terra","sf",
   "viridis", "DescTools","raster", "akima", "BiocManager", 
  "rhdf5", "cutoff", "bbmle", "devtools", "multimode")


#BiocManager::install("rhdf5")
#devtools::install_github("choisy/cutoff")
# checks if packages are installed and loaded
for (i in required_packages) {
  if (!require(i, character.only = TRUE)) {
    install.packages(i, dependencies = TRUE)
    library(i)
  }
  
}


rm(i,required_packages)


#####
#------------------------------------------------------------------------------#
## [A1] SELECT WORKING DIRECTORY 

#wDir <- (choose.dir()) 
#setwd(wDir)

setwd("C:/Users/timsc/Desktop/Masterarbeit/code/Scripte")
#####
#------------------------------------------------------------------------------#
### [A1] LOAD SCRIPTS ###
#####
source ("cc_functions.r")

#####
#------------------------------------------------------------------------------#
### [A3] proceed multiple files 
#####

# uncorrected files from RSDB 
#files <-choose.files()
#rootDir <- choose.dir() #c("C:/Users/timsc/Desktop/Masterarbeit/data/raw")

rootDir <- c("C:\\Users\\timsc\\Desktop\\Masterarbeit\\data\\raw\\stratum_4.2")
filepath_raw <- paste(rootDir, dir(rootDir, ".las") ,sep = "\\")


# import laz files, use epsg 
#EPSG:4258
CRS = 25832
laz.import(rootDir, CRS = 25832, normalize = 1, filter_ground = 1)
# * adds filepath variable to global env * 


# get corrected files 
set_filepath()


# directory of corrected files 
# chr
tmp <- list.files(filepath, pattern=".*las$")
tmp

# with full path
files_clean <- paste(filepath, dir(filepath, ".*las$") ,sep = "/")
cat("found", length(files_clean), "files")

# order by size
#files_clean[order(file.size(files_clean))]


#####
#------------------------------------------------------------------------------#
### [B1] Estimate number of canopy layers ###
#####

# ++ test ++
n = 0
for (i in files_clean[order(file.size(files_clean))]) {
  n = n +1
  cat("\n","processing ","[",n,"/",length(files_clean),"]")
  plot.new()
  get.lyrs(i)
  
  
  text(grconvertX(.05, "npc", "user"), grconvertY(.9, "npc", "user"), cex = 1.5,
       paste(n))
}



#####
#------------------------------------------------------------------------------#
### [B2] Compute Crown Cover depending on canopy layer ###
#####
# calculate crown cover by raster and vector spatial extension


# generate empty data.frames 
df_cc_full <- data.frame() 
df_crwn_full <- data.frame()
n = 0 
for (i in files_clean[355]) {
  n = n+1
  cat("\n","processing ","[",n,"/",length(files_clean),"]")
  #  estimate vertical layers
  lyr <- get.lyrs(i)
  
  
  las_tmp <-  suppressWarnings(lidR::readLAS(i))
  
  if(length(las_tmp$X) < 100){
    ## print info ##
    message("not enough trees")
    next
  }
  
  
  # cover estimation
  get.cc.data(las_tmp, lyr[[1]], basename(i))

  # write data 
  
  #write.csv(df_cc, paste0("crown_cover_", basename(i), ".csv"))
  #write.csv(df_cc, paste(filepath, paste0( sep = "_","crown_cover", ".csv"), sep = "/"))
  
  # data frames
  df_cc_full <- rbind(df_cc, df_cc_full)
  df_crwn_full <- rbind(df_crwn, df_crwn_full)
}

write.csv(df_cc_full,paste(filepath, paste0("crown_cover",sep = "_" ,"4.2", ".csv"), sep = "/"))
write.csv(df_crwn_full,paste(filepath, paste0("crowns",sep = "_" ,"4.2", ".csv"), sep = "/"))


#####


#------------------------------------------------------------------------------#
### [B3] get pointcloud stats ###
#####

# generate empty data.frames 

df_point.stats <- data.frame(matrix(ncol=4,nrow= length(files_clean), 
                    dimnames=list(NULL, c("id", "area", "density", "points"))))

#df_temp<- data.frame(matrix(ncol=4,nrow=length(files_clean), 
#                                    dimnames=list(NULL, c("id", "area", "density", "points"))))

n = 0
for (i in files_clean[order(file.size(files_clean))]) {
  n = n+1
  cat("\n","processing ","[",n,"/",length(files_clean),"]")
 
  # load las_file and capture output
  results <- suppressWarnings(capture.output(lidR::readLAS(i)))
  
  
  df_point.stats$id[n] <- sub("_norm.*","", basename(i))
  df_point.stats$area[n] <- stringr::str_split(results[5], " ", simplify = TRUE)[11]
  df_point.stats$density[n] <- stringr::str_split(results[7], " ", simplify = TRUE)[8]
  
  #id
 # df_temp$id <- sub("_norm.*","", basename(i))
  # area
#  df_temp$area <- stringr::str_split(results[5], " ", simplify = TRUE)[11]
  #  density
 # df_temp$density <- stringr::str_split(results[7], " ", simplify = TRUE)[8]
  # points
  #df_temp$points <- 
    
  #stringr::str_split(results[6], " ", simplify = T)[,10]
  #stringr::str_c(stringr::str_split(results[6], " ", simplify = T)[,9:10], collapse = "_")
  
  
  #stringr::str_split(results[6], " ", simplify = T)[,10] == "thousand"
  
}

# get mean
round(mean(na.omit(as.numeric(df_point.stats$density))),2)





# write data 
write.csv(df_point.stats, paste(filepath, paste0("point_stats",sep = "_" ,basename(filepath), ".csv"), sep = "/"))

#------------------------------------------------------------------------------#






df_point.stats$id["10a1_1"]


  10a1_1

  10a1_2


 c("101a1_2_1.las", "101a1_2_2.las","101a1_2_3.las", "101a1_2_4.las") 


 
 
 
 df_point.stats$id[n] <- sub("_norm.*","", basename(i))

 sub("_1.las.*","", basename(tmp))


tmp

sort(extent_pointcloud$label)







