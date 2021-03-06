##########################################   Start Functions    ###############################################
# This function takes a shapefile of native areas and crops the biolayers, and creates a mask
# with the shapefile.
# @param (string) species: species ID
# @return (string): species ID

# species="5421351"
# 
# nat_area_mask(species)
# countries_sh<-load(file=paste0(par_dir, "/gadm/shapefile/gadm28ISO.RDS"))

#species <- species1

nat_area_mask <- function(species) {
  #load config function
  config(dirs=T, premodeling=T)
  rst_vx <- readRDS(paste(par_dir,"/biolayer_2.5/climate_vx.RDS",sep=""))
  
  
  #required packages
  require(shapefiles); require(raster); library(rgeos); require(velox)
  require(rgdal) #; require(parallel)
  
  #velox RDS climate object
  x <- rst_vx
  
  
  #load counts
  sp_counts <- read.csv(paste(gap_dir,"/",species,"/counts.csv",sep=""))
  ####
  # DC 
  # there is errors with that data structure.
  # the first if statement is just checking to see if there are values with lat long. I produce a dataset that has 
  # the total number of features that have late log
  #change from !=0 to > 0 
  #####
  
  #run only if there are records with coordinates
  if (sp_counts$totalUseful != 0) {
    #native area folder
    narea_dir <- paste0(gap_dir, "/",species, "/", run_version ,"/bioclim")
    
    #load native area shapefile
    if (!file.exists(paste0(narea_dir, "/narea.shp"))) {
      cat("Shapefile for species ID=", species,"native area doesn't exist", "\n")
    } else {
      #cat("doing", species, "\n")
      if (!file.exists(paste0(narea_dir, "/", "crop_narea.RDS"))) {
        #load native area shapefile
        cat("   ...loading native area shapefile, and creating mask\n")
        shapean <- raster::shapefile(paste0(narea_dir,"/","narea.shp"))
        shapean$value <- 1
        if (!file.exists(paste0(narea_dir, "/", "narea_mask.tif"))) {
          na_msk <- raster::rasterize(shapean,global_mask,field="value",silent=T)
          #na_msk[which(!is.na(na_msk[]))] <- 1
          na_msk <- crop(na_msk, extent(shapean))
          writeRaster(na_msk, paste0(narea_dir, "/", "narea_mask.tif"), format="GTiff")
        } else {
          na_msk <- raster(paste0(narea_dir, "/", "narea_mask.tif"))
        }
        
        #crop and mask biolayers
        cat("   ...crop bioclim layers\n")
        x$crop(extent(shapean))
        biolayers_cropc <- x$as.RasterStack()
        
        
        
        biolayers_cropc <- biolayers_cropc * na_msk
        names(biolayers_cropc) <- names(biolayers)
        if (!biolayers_cropc@data@inmemory) {biolayers_cropc <- readAll(biolayers_cropc)}
        
        #save cropped biolayers dataset
        saveRDS(object=biolayers_cropc, file=paste0(narea_dir, "/crop_narea.RDS"))
        #rm(narea_dir); rm(shapean); rm(biolayers_cropc)
      }
      #clean memory
      rm(x); gc(reset=TRUE)
    }
  }
  #return species ID
  return(species)
}





























##########################################   Start Functions    ###############################################
# # This function takes a shapefile of native areas and crops the biolayers, and creates a mask
# # with the shapefile.
# # @param (string) species: species ID
# # @return (string): species ID
# 
# #species= species1
# # 
# # nat_area_mask(species)
# # countries_sh<-load(file=paste0(par_dir, "/gadm/shapefile/gadm28ISO.RDS"))
# 
# 
# nat_area_mask <- function(species) {
#   #load config function
#   config(dirs=T, premodeling=T)
#   # rst_vx <- readRDS(paste(par_dir,"/biolayer_2.5/climate_vx.RDS",sep=""))
#   
#   
#   #required packages
#   require(shapefiles); require(raster); library(rgeos); require(velox)
#   require(rgdal) #; require(parallel)
#   
#   #velox RDS climate object
# 
#   #load counts
#   sp_counts <- read.csv(paste(gap_dir,"/",species,"/counts.csv",sep=""),sep="\t")
#   ####
#   # DC 
#   # there is errors with that data structure.
#   # the first if statement is just checking to see if there are values with lat long. I produce a dataset that has 
#   # the total number of features that have late log
#   #change from !=0 to > 0 
#   #####
#   
#   #run only if there are records with coordinates
#   if (sp_counts$totalUseful != 0) {
#     #native area folder
#     narea_dir <- paste0(gap_dir, "/",species, "/", run_version ,"/bioclim")
#     
#     #load native area shapefile
#     if (!file.exists(paste0(narea_dir, "/narea.shp"))) {
#       cat("Shapefile for species ID=", species,"native area doesn't exist", "\n")
#     } else {
#       #cat("doing", species, "\n")
#       if (!file.exists(paste0(narea_dir, "/", "crop_narea.RDS"))) {
#         #load native area shapefile
#         cat("   ...loading native area shapefile, and creating mask\n")
#         shapean <- raster::shapefile(paste0(narea_dir,"/","narea.shp"))
#         shapean$value <- 1
#         
#         
#         ####
#         # altering this to change the resolution of the native area 
#         if (!file.exists(paste0(narea_dir, "/", "narea_mask.tif"))) {
#           # na_msk <- raster::rasterize(shapean,global_mask,field="value",silent=T)
#           ### DC Addation 
#           na_msk <- raster::rasterize(shapean,rst_vx[[1]],field="value",silent=T)
#           #na_msk[which(!is.na(na_msk[]))] <- 1
#           na_msk <- crop(na_msk, extent(shapean))
#           writeRaster(na_msk, paste0(narea_dir, "/", "narea_mask.tif"), format="GTiff", overwrite=TRUE)
#         } else {
#           na_msk <- raster(paste0(narea_dir, "/", "narea_mask.tif"))
#         }
#         
#         # #crop and mask biolayers
#         # cat("   ...crop bioclim layers\n")
#         # x$(extent(shapean))
#         # biolayers_cropc <- x$as.RasterStack()
#         # biolayers_cropc <- x * na_msk
#         # names(biolayers_cropc) <- names(biolayers)
#         # if (!biolayers_cropc@data@inmemory) {biolayers_cropc <- readAll(biolayers_cropc)}
#         # 
#         #Reworking to make it work for new 30arc sec 
#         biolayers_cropc <- raster::crop(rst_vx, na_msk)
#         biolayers_cropc <- biolayers_cropc * na_msk
#         names(biolayers_cropc) <- names(rst_vx)
#         if (!biolayers_cropc@data@inmemory) {biolayers_cropc <- readAll(biolayers_cropc)}
#         
#         #save cropped biolayers dataset
#         saveRDS(object=biolayers_cropc, file=paste0(narea_dir, "/crop_narea.RDS"))
#         #rm(narea_dir); rm(shapean); rm(biolayers_cropc)
#       }
#       #clean memory
#       #rm(x) Changing this for the time bring.... DC 
#       ; gc(reset=TRUE)
#     }
#   }
#   #return species ID
#   return(species)
# }
# 
# # testing
# # ncores <- 8 #change according available server resources
# # c1 <- makeCluster(ncores)
# # clusterEvalQ(c1, lapply(c("shapefiles", "raster", "rgeos", "rgdal"), library, character.only= TRUE))
# # clusterExport(c1, c("outfol", "ocurs"))
# # parLapply(c1, ocurs[1:200], cropsC)
