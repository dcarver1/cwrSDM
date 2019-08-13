###
# attempt to answer the two following questions
# 1. of the total number of ecoregions within tunisia, how many ecoregions have samples been seen within
# 2. How many points fall within the protected areas of tunisia
###


# Input layers
# 1. point occurance
# 2. Tunisia layer
# 3. Ecoregion Layer
# 4. Protected areas layer

###import libraries
library(raster)
library(rgdal)
library(sp)
library(dplyr)
library(tmap)
tmap_mode("view")

base_dir <- "path to your base directory"
### import data
pointFolder <- "base_dir + /parameters/ + yourSpeciesOfInterest"
points <- read.csv(paste0(pointFolder, "/yourSpeciesOfInterest.csv"))
points <- points[complete.cases(points[ , 3:4]),]
CWR <- unique(points$taxon_final)
coordinates(points) <- c("longitude", "latitude")


#Country
countryOfInterset <<- "TUN"  #needs to be ISO3
folder1 <<- "base_dir + /parameters/gadm/shapefile"
tunisia <- readOGR(paste0(folder1, "/gadm28ISO.shp"),verbose = FALSE) %>%
  subset(ISO %in% countryOfInterset)
# Ecoregion
shpFolder <-"base_dir + /parameters/ecosystems/shp"
ecoregions <-  readOGR(paste0(shpFolder,"/wwf_terr_ecos.shp"),verbose = FALSE)
#protected Lands
protFolder <- "base_dir + /parameters/protected_areas/raster"
protectedLands <- raster(paste0(protFolder, "/wdpa_reclass.tif"))




### Mask all features to Tunisia
crs(points)<-crs(tunisia)
pointsT <- crop(points, tunisia)
crs(ecoregions)<-crs(tunisia)
ecoT <- crop(ecoregions, tunisia)
crs(protectedLands)<-crs(tunisia)
proT <- crop(protectedLands, tunisia)


### intersect points with eco and pro

pointsEco <- raster::extract(ecoT, pointsT)
pointsPro <- raster::extract(proT, pointsT)

pointsT$eco <- pointsEco$ECO_ID
pointsT$pro <- pointsPro

pointsData <- pointsT@data %>%
  dplyr::select("taxon_final", "eco", "pro")
View(pointsData)
### loop over all species using the filter
CWR <- unique(pointsData$taxon_final)

summaryStats <- data.frame(matrix(NA, nrow = length(CWR), ncol = 3))
colnames(summaryStats) <- c("taxon_final", "Percentage of Ecoregion with Occurance in Tunisia", "Percentage of Occurance in protected Regions in Tunisia")

tunEco <- length(unique(ecoT$ECO_ID))

n=1
for(i in sort(CWR)){
  df <- pointsData %>%
    filter(taxon_final == i)
  if(nrow(df)==0){
    summaryStats[n,1] <- i
    summaryStats[n,2] <- NA
    summaryStats[n,3] <- NA
  }else{

  ### 1,
  ecoCoverage <- (length(unique(df$eco))/tunEco)*100

  ### 2.
  if(TRUE %in% !is.na(df$pro)){
  numberPro <- (sum(df$pro,na.rm=TRUE)/nrow(df))*100
  }else{
    numberPro <- 0
  }

  summaryStats[n,1] <- i
  summaryStats[n,2] <- ecoCoverage
  summaryStats[n,3] <- numberPro
  }
  n = n+1
}

write.csv(summaryStats, paste0(pointFolder, "/extraSummaryStats.csv"))
