##########################################  Start Install Packages  ###############################################

# install.packages(c("snowfall","raster","maptools","rgdal","ff","data.table","gtools","velox","PresenceAbsence","rJava","dismo","tidyverse","SDMTools","rgeos","shapefiles","plyr", "sp"))

##########################################   End Install Packages  ###############################################


##########################################  Start Dependences  ###############################################

library(raster)
library(maptools)
library(rgdal)
library(ff)
library(data.table)
library(gtools)
library(velox)
library(PresenceAbsence)
library(rJava)
library(dismo)
library(tidyverse)
library(SDMTools)
library(rgeos)
library(shapefiles)
library(sp)
library(plyr)
library(devtools)
library(countrycode)
#install.packages("countrycode")
#install_github("DFJL/SamplingUtil")
library(SamplingUtil)

#install.packages("rJava")

# Load massive climate file
base_dir = "enterPath"
repo_dir = "base_dir + /src"
temp_dir= "base_dir + /TEMP"
if(!file.exists(temp_dir)){dir.create(temp_dir)}
raster::rasterOptions(tmpdir=temp_dir)


# Load the sources scripts
source.files = list.files(repo_dir, ".[rR]$", full.names = TRUE, recursive = T)
source.files = source.files[ !grepl("run", source.files) ]
source.files = source.files[ !grepl("calibration", source.files) ]
source.files = source.files[ !grepl("indicator", source.files) ]
source.files = source.files[ !grepl("to_map", source.files) ]
source.files = source.files[ !grepl("count_records_sp.R", source.files) ]
source.files = source.files[ !grepl("verification_tool.R", source.files) ]

#lapply(source.files, source)
for(i in 1:length(source.files)){
  cat(i,"\n")
  source(source.files[i])

}
# Load massive climate file
config(dirs=T)
# 2.5 arc min
rst_vx <- readRDS(paste(par_dir,"/bioLayer_2.5/climate_vx.RDS" ,sep=""))
load(file=paste0(par_dir, "/gadm/shapefile/gadm28ISO.RDS"))
config(dirs=F, cleaning=F, insitu=F, exsitu=F, modeling=T, premodeling=T)
##########################################  End Dependences  ###############################################

##########################################  Start Set Parameters  ###############################################

setwd(root)

# running on local machine at the moment
server.number = "1"
server.species = read.csv(paste0(root ,"/parameters/WEP/ + youSpeciesOccuranceData.csv"), header = TRUE)

##########################################   End Set Parameters  ###############################################

### view all potential species
# print(server.species)

### test selected species
server.species2 <- slice(server.species,12)
species1 <- "a species you are testing"
ssCut <- server.species[12:16,]

#run process for a selected species.
result_master = lapply(server.species2, master_run)
##########################################   Start Process    ###############################################

# Run function in parallel for all species
result_master = lapply(server.species$taxonkey, master_run)





# Stop cluster
# sfStop()

df = ldply(result_master, data.frame)
write.csv(df, paste0("runs/results/server_",server.number,".csv"), row.names = FALSE, quote = FALSE)

##########################################   End Process    ###############################################
