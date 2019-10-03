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
source.files = source.files[ !grepl("select_sub_indicator.R", source.files) ]
source.files = source.files[ !grepl("extrasDc", source.files) ]


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
<<<<<<< HEAD
server.species = read.csv(paste0(root ,"/parameters/WEP/capsicum_CWR.csv"), header = TRUE) 
allData <- read.csv(paste0(root ,"/parameters/Capsicum/Capsicum_combinedall_2019_7_19.csv")) 

### Set groups for modeling based on the number of spatial points. 
numForModel <- allData %>%
  filter(!is.na(latitude) & !is.na(longitude))%>%
  group_by(Taxon_final) %>%
  dplyr::summarise(count = n())

# x > 10
tenPlus <- numForModel %>%
  filter(count >= 10) %>%
  select(Taxon_final) %>%
  dplyr::rename(taxonkey = Taxon_final)
# 10 >= x > 3
threeReps <- numForModel %>%
  filter(count < 10 & count >= 3) %>%
  select(Taxon_final)%>%
  dplyr::rename(taxonkey = Taxon_final)
# x < 3 
noModel <- numForModel %>%
  filter(count < 3) %>%
  select(Taxon_final) %>%
  dplyr::rename(taxonkey = Taxon_final)

# temp subset 
cas <- c("Capsicum_benoistii", "Capsicum_eshbaughii", "Capsicum_villosum_var._muticum")
=======
server.species = read.csv(paste0(root ,"/parameters/WEP/ + youSpeciesOccuranceData.csv"), header = TRUE)
>>>>>>> 22453825f4f42a2cdd6be82bce2f3f20e2abedc3

##########################################   End Set Parameters  ###############################################

### view all potential species
# print(server.species)

### test selected species
server.species2 <- slice(server.species,12)
<<<<<<< HEAD
species1 <- "Capsicum_eshbaughii"
#ssCut <- server.species[,]
=======
species1 <- "a species you are testing"
ssCut <- server.species[12:16,]
>>>>>>> 22453825f4f42a2cdd6be82bce2f3f20e2abedc3

#run process for a selected species.
result_master = lapply(server.species2, master_run)
##########################################   Start Process    ###############################################

#quick change 

# Run function in parallel for all species
result_master = lapply(server.species$taxonkey, master_run)

## subset list to for species that did not run. 
noModels <- data.frame(ncol(1))
for(i in 1:nrow(server.species)){
  tif <- list.files(path = paste0(gap_dir,"/",server.species[i,],"/",run_version,"/modeling/maxent"), pattern = ".tif" , recursive = TRUE, full.names = TRUE)
  tf <- length(tif) == 0
  noModels[i,1] <- tf 
}

reRuns <- server.species[noModels$V1,]
# 
result_master = lapply(reRuns, master_run)

# 
result_master = lapply(server.species2, master_run)


# Stop cluster
# sfStop()

df = ldply(result_master, data.frame)
write.csv(df, paste0("runs/results/server_",server.number,".csv"), row.names = FALSE, quote = FALSE)

##########################################   End Process    ###############################################
