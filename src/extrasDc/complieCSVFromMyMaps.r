####
# goal is to read in csv that have been created via qgis and compile this to a single doc.
# 20190509
####


library(dplyr)



### for each CWR
#### select col header we need
#### append to single dataframe


### read in files
baseDir <- "path to data downloaded from mymaps"
csv <- list.files(baseDir, pattern = ".csv", recursive = TRUE , full.names = TRUE)
csvs <- read.csv(csv[1])

### define all CWR names
names <- tools::file_path_sans_ext(list.files(baseDir, pattern = ".csv", recursive = TRUE , full.names = FALSE))
namesNoNumbers <- unique(gsub('[0-9]+', '', names))
# addation to change name of a species that was found on two mymaps
namesNoNumbers[5] <- "Cucurbita_foetidissima"


### loop over all species using the filter

summaryStats <- data.frame(matrix(NA, nrow = 0, ncol = 13))
colnames(summaryStats) <- c("Taxon_final", "db","type", "institute","sample_number","status",
                            "genus" , "country" , "locality" ,
                            "improvement","Longitude","Latitude","newCoords")
n=1
for(i in csv){
  df <- read.csv(i)
  df <- df %>%
    dplyr::select("Taxon_final", "db",   "type" ,
 "institute" ,"sample_number" ,"status"  ,
"genus" , "country" , "locality" ,
"improvement","Longitude","Latitude","newCoords"   )
  summaryStats <- rbind(summaryStats, df)
}


write.csv(summaryStats, paste0(baseDir, "/compiledMyMapOriginal.csv"))
