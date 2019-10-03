# References for automation
# http://www.r-bloggers.com/how-to-source-an-r-script-automatically-on-a-mac-using-automator-and-ical/
# http://www.engadget.com/2013/03/18/triggering-applescripts-from-calendar-alerts-in-mountain-lion/

# File 1: Should be an R-Script
# contains a loop that iteratively calls an Rmarkdown file (i.e. File 2)

# load packages
library(knitr)
library(markdown)
library(rmarkdown)
library(rgdal)
library(raster)
library(dplyr)

# this is a function that will pull in output tif from different folders and generate a set of maps with evaluation statistics
# base_dr <-"C:/Users/danie/Desktop/aichiTest/aichiTest/gap_analysis/endVersion!19_2_1"

<<<<<<< HEAD
# define species 
speciesType <-"Capsicum"
=======
# define species
speciesType <-"genus of interest"
>>>>>>> 22453825f4f42a2cdd6be82bce2f3f20e2abedc3

#Occurance Directory
occ_dir <- "path to occurence data"

#Country Directory - only if you need to clip outputs to a country
useCountry <- FALSE
#countryOfInterset <- "TUN"  #needs to be ISO3
#folder1 <- "C:/Users/danie/Desktop/aichiTest/aichiTest/parameters/gadm/shapefile"
#country <- readOGR(paste0(folder1, "/gadm28ISO.shp"),verbose = FALSE) %>%
# subset(ISO %in% countryOfInterset)


#define base directory
base_dr <- "path to base directory"
species_dir <- "path to species occurence data"

<<<<<<< HEAD
#create a new director 
dir.create(paste0(base_dr,"/","endProcess",speciesType, Sys.Date()))

#Define new director as a variable 
outputDir <- paste0(base_dr,"/","endProcess",speciesType, Sys.Date())

#Define the name of the run your interested in. 
runFolder <- "capsicum20190701"
=======
#create a new director
dir.create(paste0(base_dr,"/","name of the directory", Sys.Date()))

#Define new director as a variable
outputDir <- paste0(base_dr,"/","name of the directoy", Sys.Date())

#Define the name of the run your interested in.
runFolder <- "name of the run folder"
>>>>>>> 22453825f4f42a2cdd6be82bce2f3f20e2abedc3

#list all species being modeled
all <- tools::file_path_sans_ext(list.files(path = species_dir,full.names = FALSE, recursive = FALSE))
species <- grep(speciesType, all, value = TRUE)

## troubleshooting with capsicum
allData <- read.csv(paste0("C:/Users/danie/Desktop/aichiTest/aichiTest/parameters/Capsicum/Capsicum_combinedall_2019_7_19.csv")) 


### Set groups for modeling based on the number of spatial points. 
numForModel <- allData %>%
  filter(!is.na(latitude) & !is.na(longitude))%>%
  group_by(Taxon_final) %>%
  dplyr::summarise(count = n()) %>%
  dplyr::rename(taxonkey = Taxon_final)

#test single species 
t234 <- numForModel[1,]

# # x > 10
# tenPlus <- numForModel %>%
#   filter(count >= 10) %>%
#   select(taxonkey)
# # 10 >= x > 3
# threeReps <- numForModel %>%
#   filter(count < 10 & count > 3) %>%
#   select(taxonkey)
# # x < 3 
# noModel <- numForModel %>%
#   filter(count <= 3) %>%
#   select(taxonkey)


<<<<<<< HEAD
=======
rmarkdown::render(paste0(base_dr, "/extras/summaryAllRuns.Rmd"),  # file 2
                  output_file =  paste("SummaryReport_", speciesType , Sys.Date(), ".html", sep=''),
                  output_dir = paste0(outputDir))
# for each species in the data create a report
species2 <- species[8]
>>>>>>> 22453825f4f42a2cdd6be82bce2f3f20e2abedc3
# these reports are saved in output_dir with the name specified by output_file
for (taxa in numForModel$taxonkey){
  runtype <- list.dirs(path = paste0(base_dr,"/", taxa),full.names = FALSE, recursive = FALSE)
<<<<<<< HEAD
=======
  #rmarkdown::render("C:/Users/danie/Desktop/aichi13/extrasDC/summaryRunResults.rmd",  # file 2
   #                 output_file =  paste("report_", taxa, '_a' , Sys.Date(), ".html", sep=''),
    #                output_dir = paste0(base_dr, "/summaryResultsCucurbita12_17"))
>>>>>>> 22453825f4f42a2cdd6be82bce2f3f20e2abedc3
  for (run in runtype){
    #clause if you want to only summarize a single report
    if( run == runFolder){
    print(paste0(taxa,"on this ", run))
    baseDir <- paste0(base_dr, '/', taxa,'/',run)
    csv <- list.files(baseDir, pattern = ".csv", recursive = TRUE , full.names = TRUE)
    # if(length(csv) == 0){
    #   next
    # }else{
<<<<<<< HEAD
      rmarkdown::render("C:/Users/danie/Desktop/aichiTest/aichiTest/src/extrasDC/mapFunctions.rmd",  # file 2
=======
      rmarkdown::render(paste0(base_dr + "/extrasDC/mapFunctions.rmd"),  # file 2
>>>>>>> 22453825f4f42a2cdd6be82bce2f3f20e2abedc3
                        output_file =  paste("report_", taxa, '_', run,'_a' , Sys.Date(), ".html", sep=''),
                        output_dir = paste0(outputDir))
    # }
    }

      }
}
<<<<<<< HEAD

rmarkdown::render("C:/Users/danie/Desktop/aichiTest/aichiTest/src/extrasDC/summaryAllRuns.Rmd",  # file 2
                  output_file =  paste("SummaryReport_", speciesType , Sys.Date(), ".html", sep=''),
                  output_dir = paste0(outputDir))
=======
>>>>>>> 22453825f4f42a2cdd6be82bce2f3f20e2abedc3
