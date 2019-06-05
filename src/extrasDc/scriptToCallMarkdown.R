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

# define species 
speciesType <-"Cucurbita"

#Occurance Directory
occ_dir <- "C:/Users/danie/Desktop/aichiTest/aichiTest/parameters/occurrences/raw"

#Country Directory - only if you need to clip outputs to a country 
useCountry <- FALSE
#countryOfInterset <- "TUN"  #needs to be ISO3
#folder1 <- "C:/Users/danie/Desktop/aichiTest/aichiTest/parameters/gadm/shapefile"
#country <- readOGR(paste0(folder1, "/gadm28ISO.shp"),verbose = FALSE) %>%
# subset(ISO %in% countryOfInterset)


#define base directory 
base_dr <- "C:/Users/danie/Desktop/aichiTest/aichiTest/gap_analysis"
species_dir <- "C:/Users/danie/Desktop/aichiTest/aichiTest/parameters/occurrences/raw"

#create a new director 
dir.create(paste0(base_dr,"/","endProcessCucurbita", Sys.Date()))

#Define new director as a variable 
outputDir <- paste0(base_dr,"/","endProcessCucurbita", Sys.Date())

#Define the name of the run your interested in. 
runFolder <- "cucurbita20190531"

#list all species being modeled
all <- tools::file_path_sans_ext(list.files(path = species_dir,full.names = FALSE, recursive = FALSE))
species <- grep(speciesType, all, value = TRUE)


rmarkdown::render("C:/Users/danie/Desktop/aichiTest/aichiTest/extras/summaryAllRuns.Rmd",  # file 2
                  output_file =  paste("SummaryReport_", speciesType , Sys.Date(), ".html", sep=''),
                  output_dir = paste0(outputDir))
# for each species in the data create a report
species2 <- species[8]
# these reports are saved in output_dir with the name specified by output_file
for (taxa in species2){
  runtype <- list.dirs(path = paste0(base_dr,"/", taxa),full.names = FALSE, recursive = FALSE)
  #rmarkdown::render("C:/Users/danie/Desktop/aichi13/extrasDC/summaryRunResults.rmd",  # file 2
   #                 output_file =  paste("report_", taxa, '_a' , Sys.Date(), ".html", sep=''), 
    #                output_dir = paste0(base_dr, "/summaryResultsCucurbita12_17"))
  for (run in runtype){
    #clause if you want to only summarize a single report 
    if( run == runFolder){
    print(paste0(taxa,"on this ", run))
    baseDir <- paste0(base_dr, '/', taxa,'/',run)
    csv <- list.files(baseDir, pattern = ".csv", recursive = TRUE , full.names = TRUE)
    # if(length(csv) == 0){
    #   next
    # }else{
      rmarkdown::render("C:/Users/danie/Desktop/aichi13/extrasDC/mapFunctions.rmd",  # file 2
                        output_file =  paste("report_", taxa, '_', run,'_a' , Sys.Date(), ".html", sep=''),
                        output_dir = paste0(outputDir))
    # }
    }

      }
}



