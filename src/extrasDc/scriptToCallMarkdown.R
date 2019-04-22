# References for automation 
# http://www.r-bloggers.com/how-to-source-an-r-script-automatically-on-a-mac-using-automator-and-ical/
# http://www.engadget.com/2013/03/18/triggering-applescripts-from-calendar-alerts-in-mountain-lion/

# File 1: Should be an R-Script 
# contains a loop that iteratively calls an Rmarkdown file (i.e. File 2)

# load packages
library(knitr)
library(markdown)
library(rmarkdown)


# this is a function that will pull in output tif from different folders and generate a set of maps with evaluation statistics 
# base_dr <-"C:/Users/danie/Desktop/aichiTest/aichiTest/gap_analysis/endVersion!19_2_1"

# define species 
speciesType <-"Daucus"

#define base directory 
base_dr <- "C:/Users/danie/Desktop/aichiTest/aichiTest/gap_analysis"

#create a new director 
dir.create(paste0(base_dr,"/","endProcessDaucus", Sys.Date()))

#Define new director as a variable 
outputDir <- paste0(base_dr,"/","endProcessDaucus", Sys.Date())

#Define the name of the run your interested in. 
runFolder <- "daucus20190417"

#list all species being modeled
all <- list.dirs(path = base_dr,full.names = FALSE, recursive = FALSE) 
species <- grep(speciesType, all, value = TRUE)
  
# for each species in the data create a report
# these reports are saved in output_dir with the name specified by output_file
for (taxa in species){
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
    if(length(csv) == 0){
      next
    }else{
      rmarkdown::render("C:/Users/danie/Desktop/aichi13/extrasDC/mapFunctions.rmd",  # file 2
                        output_file =  paste("report_", taxa, '_', run,'_a' , Sys.Date(), ".html", sep=''),
                        output_dir = paste0(outputDir))
    }
    }

      }
}



