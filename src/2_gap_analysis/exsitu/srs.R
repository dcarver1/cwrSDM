##########################################   Start Functions    ###############################################
# This function calculates the ex-situ SRS. It loads counts.csv and computes SRS
# @param (string) species: species ID
# @return (data.frame): This function returns a data frame with SRS, and numbers
#                       of G, H, and total samples, with and without coordinates.
srs_exsitu <- function(species) {
  #load config
  config(dirs=T,exsitu=T)
  library(dplyr)
  
  #directory for species

  sp_dir <- paste(gap_dir,"/",species,sep="")
  #load counts file
  ### DC changing sp_dir so it looks at the original raw datasets to inculde 
  ### Herbarium collection that may not have spatial data
  rawData12 <- read.csv("C:/Users/danie/Desktop/aichiTest/aichiTest/extras/Cucurbita_total_2018_9_27_addition23values_cleanedGBIF.csv")
  counts <- read.csv(paste(sp_dir,"/counts.csv",sep=""))
  
  #define number of H and G collections 
  # numberG_H <- rawData12 %>%
  #   filter(as.character(Taxon_final) == species)%>%
  #   group_by(type) %>%
  #   dplyr::summarise(count = n())
  # 
  # numberH <- numberG_H %>%
  #   filter(type == "H")
  # 
  # numberG <- numberG_H %>%
  #   filter(type == "G")
  # 
  
  #calculate SRS
  
  if (counts$totalHRecords ==0) {
    srs <- 0
  } else {
    srs <- min(c(100,counts$totalGRecords/counts$totalHRecords*100))
  }
  
  
  #create data.frame with output
  out_df <- data.frame(ID=species, 
                       NTOTAL=counts$totalRecords,
                       NTOTAL_COORDS=counts$totalUseful,
                       NG= counts$totalGRecords,
                       NG_COORDS=counts$totalGUseful,
                       NH=counts$totalHRecords,
                       NH_COORDS=counts$totalHUseful,
                       SRS=srs)
  write.csv(out_df,paste0(sp_dir,"/",run_version,"/gap_analysis/exsitu/srs_result.csv"),row.names=F)
  
  
  #return object
  return(out_df)
}

#testing the function
#base_dir <- "~/nfs"
#source("~/Repositories/aichi13/src/config.R")
#srs_exsitu("2686262")

