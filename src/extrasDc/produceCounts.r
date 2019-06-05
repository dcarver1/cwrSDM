####

# 10/15/2018

# The goal of this work is to generate a tab seperated files that contains counts of multiple paraments as

# defined by the aichi docs 

### 

library(dplyr)
## use this clause to produce a counts CSV based only on a single area. 
Country <-FALSE
iso3 <- "TUN"

data <- read.csv("C:/Users/danie/Desktop/aichiTest/aichiTest/parameters/Cucurbita/Cucurbita_CWR_2019_5_29.csv")
dim(data)

names(data)
dataThin <- data %>%
  dplyr::select(c("Taxon_final", "latitude", "longitude", "type",  "country")) %>%
  mutate(hasLatLong = !is.na(latitude) | !is.na(longitude))
dim(dataThin)


if(Country == TRUE){
  dataThin <- dataThin %>%
    filter(country == iso3)
}
dim(dataThin)
#List of existing cwr species 
#CWR <- unique(data$Taxon_final)

#option is using uncleaned dataset 
speciesOfInterest <- read.csv("C:/Users/danie/Desktop/aichiTest/aichiTest/parameters/Cucurbita/edited_0429_forModeling.csv") 
CWR <- unique(speciesOfInterest$Taxon_final)




#generate a conut of all the species. 

counts <- dataThin %>% 
  group_by(Taxon_final, type) %>%
  dplyr::summarise(total = n()) %>%
  arrange(desc(total))

View(counts)

# this should be changed 

local2 <- "C:/Users/danie/Desktop/aichiTest/aichiTest/gap_analysis" 



colNames <- c("totalRecords",	"totalUseful", 	"totalGRecords",	"totalGUseful",

              "totalHRecords",	"totalHUseful",	"totalPost1950",	"totalPre1950",

              "totalNoDate",	"GBIF",	"GENE")





# loop through the list of names and generate csv based on the species 

for(i in CWR){

  tbl <- filter(dataThin, Taxon_final %in% i) %>%

    dplyr::group_by(type, hasLatLong) %>%

    dplyr::summarize(total = n())

  df <- data.frame(matrix(NA, nrow = 1, ncol = 11))

  colnames(df) <- colNames

  df$totalRecords <- sum(tbl$total)

  df$totalUseful <- sum((subset(tbl, hasLatLong == TRUE))$total)

  df$totalGRecords <- sum((subset(tbl, type == "G"))$total)

  df$totalGUseful <- sum((subset(tbl, type == "G" & hasLatLong == TRUE))$total)

  df$totalHRecords <- sum((subset(tbl, type == "H"))$total)

  df$totalHUseful <- sum((subset(tbl, type == "H" & hasLatLong == TRUE))$total)

  dir.create(paste0(local2,"/",as.character(i)), showWarnings = FALSE)
  rownames(df) <- NULL
  write.csv(df, file = paste0(local2,"/",as.character(i),"/counts.csv"))
  print(paste0(i , " with " ,dim(df)))
    } 





