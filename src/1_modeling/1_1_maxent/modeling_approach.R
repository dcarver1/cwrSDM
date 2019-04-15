# Modeling approach, pseudo-absences selection, calibration, and fitting models
# J. Soto, C. Sosa, H. Achicanoy
# CIAT, 2018

# R options
# g <- gc(reset = T); rm(list = ls()); options(warn = -1); options(scipen = 999)

# Load libraries
suppressMessages(library(tidyverse))
suppressMessages(library(dismo))
suppressMessages(library(velox))
suppressMessages(library(ff))
suppressMessages(library(data.table))
suppressMessages(library(sp))
# # Important scripts
#repo_dir<-"C:/Users/MVDIAZ/Desktop/src"
source(paste(repo_dir,"/1_modeling/1_1_maxent/create_mx_args.R",sep=""))
source(paste(repo_dir,"/1_modeling/1_1_maxent/do_projections.R",sep=""))
source(paste(repo_dir,"/1_modeling/1_1_maxent/evaluating.R",sep=""))
source(paste(repo_dir,"/1_modeling/1_1_maxent/nullModelAUC.R",sep=""))
source(paste(repo_dir,"/1_modeling/1_2_alternatives/create_buffers.R",sep=""))
source(paste(repo_dir,"/config.R",sep=""))


# From config file
# run_version <- "v1"
# gap_dir <- "//dapadfs/Workspace_cluster_9/Aichi13/gap_analysis"

# --------------------------------------------------------------------- #
# Modeling function
# --------------------------------------------------------------------- #
#species="2753717"
#spModeling(species)
#base_dir="//dapadfs"

# 
# species <- species1

spModeling <- function(species){
  # run config function
  config(dirs=T,modeling=T)
  
  if (file.exists(paste(occ_dir,"/no_sea/",species,".csv",sep=""))) {
    # Load calibration results
    if (file.exists(paste0(gap_dir, "/", species, "/", run_version, "/modeling/maxent/", species, ".csv.RData"))) {
      load(paste0(gap_dir, "/", species, "/", run_version, "/modeling/maxent/", species, ".csv.RData"))
    } else {
      optPars <- NULL
    }
    
    # Native area for projecting
    biolayers_cropc = readRDS(paste0(gap_dir, "/", species, "/", run_version, "/bioclim/crop_narea.RDS"))
    
    #load occurrence points
    xy_data <- read.csv(paste(occ_dir,"/no_sea/",species,".csv",sep=""),header=T)
    
    ###
    # ok lots of weirdness here. Not sure what this dataframe is expected to look like coming in. 
    # good questions for down south
    
    # Run alternatives #paste(sp_dir,"/bioclim/narea_mask.tif",sep="")
    if(!file.exists(paste0(gap_dir, "/", species, "/", run_version, "/modeling/alternatives/ca50_total_narea.tif"))){
      #xy_data <- optPars@occ.pts[,c("LON","LAT")]; xy_data <- as.data.frame(xy_data)
      #names(xy_data) <- c("lon", "lat")
      create_buffers(xy = xy_data,
                     msk = raster(paste(gap_dir,"/",species,"/",run_version,"/bioclim/narea_mask.tif",sep="")), # cambiar a mask de native area
                     buff_dist = 0.5,
                     format = "GTiff",
                     filename = paste0(gap_dir, "/", species, "/", run_version, "/modeling/alternatives/ca50_total_narea.tif"))
    }
    
    # Output folder
    crossValDir <- paste0(gap_dir, "/", species, "/", run_version, "/modeling/maxent")
    ### I dont think we need to do this step.
    ### why is type referencing year? 
    # xy_data$type<-as.numeric(as.character(xy_data$type))
    xy_data_na<-subset(xy_data, is.na(xy_data$type))
    # xy_data<-subset(xy_data, xy_data$type>=1950)
    # xy_data<-rbind(xy_data, xy_data_na)
    xy_data<-xy_data[,c("lon","lat")]
    rm(xy_data_na)
    
    
    ### DC Reduced this from 10 to 3 to test if the Fraterna model would run or not... I don't think this is a defencable action but it's happening. 
    if(nrow(xy_data) >= 3){
      if(!file.exists(paste0(crossValDir, "/modeling_results.", species, ".RDS"))){
        cat("Starting modeling process for species:", species, "\n")
        
        # ---------------- #
        # Inputs
        # ---------------- #
        
        # Loading climate worldwide rasters
        # rst_dir <- "//dapadfs/Workspace_cluster_9/Aichi13/parameters/biolayer_2.5/raster" PLEASE REVIEW
        #rst_dir <- paste(par_dir,"/biolayer_2.5/raster",sep="")
        rst_fls <- list.files(path = rst_dir, full.names = T)
        rst_fls <- rst_fls[grep(pattern = "*.tif$", x = rst_fls)]
        rst_fls <- raster::stack(rst_fls)
        
        
        # Determine background points
        cat("Creating background for: ", species, "\n")
        
        
        #msk <- raster("//dapadfs/Workspace_cluster_9/Aichi13/parameters/world_mask/raster/mask.tif") #PLEASE REVIEW
        # msk <- msk_global
        # msk_pts <- raster::rasterToPoints(msk)
        # msk_pts <- as.data.frame(msk_pts)
        # msk_pts <- msk_pts[which(msk_pts$y > -60),]
        # msk_pts$mask <- NULL
        # names(msk_pts) <- c("lon", "lat")
        # msk_pts <- msk_pts[complete.cases(msk_pts),]
        # msk_pts$cellID <- cellFromXY(object = msk, xy = msk_pts[,c("lon", "lat")])
        # #occ_cellID <- cellFromXY(object = msk, xy = optPars@occ.pts[,c("LON","LAT")])
        # occ_cellID <- cellFromXY(object = msk, xy = xy_data[,c("lon","lat")])
        # #msk_pts <- msk_pts[setdiff(msk_pts$cellID, occ_cellID),]; rm(occ_cellID)
        # msk_pts <- msk_pts[which(!msk_pts$cellID %in% occ_cellID),]
        # rownames(msk_pts) <- 1:nrow(msk_pts); rm(msk)
        # 
        # if(nrow(xy_data) >= 50){
        #   set.seed(1234)
        #   smpl <- base::sample(rownames(msk_pts), nrow(xy_data)*10, replace = F)
        #   bck_data <- msk_pts[na.omit(match(smpl, rownames(msk_pts))),]; rm(smpl)
        # } else {
        #   set.seed(1234)
        #   smpl <- base::sample(rownames(msk_pts), nrow(xy_data)*100, replace = F)
        #   bck_data <- msk_pts[na.omit(match(smpl, rownames(msk_pts))),]; rm(smpl)
        # }
        # bck_data$cellID <- NULL
        # bck_data$species <- species
        # 
        
        ### DC 
        # attempt to translate the native area shp to raster, Took two hours for the US and Mexico. Not a viable option 
        # Sys.time()
        # natRast <- rasterize(natArea, rst_fls@layers[1][[1]])
        # 
        ## DC replacing the back ground point generation with new method that accounts for native area 
        numberBackground <- function(area){
          n <- gArea(area)*58
          if( n >= 5000){
            n <- 5000}else{
              n <- n
            }
          return(n)
        }
        natArea <- readOGR(paste0(gap_dir, '/',species,'/',run_version,  "/bioclim/narea.shp"))
        print("natArea")
        n <- numberBackground(natArea)
        bck_data <- spsample(natArea, n = n, type = "random" )
        print('background points')
        
        
        #extract values to the presence points 
        xy_data_bio <- cbind(xy_data, rst_vx$extract_points(sp = sp::SpatialPoints(xy_data[,c("lon", "lat")])))
        xy_data_bio <- xy_data_bio[complete.cases(xy_data_bio),]
        xy_data <- xy_data_bio[,c("lon","lat")]
        print('bio done')
        
        ####DC 
        #Steps test background points for overlap with presnce data 
        # 1. buffer values from known presece locations by 0.000556 
        # 2. run an intersect between buffer and background points 
        # 3. extract all values to background points 
        # 4. subsut background base on if they intersected know presence or not 
        
        
        # create the point buffer
        # 1. buffer values from known presece locations by 0.000556 
        presBuff <- gBuffer(sp::SpatialPoints(xy_data[,c("lon", "lat")]), width=0.000556) #width=0.000556
        crs(presBuff) <- crs(bck_data)
        # convert to spatial dataframe 
        
        # 2. run an intersect between buffer and background point
        intersect <- data.frame(over(bck_data, presBuff))
        
        # 3. extract all values to background points 
        #extract bio variables to check that no NAs are present
        bck_data_bio <-as.data.frame(cbind(bck_data@coords, rst_vx$extract_points(bck_data)))
        
        ###DC Might need to come back to this but I'm going to leave this step out for now. Probably just do it later
        # bck_data_bio <- bck_data_bio[complete.cases(bck_data_bio),]
        # 
        # 
        # bck_data <- bck_data_bio[,c("lon","lat","species")]
        #  
        #  
        
        
        # 4. subsut background base on if they intersected know presence or not 
        # join intersect values back to 
        bck_data_bio$intersect <- intersect$over.bck_data..presBuff.
        bck_data_bio <- bck_data_bio[is.na(bck_data_bio$intersect), ] %>%
          subset(select = -c(intersect))
        names(bck_data_bio) <- names(xy_data_bio)
        bck_data_bio <- bck_data_bio[complete.cases(bck_data_bio),]
        
        
        
        # filter to remove point with the same lat long. 
        # library(dplyr)
        # before <- nrow(bck_data_bio)
        # bck_data_bio <- anit_join(bck_data_bio, xy_data_bio)
        # after <- nrow(bck_data_bio)
        # 
        # print(paste0("there were", before - after, " background data points that were the same as the present "))
        # 
        #put together both datasets
        #bck_data_bio$species <- NULL
        xy_mxe <- rbind(bck_data_bio, xy_data_bio)
        xy_mxe <- xy_mxe[,c(3:ncol(xy_mxe))]
        names(xy_mxe) <- names(rst_fls)
        #row.names(xy_mxe) <- 1:nrow(xy_mxe)
        print("one data")
        # ---------------- #
        # Modeling
        # ---------------- #
        
        
        
        library(randomForest)
                library(VSURF)
                #
                presenceAbsence <- as.factor(c(rep(0,nrow(bck_data_bio)),rep(1,nrow(xy_data_bio))))

                # # #vsurf
                # vsurf1 <- VSURF(x=xy_mxe , y=presenceAbsence , ntree = 2000 , parallel = TRUE)
                vsurfThres <- VSURF_thres(x=xy_mxe , y=presenceAbsence , ntree = 2000 , parallel = TRUE)
                # orderThres <- names(xy_mxe)[vsurfThres$varselect.thres]
                # orderInterp <- names(xy_mxe)[vsurf1$varselect.interp]
                # orderpred <- names(xy_mxe)[vsurf1$varselect.pred]

                # #rfUtilities
                # varimportance_covSE <- rf.modelSel(xdata = xy_mxe, ydata =presenceAbsence, imp.scale = "se")
                # orderedSE <-  order(-varimportance_covSE$imp)
                # varimportance_covMIR <- rf.modelSel(xdata = xy_mxe, ydata =presenceAbsence, imp.scale = "mir")
                # orderedMIR <-  order(-varimportance_cov$imp)[1:15]
                #

                ###
                #correlation matrix
                ###

                # define predictor list based on Run
                inputPredictors <- vsurfThres$varselect.thres
                #
                # # ordered predictors from our variable selection
                predictors <- xy_mxe[,c(inputPredictors)]
                # Calculate correlation coefficient matrix
                correlation <-cor(predictors, method="pearson")
                #change self correlation value

                # #define the list of top 15 predictors
                varNames <- colnames(correlation)
                #loop through the top 5 predictors to remove correlated varables.
                for( i in 1:5){
                  print(i)
                  # Test for correlations with predictors
                  vars <- correlation[(i+1):nrow(correlation),i] > 0.7 | correlation[(i+1):nrow(correlation),i] < -0.7
                  # Select correlated values names
                  corVar <- names(which(vars == TRUE))
                  #test is any correlated variables exist
                  if(length(corVar) >0 ){
                    # loop through the list of correlated variables
                    for(j in corVar){
                      # remove varable name from variable list
                      varNames <- varNames[which(varNames != j)]
                      print(varNames)
                      # remove row from correlation dataframe ### Indexing on the this is not working at the moment. Leave out for the time being.
                      # correlation <- correlation[!vars,]
                      # print(dim(correlation))
                      print(paste0("the variable ", j, " was removed"))
                    }

                  } else{
                    print("no correlated Varaibles")
                  }
                }


                xy_mxe <- xy_mxe[,varNames]

        # Fitting final model
                cat("Performing MaxEnt modeling  for: ", species, "\n")
                        tryCatch(expr = {
                          #fit maxent
                          fit <- dismo::maxent(x = xy_mxe, # Climate
                                               #p = optPars@occ.pts[,c("LON","LAT")], # Occurrences
                                               #p = xy_data[,c("lon","lat")], # Occurrences
                                               p = c(rep(0,nrow(bck_data_bio)),rep(1,nrow(xy_data_bio))),
                                               #a = bck_data[,c("lon","lat")], # Pseudo-absences
                                               removeDuplicates = T,
                                               # args = c("nowarnings","replicates=5","linear=true","quadratic=true","product=true","threshold=true","hinge=true","pictures=false","plots=false"),
                                               args = c("nowarnings","replicates=10","pictures=false","plots=false", CreateMXArgs(optPars)),
                                               #path = crossValDir,
                                               silent = F)

                          #copy maxent files from temp dir
                          mxe_outdir <- fit@html
                          mxe_outdir <- gsub("/maxent.html","",mxe_outdir,fixed=T)
                          mxe_fls <- list.files(mxe_outdir,pattern=".lambdas$",full.names=T)
                          xs <- file.copy(mxe_fls, crossValDir)

                          #aquire rep numbe
                          rep_number<-length(fit@models)
                        },
                        error = function(e){
                          cat("Modeling process failed:", species, "\n")
                          return("Done\n")
                        })
                        file.copy(fit@html, crossValDir)

                        #fls.rm <- list.files(crossValDir, full.names = T)
                        #fls.rm <- fls.rm[setdiff(1:length(fls.rm), c(grep(pattern = paste0(sp, ".csv.RData"), fls.rm), grep(pattern = "*.lambdas$", fls.rm)))]
                        #file.remove(fls.rm)

                        # ---------------- #
                        # Outputs
                        # ---------------- #

                        # Extract climate data for projecting
                        pnts <- rasterToPoints(x = biolayers_cropc)
                        pnts <- as.data.frame(pnts)

                        pnts$cellID <- cellFromXY(object = biolayers_cropc[[1]], xy = pnts[,1:2])

                        # Fix crossvalidation path
                        setwd(crossValDir)

                        # Do projections
                        # k: corresponding fold
                        # pnts: data.frame with climate data for all variables on projecting zone
                        # tmpl_raster: template raster to project
                        cat("Performing projections using lambda files for: ", species, "\n")

                        pred <- raster::stack(lapply(1:rep_number, function(x) make.projections(x, pnts = pnts, tmpl_raster = biolayers_cropc[[1]])))

                        # Saving results
                        results <- list(model = fit,
                                        projections = pred,
                                        occ_predictions = raster::extract(x = pred, y = xy_data[,c("lon","lat")]),
                                        bck_predictions = raster::extract(x = pred, y = bck_data@coords))

                        cat("Saving RDS File with Models outcomes for: ", species, "\n")
                        saveRDS(object = results, file = paste0(crossValDir, "/modeling_results.", species, ".RDS"))

                        cat("Saving Median and SD rasters for: ", species, "\n")
                        spMedian <- raster::calc(pred, fun = function(x) median(x, na.rm = T))
                        raster::writeRaster(x = spMedian, filename = paste0(crossValDir, "/spdist_median.tif"), overwrite=TRUE)
                        spSD <- raster::calc(pred, fun = function(x) sd(x, na.rm = T))
                        raster::writeRaster(x = spSD, filename = paste0(crossValDir, "/spdist_sd.tif"), overwrite=TRUE)

                        # ---------------- #
                        # Evaluation metrics
                        # ---------------- #

                        # Extracting metrics for 5 replicates
                        cat("Gathering replicate metrics  for: ", species, "\n")
                        evaluate_table <- metrics_function(species)
                        #evaluate_table<-read.csv(paste0(crossValDir,"/","eval_metrics_rep.csv"),header=T)

                        # Apply threshold from evaluation
                        cat("Thresholding using Max metrics  for: ", species, "\n")
                        thrsld <- as.numeric(mean(evaluate_table[,"Threshold"],na.rm=T))
                        if (!file.exists(paste0(crossValDir, "/spdist_thrsld.tif"))) {
                          spThrsld <- spMedian
                          spThrsld[which(spThrsld[] >= thrsld)] <- 1
                          spThrsld[which(spThrsld[] < thrsld)] <- 0
                          raster::writeRaster(x = spThrsld, filename = paste0(crossValDir, "/spdist_thrsld.tif"))
                        } else {
                          spThrsld <- raster(paste0(crossValDir, "/spdist_thrsld.tif"))
                        }

                        # Gathering final evaluation table
                        x <- evaluate_function(species, evaluate_table)
                        #return(cat("Process finished successfully for specie:", species, "\n"))
                      } else {
                        cat("Species:", species, "has been already modeled\n")
                      }
      ### DC changes clause here from 10 to 3  to account for fraterna 
                    } else {if(base::nrow(xy_data)<=3 & base::nrow(xy_data)>0  ) {
                      cat("Species:", species, "only has", nrow(xy_data), "coordinates, it is not appropriate for modeling\n")
                      crossValDir <- paste0(gap_dir, "/", species, "/", run_version, "/modeling/maxent")
                      evaluate_table <- data.frame(species=species,training=NA,testing=NA,ATAUC=NA,STAUC=NA,
                                                   Threshold=NA,Sensitivity=NA,Specificity=NA,TSS=NA,PCC=NA,
                                                   nAUC=NA,cAUC=NA,ASD15=NA,VALID=FALSE)
                      evaluate_table <- write.csv(evaluate_table, paste0(crossValDir,"/","eval_metrics.csv"),row.names=F,quote=F)
                    }
                    }



                  } else {
                    cat("Species:", species, "has no data with coordinates, and cannot be modeled\n")
                    crossValDir <- paste0(gap_dir, "/", species, "/", run_version, "/modeling/maxent")
                    evaluate_table <- data.frame(species=species,training=NA,testing=NA,ATAUC=NA,STAUC=NA,
                                                 Threshold=NA,Sensitivity=NA,Specificity=NA,TSS=NA,PCC=NA,
                                                 nAUC=NA,cAUC=NA,ASD15=NA,VALID=FALSE)
                    evaluate_table <- write.csv(evaluate_table, paste0(crossValDir,"/","eval_metrics.csv"),row.names=F,quote=F)
                  }
                  return(species)
                }




# ================================================================================================================================= #
# Evaluate projections results
# ================================================================================================================================= #
# 
# # Do projections
# system.time(expr = {
#   pred <- raster::stack(lapply(1:5, function(x) make.projections(x, pnts = pnts, tmpl_raster = biolayers_cropc[[1]])))
# })
# system.time(expr = {pred2 <- predict(fit, biolayers_cropc)})
# 
# par(mfrow = c(2,3))
# for(i in 1:5){
#   hist(pred[[i]][!is.na(pred[[i]][])] - pred2[[i]][!is.na(pred2[[i]][])])
# }
# 
# j.size <- "-mx8000m"
# maxentApp <- "C:/Users/HAACHICANOY/Documents/R/win-library/3.4/dismo/java/maxent.jar"
# projLayers <- "C:/Users/HAACHICANOY/Downloads/Climate"
# 
# # outDir <- "C:/Users/HAACHICANOY/Downloads/Climate"
# # for(i in 1:nlayers(biolayers_cropc)){
# #   writeRaster(x = biolayers_cropc[[i]], filename = paste0(outDir, "/", names(biolayers_cropc)[i], ".asc"))
# # }
# 
# for(i in 1:5){
#   lambdaFile <- paste0("C:/Users/HAACHICANOY/Downloads/Model2/species_", i-1, ".lambdas")
#   outGrid <- paste0("C:/Users/HAACHICANOY/Desktop/fold", i-1, ".asc")
#   system(paste("java", j.size, "-cp", maxentApp, "density.Project", lambdaFile, projLayers, outGrid, "nowarnings", "fadebyclamping", "-r", "-a", "-z"), wait=TRUE)
# }
# 
# lambdaFile <- paste0("C:/Users/HAACHICANOY/Downloads/Model2/species_0.lambdas")
# outGrid <- paste0("C:/Users/HAACHICANOY/Desktop/fold0.asc")
# system(paste("java", j.size, "-cp", maxentApp, "density.Project", lambdaFile, projLayers, outGrid, "nowarnings", "fadebyclamping", "-r", "-a", "-z"), wait=TRUE)
# 
# javaFold_0 <- raster::raster("C:/Users/HAACHICANOY/Desktop/fold0.asc")
# javaFold_1 <- raster::raster("C:/Users/HAACHICANOY/Desktop/fold1.asc")
# 
# summary(javaFold_0[!is.na(javaFold_0[])] - pred[[1]][!is.na(pred[[1]][])])
# summary(javaFold_0[!is.na(javaFold_0[])] - pred2[[1]][!is.na(pred2[[1]][])])
# 
# summary(javaFold_1[!is.na(javaFold_1[])] - pred[[2]][!is.na(pred[[2]][])])
# summary(javaFold_1[!is.na(javaFold_1[])] - pred2[[2]][!is.na(pred2[[2]][])])























# # Modeling approach, pseudo-absences selection, calibration, and fitting models
# # J. Soto, C. Sosa, H. Achicanoy
# # CIAT, 2018
# 
# # R options
# # g <- gc(reset = T); rm(list = ls()); options(warn = -1); options(scipen = 999)
# 
# # Load libraries
# suppressMessages(library(tidyverse))
# suppressMessages(library(dismo))
# suppressMessages(library(velox))
# suppressMessages(library(ff))
# suppressMessages(library(data.table))
# suppressMessages(library(sp))
# # # Important scripts
# #repo_dir<-"C:/Users/MVDIAZ/Desktop/src"
# source(paste(repo_dir,"/1_modeling/1_1_maxent/create_mx_args.R",sep=""))
# source(paste(repo_dir,"/1_modeling/1_1_maxent/do_projections.R",sep=""))
# source(paste(repo_dir,"/1_modeling/1_1_maxent/evaluating.R",sep=""))
# source(paste(repo_dir,"/1_modeling/1_1_maxent/nullModelAUC.R",sep=""))
# source(paste(repo_dir,"/1_modeling/1_2_alternatives/create_buffers.R",sep=""))
# source(paste(repo_dir,"/config.R",sep=""))
# 
# 
# # From config file
# # run_version <- "v1"
# # gap_dir <- "//dapadfs/Workspace_cluster_9/Aichi13/gap_analysis"
# 
# # --------------------------------------------------------------------- #
# # Modeling function
# # --------------------------------------------------------------------- #
# #species="2753717"
# #spModeling(species)
# #base_dir="//dapadfs"
# 
# #species <- species1
# 
# spModeling <- function(species){
#   # run config function
#   config(dirs=T,modeling=T)
#   
#   if (file.exists(paste(occ_dir,"/no_sea/",species,".csv",sep=""))) {
#     # Load calibration results
#     if (file.exists(paste0(gap_dir, "/", species, "/", run_version, "/modeling/maxent/", species, ".csv.RData"))) {
#       load(paste0(gap_dir, "/", species, "/", run_version, "/modeling/maxent/", species, ".csv.RData"))
#     } else {
#       optPars <- NULL
#     }
#     
#     # Native area for projecting
#     biolayers_cropc = readRDS(paste0(gap_dir, "/", species, "/", run_version, "/bioclim/crop_narea.RDS"))
#     
#     #load occurrence points
#     xy_data <- read.csv(paste(occ_dir,"/no_sea/",species,".csv",sep=""),header=T)
#     
#     ###
#     # ok lots of weirdness here. Not sure what this dataframe is expected to look like coming in. 
#     # good questions for down south
#    
#     # Run alternatives #paste(sp_dir,"/bioclim/narea_mask.tif",sep="")
#     if(!file.exists(paste0(gap_dir, "/", species, "/", run_version, "/modeling/alternatives/ca50_total_narea.tif"))){
#       #xy_data <- optPars@occ.pts[,c("LON","LAT")]; xy_data <- as.data.frame(xy_data)
#       #names(xy_data) <- c("lon", "lat")
#       create_buffers(xy = xy_data,
#                      msk = raster(paste(gap_dir,"/",species,"/",run_version,"/bioclim/narea_mask.tif",sep="")), # cambiar a mask de native area
#                      buff_dist = 0.5,
#                      format = "GTiff",
#                      filename = paste0(gap_dir, "/", species, "/", run_version, "/modeling/alternatives/ca50_total_narea.tif"))
#     }
#     
#     # Output folder
#     crossValDir <- paste0(gap_dir, "/", species, "/", run_version, "/modeling/maxent")
#     ### I dont think we need to do this step.
#     ### why is type referencing year? 
#     # xy_data$type<-as.numeric(as.character(xy_data$type))
#     xy_data_na<-subset(xy_data, is.na(xy_data$type))
#     # xy_data<-subset(xy_data, xy_data$type>=1950)
#     # xy_data<-rbind(xy_data, xy_data_na)
#     xy_data<-xy_data[,c("lon","lat")]
#     rm(xy_data_na)
#     
#       if(nrow(xy_data) >= 10){
#       if(!file.exists(paste0(crossValDir, "/modeling_results.", species, ".RDS"))){
#         cat("Starting modeling process for species:", species, "\n")
#         
#         # ---------------- #
#         # Inputs
#         # ---------------- #
#         
#         # Loading climate worldwide rasters
#         # rst_dir <- "//dapadfs/Workspace_cluster_9/Aichi13/parameters/biolayer_2.5/raster" PLEASE REVIEW
#         #rst_dir <- paste(par_dir,"/biolayer_2.5/raster",sep="")
#         rst_fls <- list.files(path = rst_dir, full.names = T)
#         rst_fls <- rst_fls[grep(pattern = "*.tif$", x = rst_fls)]
#         rst_fls <- raster::stack(rst_fls)
#         
#         
#         
#         # Determine background points
#         cat("Creating background for: ", species, "\n")
#         
#         ## DC replacing the back ground point generation with new method that accounts for native area 
#         numberBackground <- function(area){
#           n <- gArea(area)*58
#           if( n >= 5000){
#             n <- 5000}else{
#               n <- floor(n)
#             }
#           return(n)
#         }
#         
#         natArea <- readOGR(paste0(gap_dir, '/',species,'/',run_version,  "/bioclim/narea.shp"))
#         
#         
#         
#         #msk <- raster("//dapadfs/Workspace_cluster_9/Aichi13/parameters/world_mask/raster/mask.tif") #PLEASE REVIEW
#         msk <- msk_global
#         msk_thin <- mask(msk, natArea)
#         msk_pts <- raster::rasterToPoints(msk_thin)
#         msk_pts <- as.data.frame(msk_pts)
#         msk_pts <- msk_pts[which(msk_pts$y > -60),]
#         msk_pts$mask <- NULL
#         names(msk_pts) <- c("lon", "lat")
#         msk_pts <- msk_pts[complete.cases(msk_pts),]
#         msk_pts$cellID <- cellFromXY(object = msk, xy = msk_pts[,c("lon", "lat")])
#         #occ_cellID <- cellFromXY(object = msk, xy = optPars@occ.pts[,c("LON","LAT")])
#         occ_cellID <- cellFromXY(object = msk, xy = xy_data[,c("lon","lat")])
#         #msk_pts <- msk_pts[setdiff(msk_pts$cellID, occ_cellID),]; rm(occ_cellID)
#         msk_pts <- msk_pts[which(!msk_pts$cellID %in% occ_cellID),]
#         rownames(msk_pts) <- 1:nrow(msk_pts); rm(msk)
# 
#         # if(nrow(xy_data) >= 50){
#         #   set.seed(1234)
#         #   smpl <- base::sample(rownames(msk_pts), nrow(xy_data)*10, replace = F)
#         #   bck_data <- msk_pts[na.omit(match(smpl, rownames(msk_pts))),]; rm(smpl)
#         # } else {
#         #   set.seed(1234)
#         #   smpl <- base::sample(rownames(msk_pts), nrow(xy_data)*100, replace = F)
#         #   bck_data <- msk_pts[na.omit(match(smpl, rownames(msk_pts))),]; rm(smpl)
#         # }
# 
#         set.seed(1234)
#         
#         #Define number of background points 
#         n <- numberBackground(natArea)
#         
#         smpl <- base::sample(rownames(msk_pts), n, replace = F)
#         bck_data <- msk_pts[na.omit(match(smpl, rownames(msk_pts))),]; rm(smpl)
#         
#         bck_data$cellID <- NULL
#         bck_data$species <- species
# 
#         
#         #extract bio variables to check that no NAs are present
#         sp = sp::SpatialPoints(bck_data[,c("lon", "lat")])
#         bck_data_bio<- raster::extract(rst_vx, sp)
#         
# 
#         # bck_data_bio <- cbind(bck_data, rst_vx$extract_points(sp = sp::SpatialPoints(bck_data[,c("lon", "lat")])))
#         # bck_data_bio <- bck_data_bio[complete.cases(bck_data_bio),]
#         # bck_data <- bck_data_bio[,c("lon","lat","species")]
#         # 
#         #do the same for presences
#         
#         ###DC addition for 30arc sec
#         sp1 = sp::SpatialPoints(xy_data[,c("lon", "lat")])
#         xy_data_bio<- raster::extract(rst_vx, sp1)
#         # 
#         # xy_data_bio <- cbind(xy_data, rst_vx$extract_points(sp = sp::SpatialPoints(xy_data[,c("lon", "lat")])))
#         # xy_data_bio <- xy_data_bio[complete.cases(xy_data_bio),]
#         # xy_data <- xy_data_bio[,c("lon","lat")]
#         # 
# 
# 
#         # ---------------- #
#         # RF model selection  
#         # ---------------- #
#         
#         
#         # # #put together both datasets
#         # bck_data_bio$species <- NULL
#         xy_mxe <- rbind(bck_data_bio, xy_data_bio)
#         xy_mxe <- xy_mxe[,c(3:ncol(xy_mxe))]
#         names(xy_mxe) <- names(rst_fls)
#         row.names(xy_mxe) <- 1:nrow(xy_mxe)
#         
#         
#         # #install.packages('randomForest')
#         # install.packages('rfUtilities')
#         # install.packages('VSURF')
#         library(randomForest)
#         library(rfUtilities)
#         library(VSURF)
#         # 
#         presenceAbsence <- as.factor(c(rep(0,nrow(bck_data_bio)),rep(1,nrow(xy_data_bio))))
# 
#         # # #vsurf 
#         # vsurf1 <- VSURF(x=xy_mxe , y=presenceAbsence , ntree = 2000 , parallel = TRUE)
#         vsurfThres <- VSURF_thres(x=xy_mxe , y=presenceAbsence , ntree = 2000 , parallel = TRUE)
#         # orderThres <- names(xy_mxe)[vsurfThres$varselect.thres]
#         # orderInterp <- names(xy_mxe)[vsurf1$varselect.interp]
#         # orderpred <- names(xy_mxe)[vsurf1$varselect.pred]
#         
#         # #rfUtilities 
#         # varimportance_covSE <- rf.modelSel(xdata = xy_mxe, ydata =presenceAbsence, imp.scale = "se")
#         # orderedSE <-  order(-varimportance_covSE$imp)
#         # varimportance_covMIR <- rf.modelSel(xdata = xy_mxe, ydata =presenceAbsence, imp.scale = "mir")
#         # orderedMIR <-  order(-varimportance_cov$imp)[1:15]
#         # 
# 
#         ###
#         #correlation matrix
#         ###
# 
#         # define predictor list based on Run
#         inputPredictors <- vsurfThres
#         # 
#         # # ordered predictors from our variable selection
#         predictors <- xy_mxe[,c(inputPredictors)]
#         # Calculate correlation coefficient matrix
#         correlation <-cor(predictors, method="pearson")
#         #change self correlation value
# 
#         # #define the list of top 15 predictors
#         varNames <- colnames(correlation)
#         #loop through the top 5 predictors to remove correlated varables.
#         for( i in 1:5){
#           print(i)
#           # Test for correlations with predictors
#           vars <- correlation[(i+1):nrow(correlation),i] > 0.7 | correlation[(i+1):nrow(correlation),i] < -0.7
#           # Select correlated values names
#           corVar <- names(which(vars == TRUE))
#           #test is any correlated variables exist
#           if(length(corVar) >0 ){
#             # loop through the list of correlated variables
#             for(j in corVar){
#               # remove varable name from variable list
#               varNames <- varNames[which(varNames != j)]
#               print(varNames)
#               # remove row from correlation dataframe ### Indexing on the this is not working at the moment. Leave out for the time being.
#               # correlation <- correlation[!vars,]
#               # print(dim(correlation))
#               print(paste0("the variable ", j, " was removed"))
#             }
# 
#           } else{
#             print("no correlated Varaibles")
#           }
#         }
# 
# 
# 
#         # rf_modelBIALL <- randomForest(x = xy_mxe, y = presenceAbsence, ntree = 2000)
#         # rf_modelBIALL
#         # 
#         # 
#         # rf_modelBIse = randomForest(x = xy_mxe[orderedSE], y = presenceAbsence, ntree = 2000)
#         # rf_modelBIse
#         # 
#         # 
#         # rf_modelBIMIR = randomForest(x = xy_mxe[orderedMIR], y = presenceAbsence, ntree = 2000)
#         # rf_modelBIMIR
#         # 
#         # rf_modelBIVS = randomForest(x = xy_mxe[orderVsurf], y = presenceAbsence, ntree = 2000)
#         # rf_modelBIVS
#         # 
#         # 
#         # 
#         # predicted <- predict(rf_modelBIVS)
#         # observed <- presenceAbsence
#         # aos <- accuracy(pred=predicted, obs=observed)
#         # aos
#         # 
#         # xy_mxeRF <- xy_mxe[orderVsurf]
#         
#         # ---------------- #
#         # Variable importance
#         # ---------------- #
#         # install.packages("plsdepot")
#         # install.packages("usdm")
#         # library(plsdepot)
#         # library(usdm)
#         # 
#         # y <- xy_data_bio
#         # 
#         # z = nipals(Data=y[,8:ncol(y)], comps=5, scaled=T)
#         # vars1 = z$cor.xt[,1] > 0.7 | z$cor.xt[,1] < -0.7
#         # vars2 = z$cor.xt[,2] > 0.7 | z$cor.xt[,2] < -0.7
#         # vars = c(vars1, vars2)
#         # vars = names(vars[which(vars==TRUE)])
#         # 
#         # 
#         # x2 <- bck_data_bio[vars]
#         # 
#         # vif_res = vifstep(x2, th=10)
#         # vif_res = sort(as.character(vif_res@results$Variables))
#         # vif_names <- names(rst_fls)[as.numeric(vif_res)]
# 
# 
#         #put together both datasets
#         #bck_data_bio$species <- NULL
#         xy_mxe <- as.data.frame(rbind(bck_data_bio, xy_data_bio))
#         #xy_mxe <- xy_mxe[,c(3:ncol(xy_mxe))]
#         names(xy_mxe) <- names(rst_vx)
#         
#         # define where the predictor list is coming from 
#         predictors <- varNames
# 
# 
#         #filter so only predictors are used.
#         xy_mxe <- xy_mxe[,predictors]
# 
# 
# 
#         # ---------------- #
#         # Modeling
#         # ---------------- #
#         
#         # Fitting final model
#         
#         cat("Performing MaxEnt modeling  for: ", species, "\n")
#         tryCatch(expr = {
#           #fit maxent
#           fit <- dismo::maxent(x = xy_mxe, # Climate
#                                #p = optPars@occ.pts[,c("LON","LAT")], # Occurrences
#                                #p = xy_data[,c("lon","lat")], # Occurrences
#                                p = c(rep(0,nrow(bck_data_bio)),rep(1,nrow(xy_data_bio))),
#                                #a = bck_data[,c("lon","lat")], # Pseudo-absences
#                                removeDuplicates = T,
#                                # args = c("nowarnings","replicates=5","linear=true","quadratic=true","product=true","threshold=true","hinge=true","pictures=false","plots=false"),
#                                args = c("nowarnings","replicates=5","pictures=false","plots=false", CreateMXArgs(optPars)),
#                                #path = crossValDir,
#                                silent = F)
#           
#           #copy maxent files from temp dir
#           mxe_outdir <- fit@html
#           mxe_outdir <- gsub("/maxent.html","",mxe_outdir,fixed=T)
#           mxe_fls <- list.files(mxe_outdir,pattern=".lambdas$",full.names=T)
#           xs <- file.copy(mxe_fls, crossValDir)
#           
#           #aquire rep numbe
#           rep_number<-length(fit@models)
#         },
#         error = function(e){
#           cat("Modeling process failed:", species, "\n")
#           return("Done\n")
#         })
#         file.copy(fit@html, crossValDir)
#         
#         #fls.rm <- list.files(crossValDir, full.names = T)
#         #fls.rm <- fls.rm[setdiff(1:length(fls.rm), c(grep(pattern = paste0(sp, ".csv.RData"), fls.rm), grep(pattern = "*.lambdas$", fls.rm)))]
#         #file.remove(fls.rm)
#         
#         # ---------------- #
#         # Outputs
#         # ---------------- #
#         
#         # Extract climate data for projecting
#         pnts <- rasterToPoints(x = biolayers_cropc)
#         pnts <- as.data.frame(pnts)
#         
#         pnts$cellID <- cellFromXY(object = biolayers_cropc[[1]], xy = pnts[,1:2])
#         
#         # Fix crossvalidation path
#         setwd(crossValDir)
#         
#         # Do projections
#         # k: corresponding fold
#         # pnts: data.frame with climate data for all variables on projecting zone
#         # tmpl_raster: template raster to project
#         cat("Performing projections using lambda files for: ", species, "\n")
#         
#         pred <- raster::stack(lapply(1:rep_number, function(x) make.projections(x, pnts = pnts, tmpl_raster = biolayers_cropc[[1]])))
#         
#         # Saving results
#         results <- list(model = fit,
#                         projections = pred,
#                         occ_predictions = raster::extract(x = pred, y = xy_data[,c("lon","lat")]),
#                         bck_predictions = raster::extract(x = pred, y = bck_data[,c("lon","lat")]))
#         
#         cat("Saving RDS File with Models outcomes for: ", species, "\n")
#         saveRDS(object = results, file = paste0(crossValDir, "/modeling_results.", species, ".RDS"))
#         
#         cat("Saving Median and SD rasters for: ", species, "\n")
#         spMedian <- raster::calc(pred, fun = function(x) median(x, na.rm = T))
#         raster::writeRaster(x = spMedian, filename = paste0(crossValDir, "/spdist_median.tif"), overwrite=TRUE)
#         spSD <- raster::calc(pred, fun = function(x) sd(x, na.rm = T))
#         raster::writeRaster(x = spSD, filename = paste0(crossValDir, "/spdist_sd.tif"), overwrite=TRUE)
#         
#         # ---------------- #
#         # Evaluation metrics
#         # ---------------- #
#         
#         # Extracting metrics for 5 replicates
#         cat("Gathering replicate metrics  for: ", species, "\n")
#         evaluate_table <- metrics_function(species)
#         #evaluate_table<-read.csv(paste0(crossValDir,"/","eval_metrics_rep.csv"),header=T)
#         
#         # Apply threshold from evaluation
#         cat("Thresholding using Max metrics  for: ", species, "\n")
#         thrsld <- as.numeric(mean(evaluate_table[,"Threshold"],na.rm=T))
#         if (!file.exists(paste0(crossValDir, "/spdist_thrsld.tif"))) {
#           spThrsld <- spMedian
#           spThrsld[which(spThrsld[] >= thrsld)] <- 1
#           spThrsld[which(spThrsld[] < thrsld)] <- 0
#           raster::writeRaster(x = spThrsld, filename = paste0(crossValDir, "/spdist_thrsld.tif"))
#         } else {
#           spThrsld <- raster(paste0(crossValDir, "/spdist_thrsld.tif"))
#         }
#         
#         # Gathering final evaluation table
#         x <- evaluate_function(species, evaluate_table)
#         #return(cat("Process finished successfully for specie:", species, "\n"))
#       } else {
#         cat("Species:", species, "has been already modeled\n")
#       }
#     } else {if(base::nrow(xy_data)<10 & base::nrow(xy_data)>0  ) {
#       cat("Species:", species, "only has", nrow(xy_data), "coordinates, it is not appropriate for modeling\n")
#       crossValDir <- paste0(gap_dir, "/", species, "/", run_version, "/modeling/maxent")
#       evaluate_table <- data.frame(species=species,training=NA,testing=NA,ATAUC=NA,STAUC=NA,
#                                    Threshold=NA,Sensitivity=NA,Specificity=NA,TSS=NA,PCC=NA,
#                                    nAUC=NA,cAUC=NA,ASD15=NA,VALID=FALSE)
#       evaluate_table <- write.csv(evaluate_table, paste0(crossValDir,"/","eval_metrics.csv"),row.names=F,quote=F)
#     }
#     }
#   
#     
#     
#   } else {
#     cat("Species:", species, "has no data with coordinates, and cannot be modeled\n")
#     crossValDir <- paste0(gap_dir, "/", species, "/", run_version, "/modeling/maxent")
#     evaluate_table <- data.frame(species=species,training=NA,testing=NA,ATAUC=NA,STAUC=NA,
#                                  Threshold=NA,Sensitivity=NA,Specificity=NA,TSS=NA,PCC=NA,
#                                  nAUC=NA,cAUC=NA,ASD15=NA,VALID=FALSE)
#     evaluate_table <- write.csv(evaluate_table, paste0(crossValDir,"/","eval_metrics.csv"),row.names=F,quote=F)
#   }
#   return(species)
# }
# 
# 
# 
# # ================================================================================================================================= #
# # Evaluate projections results
# # ================================================================================================================================= #
# # 
# # # Do projections
# # system.time(expr = {
# #   pred <- raster::stack(lapply(1:5, function(x) make.projections(x, pnts = pnts, tmpl_raster = biolayers_cropc[[1]])))
# # })
# # system.time(expr = {pred2 <- predict(fit, biolayers_cropc)})
# # 
# # par(mfrow = c(2,3))
# # for(i in 1:5){
# #   hist(pred[[i]][!is.na(pred[[i]][])] - pred2[[i]][!is.na(pred2[[i]][])])
# # }
# # 
# # j.size <- "-mx8000m"
# # maxentApp <- "C:/Users/HAACHICANOY/Documents/R/win-library/3.4/dismo/java/maxent.jar"
# # projLayers <- "C:/Users/HAACHICANOY/Downloads/Climate"
# # 
# # # outDir <- "C:/Users/HAACHICANOY/Downloads/Climate"
# # # for(i in 1:nlayers(biolayers_cropc)){
# # #   writeRaster(x = biolayers_cropc[[i]], filename = paste0(outDir, "/", names(biolayers_cropc)[i], ".asc"))
# # # }
# # 
# # for(i in 1:5){
# #   lambdaFile <- paste0("C:/Users/HAACHICANOY/Downloads/Model2/species_", i-1, ".lambdas")
# #   outGrid <- paste0("C:/Users/HAACHICANOY/Desktop/fold", i-1, ".asc")
# #   system(paste("java", j.size, "-cp", maxentApp, "density.Project", lambdaFile, projLayers, outGrid, "nowarnings", "fadebyclamping", "-r", "-a", "-z"), wait=TRUE)
# # }
# # 
# # lambdaFile <- paste0("C:/Users/HAACHICANOY/Downloads/Model2/species_0.lambdas")
# # outGrid <- paste0("C:/Users/HAACHICANOY/Desktop/fold0.asc")
# # system(paste("java", j.size, "-cp", maxentApp, "density.Project", lambdaFile, projLayers, outGrid, "nowarnings", "fadebyclamping", "-r", "-a", "-z"), wait=TRUE)
# # 
# # javaFold_0 <- raster::raster("C:/Users/HAACHICANOY/Desktop/fold0.asc")
# # javaFold_1 <- raster::raster("C:/Users/HAACHICANOY/Desktop/fold1.asc")
# # 
# # summary(javaFold_0[!is.na(javaFold_0[])] - pred[[1]][!is.na(pred[[1]][])])
# # summary(javaFold_0[!is.na(javaFold_0[])] - pred2[[1]][!is.na(pred2[[1]][])])
# # 
# # summary(javaFold_1[!is.na(javaFold_1[])] - pred[[2]][!is.na(pred[[2]][])])
# # summary(javaFold_1[!is.na(javaFold_1[])] - pred2[[2]][!is.na(pred2[[2]][])])
# 
