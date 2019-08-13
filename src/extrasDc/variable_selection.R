# Ipomoea paper
# Variable selection for modeling

library(raster)
library(usdm)

# Read data with climatic and soils information

occ_taxa <- list.files(paste0(crop_dir,"/","occurrence_files"),full.names=T)
occ_taxa <- lapply(occ_taxa, read.csv)
occ_taxa <- Reduce(function(...) rbind(..., deparse.level=1), occ_taxa)
occ_taxa <- occ_taxa[order(occ_taxa$Taxon),]
occ_taxa <- occ_taxa[complete.cases(occ_taxa),]
occ_taxa <- unique(occ_taxa)
rownames(occ_taxa) <- 1:nrow(occ_taxa)

occ_list = lapply(unique(occ_taxa$Taxon), function(x){z=occ_taxa[which(occ_taxa$Taxon==x),]; rownames(z)=1:nrow(z); return(z)})
names(occ_list) = as.character(unique(occ_taxa$Taxon))

# ----------------------------------------------------------------------------------- #
# Nonlinear iterative partial least square algorithm
# ----------------------------------------------------------------------------------- #

# By specie
library(plsdepot)

nipals_by_specie = lapply(1:length(occ_list), function(x){
   y<-as.data.frame(occ_list[[x]])
   cat("processing NIPALS for ", as.character(unique(y$Taxon))," | ",x,"\n")


   x2<-lapply(1:ncol(unique(y[,4:ncol(y)])),function(x){
     y_temp<-unique(y[,4:ncol(y)])
     x2<-length(unique(y_temp[,x]))
     return(x2)
   })



   if(nrow(y)>4 & nrow(unique(y[,4:ncol(y)]))>5 & any(x2)>3){
     cat("processing NIPALS for ", as.character(unique(y$Taxon))," | ",x,"\n")

   z = nipals(Data=y[,4:ncol(y)], comps=5, scaled=T)
  vars1 = z$cor.xt[,1] > 0.7 | z$cor.xt[,1] < -0.7
  vars2 = z$cor.xt[,2] > 0.7 | z$cor.xt[,2] < -0.7
  vars = c(vars1, vars2)
  vars = names(vars[which(vars==TRUE)])
  return(vars)
}else if(nrow(unique(y[,4:ncol(y)]))<5  | any(x2)==1) {
  cat("ommiting NIPALS for ", as.character(unique(y$Taxon))," | ",x," LESS THAN 5 UNIQUE VALUES","\n")

  vars = colnames(y[,4:ncol(y)])
  return(vars)
}else{
  cat("ommiting NIPALS for ", as.character(unique(y$Taxon))," | ",x,"\n")

  vars = colnames(y[,4:ncol(y)])


}
  })
names(nipals_by_specie) = names(occ_list)

# Selected variables number
lapply(nipals_by_specie, length)

# Explore correlation level
nipals_vif_results = mapply(x=occ_list, y=nipals_by_specie, FUN=function(x,y)
{
  print(as.character(unique(x$Taxon)))

  z=as.data.frame(x[,paste(y)])
  if(nrow(z)<10){
    vif_res<-y
    return(vif_res)
  }else{
  #print(z)
  vif_res = vifstep(z, th=10)
  vif_res = sort(as.character(vif_res@results$Variables))
  return(vif_res)
  }
})
#code_cajanus = data.frame(Taxon_name=as.character(unique(occ$taxon_final)))
#code_cajanus$Taxon_code = c("57", "58", "59", "66", "67", "68", "60", "70", "61", "71", "72", "62", "63", "64", "65")
#names(nipals_vif_results) = as.character(code_cajanus$Taxon_code)

save(nipals_vif_results, file=paste0(crop_dir,"/","maxent_modeling/selected_variables.RData"))
