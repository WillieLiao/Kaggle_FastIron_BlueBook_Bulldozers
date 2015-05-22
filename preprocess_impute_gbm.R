# load(paste0(TRAIN_DATA_PATH, "merged-after-mfgfeats.rda"))

# Features are machine dependent
gp.size <- sapply(ProdGps, function(gp) length(merged$ProdGp[merged$ProdGp==gp]))
ttm <- vector("list")

for (ProdGp in ProdGps){
  # Only use features that have values in > 1% of sales.  Accounts for bad data and ProdGp dependent features.  Makes models more robust.
  print(ProdGp)  
  feats.gp.percent <- sapply(merged[merged$ProdGp==ProdGp, !(names(merged) %in% "SaleDate")], function(x) length(x[!is.na(x) & x!=""])) / gp.size[ProdGp]
  feats.gp.good <- c(names(which(feats.gp.percent > .01)), "SaleDate")
  print(names(which(feats.gp.percent <= .01)))
  ttm[[ProdGp]] <- droplevels(merged[merged$ProdGp==ProdGp, feats.gp.good])
}

rm(merged, ProdGp, feats.gp.good, feats.gp.percent, gp.size)

# lapply(ttm, function(ds) names(ds[sapply(ds, function(col) sum(is.na(col))>0)]))
# lapply(ttm, dim)

save(ttm, file=paste0(TRAIN_DATA_PATH, "ttm-before-impute.rda"))

##########################################################
# impute using gbm imputation
##########################################################
# gbm cannot handle > 1024 factors, so break it up by ProdGp
# Makes sense to do modelling by ProdGp anyways
# load(paste0(TRAIN_DATA_PATH, "ttm-preprocess-feats-", data_type, ".rda"))
#impute
require(imputation)
colDrop <- c('cpi_us','cpi_trucks','pc_agri','pc_contr','wp_agri','wp_contr')
colImpute <- c('Tire','cpi_us','cpi_trucks','pc_agri','pc_contr','wp_agri','wp_contr','YearMade','YearMade.min','YearMade.med','Age','Age.med','Age.min','SizeL','SizeU','SizeL.med','SizeU.med')

ttm <- foreach(ProdGp=ProdGps, tt=ttm, .packages="imputation") %dopar% {  
  set.seed(20130301)  
  colIgnore <- c('SalesID', 'SalePrice', 'SaleDate', 'MacID', 'ModelID', 'MKey', 'UseBand', 'Size', 'Hours', 'HoursCap')
  if (levels(tt$MBase) >= 1024) colIgnore <- c(colIgnore, 'MBase')
  imp <- gbmImpute(subset(tt, select=!names(tt) %in% colIgnore),
                   max.iters=1, cv.fold=1, n.trees=500)$x
  imp <- imp[, names(imp) %in% colImpute]
  names(imp) <- paste(names(imp), "imp", sep=".")
  tt <- data.frame(subset(tt, select=!names(tt) %in% colDrop), imp)  
  row.names(tt) <- tt$SalesID
  gc()  
  tt 
}

rm(imp, ProdGp, colIgnore, colImpute, colDrop)
names(ttm) <- ProdGps
groups <- ProdGps
save(ttm, groups, file=paste0(TRAIN_DATA_PATH, "ttm-imputed-orig.rda"))

#################################################
# explore imputation - conclusion is median gives better YearMade than YearMade.imp.
#################################################
# require(ggplot2)
# imp <- c()
# for (i in 1:6){  
#   imp <- rbind(imp, ttm[[i]][ttm[[i]]$SalesID %in% missing, grep("Year", names(ttm[[i]]))])
# }
# names(imp) <- c('s','y','min','med','y.imp','min.imp','med.imp')
# par(mfrow=c(2,2))
# apply(imp,2, hist)
#                  
# par(mfrow=c(2,2))
# qplot(s,y.imp,data=imp, alpha=I(1/10), geom="jitter")
# qplot(s,med,data=imp, alpha=I(1/10), geom="jitter")
# qplot(s,min,data=imp, alpha=I(1/10), geom="jitter")
# qplot(y.imp,med,data=imp, alpha=I(1/10), geom="jitter")
# 
# qplot(s,med.imp,data=imp, alpha=I(1/10), geom="jitter")
# qplot(s,min.imp,data=imp, alpha=I(1/10), geom="jitter")
# qplot(med,min,data=imp, alpha=I(1/10), geom="jitter")
# qplot(med.imp,min.imp,data=imp[is.na(imp$med),], alpha=I(1/10), geom="jitter")
# head(imp)
# rm(imp,ds)