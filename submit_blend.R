cv.type <- ''
ds.types <- c('orig6','rank6')
feats.types <- c('base', 'many')

##########################
# get obs and SalesID
##########################
obs <- vector('list')
for (ds.type in ds.types){
  load(paste0(TRAIN_DATA_PATH, "-", ds.type, ".rda"))
  obs[[ds.type]] <- lapply(all.ds, function(ds) data.frame(SalesID=as.numeric(names(ds[["y_ts"]]))))
}
rm(all.ds)

##########################
# get min/max of SalePrice
##########################
load(paste0(TRAIN_DATA_PATH, "-orig6.rda"))
obs.orig <- vector('list')
obs.orig[['max']] <- lapply(all.ds, function(ds) max(ds[['y_tr']]))
obs.orig[['min']] <- lapply(all.ds, function(ds) min(ds[['y_tr']]))
obs.orig.SalePrice <- vector('list')
obs.orig.SalePrice <- lapply(all.ds, function(ds) ds[['y_tr']])
rm(all.ds)

##########################
# gbm
##########################
p.gbm <- obs[[1]]

for (ds.type in ds.types){    
  for (feats.type in feats.types){          
    fname <- paste('gbm', ds.type, feats.type, sep='-')
    fname <- paste0(SUBMISSION_PATH, fname, '.rda')
    load(fname)
    names(preds.gbm) <- groups
    for (group in groups){
      p <- preds.gbm[[group]]
      p <- data.frame(obs[[ds.type]][[group]], p) # add column of SalesID
      names(p) <- c("SalesID", paste('gbm',ds.type,feats.type,sep='_'))
      p.gbm[[group]] <- merge(p.gbm[[group]], p, by="SalesID", all.x=T, all.y=F, sort=F)
    }
  }
}
rm(preds.gbm,p)
save(p.gbm, file=paste0(SUBMISSION_PATH, 'sub_gbm.rda'))

##########################
# rf
##########################
p.rf <- obs[[1]]
for (group in groups){
  for (ds.type in ds.types){    
    for (feats.type in feats.types){          
      fname <- paste('rf', ds.type, group, feats.type, sep='-')
      fname <- paste0(SUBMISSION_PATH, fname, '.csv')
      p <- read.csv(fname, header=F)[,]
      p <- data.frame(obs[[ds.type]][[group]], p) # add column of SalesID
      names(p) <- c("SalesID", paste('rf',ds.type,feats.type,sep='_'))
      p.rf[[group]] <- merge(p.rf[[group]], p, by="SalesID", all.x=T, all.y=F, sort=F)
    }
  }
}
rm(p)
save(p.rf, file=paste0(SUBMISSION_PATH, 'sub_rf.rda'))

##########################
# bag
##########################
load(paste0(SUBMISSION_PATH, "bag.rda"))
p.bag <- obs[[1]]

ds.type <- 'orig6'
for (group in groups){
  p <- 0.5*preds.bags[[1]][[group]] + 0.5*preds.bags[[2]][[group]]    
  p <- data.frame(obs[[ds.type]][[group]], p) # add column of SalesID
  names(p) <- c("SalesID", 'bag')
  p.bag[[group]] <- merge(p.bag[[group]], p, by="SalesID", all.x=T, all.y=F, sort=F)
}
rm(preds.bags,p)
save(p.bag, file=paste0(SUBMISSION_PATH, 'sub_bag.rda'))

##########################
# blend
##########################
save(p.bag, p.gbm, p.rf, file=paste0(SUBMISSION_PATH, 'sub_all.rda'))
s.all <- obs[[1]]

# weights from fitting to 2009 May-Nov
# to "reproduce" (seeds not set in cv), just run everything with training data up to April 30, 2009 and use least squares for stacking.
# bag, gbm1, gbm2, gbm3, gbm4, rf1, rf2, rf3, rf4
weights <- 
  list(TTT = c(-0.010795, -0.159501, 0.343824, 0.330318, 0.104068, -0.066831, 0.389599, 0.218605, -0.270823, 0.116241), 
       WL = c(0.084622, -0.026895, 0.290624, -0.013783, 0.212654, -0.274945, 0.653439, 0.173416, -0.305501, 0.202336),
       TEX = c(0.015951, -0.089548, 0.021026, 0.214053, 0.438263, -0.046468, -0.095521, -0.274333, 0.200497, 0.615048),
       BL = c(-0.070578, -0.237213, 0.394071, 0.42002, 0.128739, 0.517165, -0.841454, -0.723644, 1.296514, 0.118961),
       MG = c(0.028676, 0.15576, -0.023857, 0.266352, 0.169415, -0.040361, -0.727401, -0.039122, 1.076806, 0.131313),
       SSL = c(0.04684, -0.213099, 0.460359, 0.320583, 0.34818, -0.37981, 0.065975, 0.426244, -0.058096, -0.012821))

# prepare final submission
for (group in groups){
  
  # combine
  s.all[[group]] <- merge(s.all[[group]], p.bag[[group]], all.x=T, all.y=F, sort=F)
  s.all[[group]] <- merge(s.all[[group]], p.gbm[[group]], all.x=T, all.y=F, sort=F)
  s.all[[group]] <- merge(s.all[[group]], p.rf[[group]], all.x=T, all.y=F, sort=F)
  
  # create median of blends
  s.all[[group]]$median <- apply(s.all[[group]][,-1], 1, median)
  cols <- ncol(s.all[[group]])
  
  # replace extreme values... can't get apply to work so loop it
  for (i in 1:nrow(s.all[[group]])){
    if(s.all[[group]][i, cols] > obs.orig[['max']][[group]]) s.all[[group]][i, cols] <- min(s.all[[group]][i, -1])
    if(s.all[[group]][i, cols] < obs.orig[['min']][[group]]) s.all[[group]][i, cols] <- max(s.all[[group]][i, -1])
    for (j in 2:(cols-1)){
      if(s.all[[group]][i,j] > obs.orig[['max']][[group]] | s.all[[group]][i,j] < obs.orig[['min']][[group]])
        s.all[[group]][i,j] <- s.all[[group]][i,cols]
    }    
  }  
  
  # weight
  s.all[[group]]$sub <- round(exp(as.vector(as.matrix(s.all[[group]][,2:cols]) %*% matrix(weights[[group]], cols-1, 1))))
  write.table(s.all[[group]][, c("SalesID", "sub")], file=paste0(SUBMISSION_PATH,"sub.csv"), row.names=F, col.names=F, append=T, sep=",")  
}

rm(cols, group, i, j, list=ls(pattern='type'))
save(s.all, obs, weights, file=paste0(SUBMISSION_PATH, 'sub_all.rda'))

# check submission
pdf(paste0(SUBMISSION_PATH, 'hist.pdf'))
par(mfrow=c(3,4))
for (group in groups){  
  print(cbind(summary(exp(obs.orig.SalePrice[[group]])), summary(s.all[[group]]$sub), summary(exp(s.all[[group]][,2:ncol(s.all[[group]])]))))
#   sapply(2:(ncol(s.all[[group]])-1), function(x) hist(exp(s.all[[group]][,x]), main=eval(paste(group, names(s.all[[group]])[x]))))
  hist(exp(obs.orig.SalePrice[[group]]), main=eval(paste(group, "OBSERVED")))
  hist(s.all[[group]]$sub, main=eval(paste(group, 'SUBMISSION')))
}
dev.off()

rm(list=ls(pattern="group"), seed)

groups <- names(s.all)
for (group in groups){
  s.all[[group]]$median2 <- round(exp(s.all[[group]]$median))
  write.table(s.all[[group]][, c("SalesID", "median2")], file=paste0(SUBMISSION_PATH,"med.csv"), row.names=F, col.names=F, append=T, sep=",")  
}