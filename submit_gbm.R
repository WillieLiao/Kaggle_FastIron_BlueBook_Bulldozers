require(gbm)
seeds <- 201301:201304
data_types <- c('orig6', 'orig6', 'rank6', 'rank6')
print.model <- T
plot.model <- T
shrinks <- c(.003, .003, .005, .005)
bagfraction <- .8
depths <- list(c(15,8,7,10,20,5),
               c(12,10,7,10,14,6),
               c(12,10,10,12,17,5),
               c(9,7,7,5,14,5))
ntrees <- list(c(8750,6000,3500,4750,4250,2500),
               c(12250,7250,5250,9000,8750,4200),
               c(9200,4500,16250,9100,4400,5000),
               c(15500,11500,15000,15500,5500,9250))
feats.name <- c('base','many','base','many')
feats <- list(feats.orig, feats.orig2, feats.orig, feats.orig2)

# library(doParallel)
# cl<-makeCluster(4)
# registerDoParallel(cl)

for (i in 1:4){    
  load(paste0(TRAIN_DATA_PATH, "-", data_types[i], ".rda"))
  preds.gbm <- vector("list")
  preds.gbm <- foreach(group=groups, ntree=ntrees[[i]], depth=depths[[i]], shrink=rep(shrinks[i], length(groups)), ds=all.ds, .packages="gbm") %dopar% {
    sink(paste0("gbm-", feats.name[i], "bf", bagfraction, "-", data_types[i], ".txt"), append=T)  
    set.seed(seeds[i]+shrink*ntree)    
    tr <- all.ds[[group]][["tr"]]
    ts <- all.ds[[group]][["ts"]]
    y_tr <- all.ds[[group]][["y_tr"]]  
    
    fit <- gbm.fit(tr[, names(tr) %in% feats[[i]]], y_tr, distribution="gaussian", keep.data=F, shrinkage=shrink, interaction.depth=depth, n.trees=ntree, bag.fraction=bagfraction, verbose=F)
    p <- predict(fit, ts[, names(ts) %in% feats[[i]]], n.trees=ntree)
    names(p) <- names(ts)
    if (print.model) print(summary(fit, plot=F)) 
    rm(fit, tr, ts, y_tr); gc() 
    p
  }  
  save (preds.gbm, file=paste0(SUBMISSION_PATH, "gbm-", data_types[i], feats.name[i], ".rda"))
}
