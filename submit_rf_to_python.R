data_types <- c("orig","rank")

for (data_type in data_types){
  ds.type <- paste0('-', data_type, 6)
  load(paste0(TRAIN_DATA_PATH, ds.type, ".rda"))

  for (group in groups){
    tr <- all.ds[[group]][['tr']]
    ts <- all.ds[[group]][['ts']]
    
    # not sure why pandas isn't grouping all '' together as NAN.
    if (!is.null(tr$MSeries) & data_type=='rank') tr$MSeries <- paste0('series', as.character(tr$MSeries))
    if (!is.null(ts$MSeries) & data_type=='rank') ts$MSeries <- paste0('series', as.character(ts$MSeries))    
    if (!is.null(tr$MDesc) & data_type=='rank') tr$MDesc <- paste0('desc', as.character(tr$MDesc))
    if (!is.null(ts$MDesc) & data_type=='rank') ts$MDesc <- paste0('desc', as.character(ts$MDesc))
    
    write.csv(cbind(SalePrice=all.ds[[group]][['y_tr']],
                    tr[, names(tr) %in% c(feats.imp, feats.imp2)]),
              file=paste0(TRAIN_DATA_PATH, 'tr-', group, ds.type, '.csv'), row.names=F)
    write.csv(cbind(SalePrice=all.ds[[group]][['y_ts']],
                    ts[, names(ts) %in% c(feats.imp, feats.imp2)]),
              file=paste0(TRAIN_DATA_PATH, 'ts-', group, ds.type, '.csv'), row.names=F)  
  }
}
rm(ds.type, data_types, all.ds, tr, ts); gc()