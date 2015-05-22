##########################################################
# create datasets
##########################################################
load(paste0(TRAIN_DATA_PATH, 'ttm-imputed-', data_type, '.rda'))
ProdGps <- c('TTT', 'WL', 'TEX', 'BL', 'MG', 'SSL')
ProdGp.Mfg <- c('TTT', 'WL.big', 'TEX.big', 'BL', 'MG', 'SSL', 'TEX', 'WL')

all.ds <- vector("list")  
for (ProdGp in ProdGps){    
  ds <- ttm[[ProdGp]]  
  idx.tr <- !is.na(ds$SalePrice)
  idx.ts <- is.na(ds$SalePrice)
  ds <- droplevels(subset(ds, idx.tr | idx.ts))
  idx.tr <- !is.na(ds$SalePrice)
  idx.ts <- is.na(ds$SalePrice)
  
  tr <- subset(ds, idx.tr, select=-SalePrice)
  ts <- subset(ds, idx.ts, select=-SalePrice)
  y_tr <- ds$SalePrice[idx.tr]
  y_ts <- rep(0, sum(idx.ts))      
  row.names(tr) <- tr$SalesID
  row.names(ts) <- ts$SalesID
  names(y_tr) <- tr$SalesID
  names(y_ts) <- ts$SalesID
  
  rm(ds)
  
  all.ds[[ProdGp]][["tr"]] <- tr
  all.ds[[ProdGp]][["ts"]] <- ts
  all.ds[[ProdGp]][["y_tr"]] <- y_tr
  all.ds[[ProdGp]][["y_ts"]] <- y_ts
  gc()
}

groups <- ProdGps
save(all.ds, groups, file=paste0(TRAIN_DATA_PATH, '-', data_type, "6.rda"))  
rm(tr, ts, y_tr, y_ts, idx.tr, idx.ts, groups, ProdGp, ProdGps, ProdGp.Mfg)
rm(ttm)

# splitting into 8 groups
# for (ProdGp in ProdGps){    
#   ds <- ttm[[ProdGp]]  
# 
#   if (ProdGp=="TEX"){
#     # c(26,43,74,103,99,25)
#     idx.tr <- !is.na(ds$SalePrice) & (ds$MfgID %in% c(26,43,74,103,99,25))
#     idx.ts <-  is.na(ds$SalePrice) & (ds$MfgID %in% c(26,43,74,103,99,25))
#     ds.small <- droplevels(subset(ds, !idx.tr & !idx.ts))
#     ds <- droplevels(subset(ds, idx.tr | idx.ts))      
#     idx.tr <- !is.na(ds$SalePrice) & (ds$MfgID %in% c(26,43,74,103,99,25))
#     idx.ts <-  is.na(ds$SalePrice) & (ds$MfgID %in% c(26,43,74,103,99,25))
#     
#     tr <- subset(ds, idx.tr, select=-SalePrice)
#     ts <- subset(ds, idx.ts, select=-SalePrice)
#     y_tr <- ds$SalePrice[idx.tr]
#     y_ts <- rep(0, sum(idx.ts))    
#     row.names(tr) <- tr$SalesID
#     row.names(ts) <- ts$SalesID
#     names(y_tr) <- tr$SalesID
#     names(y_ts) <- ts$SalesID
#     
#     all.ds[['TEX.big']][["tr"]] <- tr
#     all.ds[['TEX.big']][["ts"]] <- ts
#     all.ds[['TEX.big']][["y_tr"]] <- y_tr
#     all.ds[['TEX.big']][["y_ts"]] <- y_ts      
#     ds <- ds.small; rm(ds.small)
#   }
#   else if (ProdGp=="WL"){
#     idx.tr <- !is.na(ds$SalePrice) & (ds$MfgID %in% c(26,43,103,25))
#     idx.ts <-  is.na(ds$SalePrice) & (ds$MfgID %in% c(26,43,103,25))
#     ds.small <- droplevels(subset(ds, !idx.tr & !idx.ts))
#     ds <- droplevels(subset(ds, idx.tr | idx.ts))      
#     idx.tr <- !is.na(ds$SalePrice) & (ds$MfgID %in% c(26,43,103,25))
#     idx.ts <-  is.na(ds$SalePrice) & (ds$MfgID %in% c(26,43,103,25))
#     
#     tr <- subset(ds, idx.tr, select=-SalePrice)
#     ts <- subset(ds, idx.ts, select=-SalePrice)
#     y_tr <- ds$SalePrice[idx.tr]
#     y_ts <- rep(0, sum(idx.ts))      
#     row.names(tr) <- tr$SalesID
#     row.names(ts) <- ts$SalesID
#     names(y_tr) <- tr$SalesID
#     names(y_ts) <- ts$SalesID
#     
#     all.ds[['WL.big']][["tr"]] <- tr
#     all.ds[['WL.big']][["ts"]] <- ts
#     all.ds[['WL.big']][["y_tr"]] <- y_tr
#     all.ds[['WL.big']][["y_ts"]] <- y_ts      
#     ds <- ds.small; rm(ds.small)
#   }
#   
#   idx.tr <- !is.na(ds$SalePrice)
#   idx.ts <- is.na(ds$SalePrice)
#   ds <- droplevels(subset(ds, idx.tr | idx.ts))
#   idx.tr <- !is.na(ds$SalePrice)
#   idx.ts <- is.na(ds$SalePrice)
#   
#   tr <- subset(ds, idx.tr, select=-SalePrice)
#   ts <- subset(ds, idx.ts, select=-SalePrice)
#   y_tr <- ds$SalePrice[idx.tr]
#   y_ts <- rep(0, sum(idx.ts))      
#   row.names(tr) <- tr$SalesID
#   row.names(ts) <- ts$SalesID
#   names(y_tr) <- tr$SalesID
#   names(y_ts) <- ts$SalesID
#   
#   rm(ds)
#   
#   all.ds[[ProdGp]][["tr"]] <- tr
#   all.ds[[ProdGp]][["ts"]] <- ts
#   all.ds[[ProdGp]][["y_tr"]] <- y_tr
#   all.ds[[ProdGp]][["y_ts"]] <- y_ts
#   gc()
# }
# 
# groups <- ProdGp.Mfg
# all.ds <- all.ds[ProdGp.Mfg]
# save(all.ds, groups, file=paste0(TRAIN_DATA_PATH, '-', data_type, "8.rda"))  
