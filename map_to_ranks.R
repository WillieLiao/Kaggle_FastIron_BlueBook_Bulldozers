require(plyr)
load(paste0(TRAIN_DATA_PATH, "ttm-imputed-orig.rda"))

ttm2 <- vector("list")
for (group in groups){
  ds <- ttm[[group]]  
  ds <- redo_MKey(ds)
  cat(group, 'orig MKey', length(unique(ds$MKey)), 'MBase', length(unique(ds$MBase)), '\n')
  
  # split MBase
  base <- vector(mode="character",length=5*nrow(ds))
  base <- unlist(sapply(ds$MBase, function(s) split_base(s)))
  base <- data.frame(matrix(base, nrow(ds), 5, byrow=T))
  names(base) <- c('pre','thou','hund','tens','ones')
  ds <- data.frame(ds, base)
  rm(base)
  
  # split series
  series <- vector(mode="character",length=2*nrow(ds))
  series <- unlist(sapply(ds$MSeries, function(s) split_series(s)))
  series <- data.frame(matrix(series, nrow(ds), 2, byrow=T))
  names(series) <- c('ser','ser2')
  ds <- data.frame(ds, series)
  rm(series)
  
  # prepare key of unique levels for ranking
  feats <- c("MfgID", "MKey", "pre", "thou", "hund", "tens", "ones", "MSecond", "ser", "ser2", "MDesc")
  key <- ddply(ds, names(ds)[names(ds) %in% feats], nrow)
  f <- which(sapply(key, is.factor))
  key[,f] <- sapply(key[,f], as.character)
  key[,f][is.na(key[,f])] <- ""
  
  # order by mfgs from small to big
  # select fields to replace by their rank
  mfgs <- ddply(key, .(MfgID), summarize, rows=sum(V1))
  mfgs <- mfgs[order(mfgs$rows),]
  # key <- join(key, mfgs.sums, type="left", match="first")
  mfgsID <- mfgs$MfgID
  names(key)[ncol(key)] <- "rows"  
  f <- c("thou", "hund", "tens", "ones", "ser")
  key[,f] <- sapply(key[,f], as.numeric)
  key[,f][is.na(key[,f])] <- 0
  # sapply(key, function(x) sum(is.na(x)))
  cols <-  names(key)[3:(ncol(key)-1)]
  
  # merge together levels less than lim; save a copy before modifying it
  lim <- 20
  key.labels <- key
  
  for (mfg in mfgsID){
    key.mfg <- key[key$MfgID==mfg, c(cols, "rows")]
    for(col in cols){
      d <- ddply(key.mfg, col, summarize, rows=sum(rows))
      o <- 0
      for (i in seq_len(nrow(d))){
        if (d$rows[i]>=lim){
          o <- o + 1
          d$rows[i] <- o
        } else {
          d$rows[i] <- o + .5
        }
      }
      key.mfg <- join(key.mfg, d, by=col, match="first")      
      names(key.mfg)[ncol(key.mfg)] <- paste0("o_",col)
    }
    key[key$MfgID==mfg, cols] <- key.mfg[,paste0("o_",cols)]    
  }
  
  rm(mfg,mfgs,mfgsID,key.mfg,d,f,o,i,col,lim)
  key[,cols] <- sapply(key[,cols], as.numeric)
  save(key, key.labels, file=paste0(TRAIN_DATA_PATH, "key-rank-", group, ".rda"))
  
  # replace original columns with the new columns
  ds <- ds[, !(names(ds) %in% cols)]
  ds <- merge(ds, key[, !(names(key) %in% 'rows')], all.x=T, all.y=F, sort=F)
  ds$ser2 <- as.numeric(ds$ser2)
  
  # join together ser and ser2  
  pad.ser <- ifelse(length(unique(trunc(ds$ser2)))>9, 2, 1)
  ds$ser <- factor(paste0(trunc(ds$ser-1), sprintf(paste0("%0", pad.ser, "d"),trunc(ds$ser2-1))), ordered=T)
  
  # join together thou, hund, tens, ones -> MBase
  pad.thou <- ifelse(length(unique(trunc(ds$thou)))>9, 2, 1)
  pad.ones <- ifelse(length(unique(trunc(ds$ones)))>9, 2, 1)
  ds$MBase <- factor(paste0(
    sprintf(paste0("%0", pad.thou, "d"),trunc(ds$thou-1)),
    trunc(ds$hund-1),
    trunc(ds$tens-1),
    sprintf(paste0("%0", pad.ones, "d"),trunc(ds$ones-1))
  ), ordered=T)
  
  # join all together (pre, MBase, MSecondond, series, MDesc) for MKey
  pad.pre <- ifelse(length(unique(trunc(ds$pre)))>9, 2, 1)
  pad.MSecond <- ifelse(length(unique(trunc(ds$MSecond)))>9, 2, 1)
  if (!is.null(ds$MDesc)) pad.MDesc <- ifelse(length(unique(trunc(ds$MDesc)))>9, 2, 1)
  ds$MKey <- factor(paste0(
    sprintf(paste0("%0", pad.pre, "d"),trunc(ds$pre-1)),
    as.character(ds$MBase),
    sprintf(paste0("%0", pad.MSecond, "d"),trunc(ds$MSecond-1)),
    as.character(ds$ser),
    if (!is.null(ds$MDesc)) sprintf(paste0("%0", pad.MDesc, "d"),trunc(ds$MDesc-1))    
  ), ordered=T)
  
  # delete some columns
  cat(group, 'new MKey', length(unique(ds$MKey)), 'MBase', length(unique(ds$MBase)), '\n')
  rm(list=ls(pattern="pad."))
  ds <- ds[, !(names(ds) %in% c("thou", "hund", "tens", "ones", "ser2", "MSeries"))]  
  ttm2[[group]] <- ds  
  rm(ds, key, key.labels, cols)
}

names(ttm2) <- groups
ttm <- ttm2

save(ttm, groups, file=paste0(TRAIN_DATA_PATH, "ttm-imputed-", data_type, ".rda"))
rm(ttm, ttm2)