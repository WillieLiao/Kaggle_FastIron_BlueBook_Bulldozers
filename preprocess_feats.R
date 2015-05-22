require(plyr)
# load(paste0(TRAIN_DATA_PATH, "merged.rda"))

# ##########################################################
# # Change Type
# ##########################################################
# ordered factors:  UseBand, Size, GrouserType
# sapply(merged, class)
merged <- merged[, !(names(merged) %in% c('ModelID','UnderW','BladeW','StickL','Hours'))]

mfgs <- count(merged, "MfgID")
mfgs.small <- mfgs$MfgID[mfgs$freq<=15]
merged$MfgID[merged$MfgID %in% mfgs.small] <- 0
merged$MfgID <- factor(merged$MfgID)
merged$UseBand <- factor(merged$UseBand, levels=c("Low", "Medium", "High"), ordered=T)
merged$Size <- factor(merged$Size, levels=c("Mini", "Compact", "Small", "Medium", "Large / Medium", "Large"), ordered=T)
merged$GrouserType <- factor(merged$GrouserType, levels=c("Double", "Triple"), ordered=T)
merged$Month <- factor(merged$Month, ordered=T)

# factors unordered
MakeFactors <- c("Source", "AucID", names(which(sapply(merged, class)=="character")))
names.merged <- names(merged)
for (i in 1:ncol(merged)){
  if (names.merged[i] %in% MakeFactors) merged[,i] <- factor(merged[,i])
}
rm(i, MakeFactors, names.merged, mfgs, mfgs.small)
sapply(merged, class)

# if we have not seen the ProdGp before, treat it as an outlier
ProdGps <- c('BL', 'MG', 'SSL', 'TEX', 'TTT', 'WL')
outlier.unk.ProdGp <- merged[!(merged$ProdGp %in% ProdGps), ]

# overall manufacturer features
load(paste0(TRAIN_DATA_PATH, "mac-after-impute-manual.rda"))
mfg <- ddply(mac, .(MfgID), summarize, market=length(MfgID))
mfg2 <- ddply(mac, .(MfgID, MBase), summarize, models=length(MBase))
mfg2 <- ddply(mfg2, .(MfgID), summarize, models=length(MfgID))
rm(mac)

mfg$market <- (sqrt(mfg$market)) / sum(sqrt(mfg$market))
mfg$models <- sqrt(mfg2$models)
mfg <- rbind(mfg, c("0", sort(mfg$market)[10], 1))
merged <- join(merged, mfg, match='first', by="MfgID", type="left")
merged[, c("market","models")] <- sapply(merged[, c("market","models")], as.numeric)
rm(mfg, mfg2)

save(merged, file=paste0(TRAIN_DATA_PATH, "merged-after-mfgfeats.rda"))
