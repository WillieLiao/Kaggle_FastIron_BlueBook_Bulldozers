require(ggplot2)
require(plyr)

##########################################################
# merge SalePrice and tt
##########################################################
# load(paste0(TRAIN_DATA_PATH, "mac-after-impute-manual.rda"))
# load(paste0(TRAIN_DATA_PATH, "tt-after-impute-manual.rda"))
load(paste0(TRAIN_DATA_PATH, "SalePrice.rda"))
tt <- merge(SalePrice, tt, all.x=F, all.y=T, sort=F)
rm(SalePrice)

##########################################################
# merge train and mac
##########################################################
# Get a feel for unmatched values.
# Fix major, possinbly recurring, data entry errors but leave most of it alone.  Simulates new unknown data set.
# unmatched.MKey <- sort(unique(tt$MKey[!(tt$MKey %in% mac$MKey)]))
# unmatched.MBase <- sort(unique(tt$MBase[!(tt$MBase %in% mac$MBase)]))
# unmatched.MSecond <- sort(unique(tt$MSecond[!(tt$MSecond %in% mac$MSecond)]))
# unmatched.MSeries <- sort(unique(tt$MSeries[!(tt$MSeries %in% mac$MSeries)]))
# unmatched.ModelID <- sort(unique(tt$ModelID[!(tt$ModelID %in% mac$ModelID)]))
# rm(list=ls(pattern="unmatched."))

# Use hill climb to match.
# TODO: Wrap in functions
# First attempt uses all similar fields.
matched <- merge(tt, mac, all.x=F, all.y=F, sort=F, suffix=c(".tt", ".mac"),
                 by=c("MacID", "MKey", "MBase", "MSecond", "MSeries", "MDesc", "ModelID", "ProdGp", "YearMade"))
length(unique(matched$SalesID)); dim(matched)
merged <- matched
unmatched <- tt[!(tt$SalesID %in% merged$SalesID),] #48395
unmatched.all <- unmatched$SalesID

# Relax matching on YearMade.
matched <- merge(unmatched, mac, all.x=F, all.y=F, sort=F, suffix=c(".tt", ".mac"),
                 by=c("MacID", "MKey", "MBase", "MSecond", "MSeries", "MDesc", "ModelID", "ProdGp"))
length(unique(matched$SalesID)); dim(matched) 
# qplot(YearMade.tt, YearMade.mac, data=matched, alpha=I(1/2), geom="jitter")
# qplot(SaleDate, YearMade.tt, data=matched, alpha=I(1/2), geom="jitter")
# qplot(SaleDate, YearMade.mac, data=matched, alpha=I(1/2), geom="jitter")

# If one dataset is saying made > sale but the other isn't, use the other instead.
YearMade.bad.tt_good.mac <- which((matched$Year < matched$YearMade.tt-1) & (matched$Year > matched$YearMade.mac-1)
                                  & !is.na(matched$Year) & !is.na(matched$YearMade.tt) & !is.na(matched$YearMade.mac))
YearMade.bad.mac_good.tt <- which((matched$Year < matched$YearMade.mac-1) & (matched$Year > matched$YearMade.tt-1)
                                  & !is.na(matched$Year) & !is.na(matched$YearMade.tt) & !is.na(matched$YearMade.mac))
if (length(YearMade.bad.tt_good.mac) > 0)
  matched$YearMade.tt[YearMade.bad.tt_good.mac] <- matched$YearMade.mac[YearMade.bad.tt_good.mac]
if (length(YearMade.bad.mac_good.tt) > 0)
  matched$YearMade.mac[YearMade.bad.mac_good.tt] <- matched$YearMade.tt[YearMade.bad.mac_good.tt]

# Average the two different YearMade (or just use one if the other is NA).
# names(matched)
# sum(is.na(matched$YearMade.tt))
# sum(is.na(matched$YearMade.mac))
# sum(!is.na(matched$YearMade.tt) & matched$YearMade.tt > matched$Year+1)
# sum(!is.na(matched$YearMade.mac) & matched$YearMade.mac > matched$Year+1)
# qplot(SaleDate, YearMade.avg, data=matched, alpha=I(1/2), geom="jitter")
# qplot(SaleDate, YearMade.max, data=matched, alpha=I(1/2), geom="jitter")
# colYearMades <-  grep("(Sale|YearMade)", names(matched))
# matched[matched$YearMade.avg>matched$Year+1, colYearMades]
matched$YearMade <- apply(cbind(matched$YearMade.tt, matched$YearMade.mac), 1, function(x) mean(x, na.rm=T))
rowYearMade.bad <- which(matched$YearMade > matched$Year+1)
matched$YearMade[rowYearMade.bad] <- apply(cbind(matched$YearMade.med.tt[rowYearMade.bad], matched$YearMade.med.mac[rowYearMade.bad]), 1, function(x) mean(x, na.rm=T))
# qplot(SaleDate, YearMade, data=matched, alpha=I(1/2), geom="jitter")

merged <- rbind(merged, matched[, names(merged)])
unmatched <- tt[!(tt$SalesID %in% merged$SalesID),] #24538
dim(unmatched)

# Relax matching on ModelID.
matched <- merge(unmatched, mac, all.x=F, all.y=F, sort=F, suffix=c(".tt", ".mac"),
                 by=c("MacID", "MKey", "MBase", "MSecond", "MSeries", "MDesc", "ProdGp", "YearMade"))
length(unique(matched$SalesID)); dim(matched) 
matched$ModelID <- matched$ModelID.tt
merged <- rbind(merged, subset(matched, select=-c(ModelID.tt, ModelID.mac)))
unmatched <- tt[!(tt$SalesID %in% merged$SalesID),] #24161
dim(unmatched)

# Relax matching on ModelID & YearMade. Average the two different YearMade.
matched <- merge(unmatched, mac, all.x=F, all.y=F, sort=F, suffix=c(".tt", ".mac"),
                 by=c("MacID", "MKey", "MBase", "MSecond", "MSeries", "MDesc", "ProdGp"))
length(unique(matched$SalesID)); dim(matched) 
# If one dataset is saying made > sale but the other isn't, use the other instead.
YearMade.bad.tt_good.mac <- which((matched$Year < matched$YearMade.tt-1) & (matched$Year > matched$YearMade.mac-1)
                                  & !is.na(matched$Year) & !is.na(matched$YearMade.tt) & !is.na(matched$YearMade.mac))
YearMade.bad.mac_good.tt <- which((matched$Year < matched$YearMade.mac-1) & (matched$Year > matched$YearMade.tt-1)
                                  & !is.na(matched$Year) & !is.na(matched$YearMade.tt) & !is.na(matched$YearMade.mac))
if (length(YearMade.bad.tt_good.mac) > 0)
  matched$YearMade.tt[YearMade.bad.tt_good.mac] <- matched$YearMade.mac[YearMade.bad.tt_good.mac]
if (length(YearMade.bad.mac_good.tt) > 0)
  matched$YearMade.mac[YearMade.bad.mac_good.tt] <- matched$YearMade.tt[YearMade.bad.mac_good.tt]
matched$YearMade <- apply(cbind(matched$YearMade.tt, matched$YearMade.mac), 1, function(x) mean(x, na.rm=T))
matched$ModelID <- matched$ModelID.tt
merged <- rbind(merged, subset(matched, select=-c(ModelID.tt, ModelID.mac, YearMade.tt, YearMade.mac)))
unmatched <- tt[!(tt$SalesID %in% merged$SalesID),] #24074
dim(unmatched)

# Relax matching on secondary model characteristics.
matched <- merge(unmatched, mac, all.x=F, all.y=F, sort=F, suffix=c(".tt", ".mac"),
                 by=c("MacID", "MBase", "ModelID", "ProdGp", "YearMade"))
length(unique(matched$SalesID)); dim(matched) 
# names(matched)

names(matched)[which(names(matched) %in% paste0(c("MKey", "MSecond", "MSeries", "MDesc"), ".tt"))] <- c("MKey", "MSecond", "MSeries", "MDesc")
merged <- rbind(merged, subset(matched, select=-c(MKey.mac, MSecond.mac, MSeries.mac, MDesc.mac)))
unmatched <- tt[!(tt$SalesID %in% merged$SalesID),] # 23919
dim(unmatched)

# Relax matching on secondary model characteristics and YearMade.
matched <- merge(unmatched, mac, all.x=F, all.y=F, sort=F, suffix=c(".tt", ".mac"),
                 by=c("MacID", "MBase", "ModelID", "ProdGp"))
length(unique(matched$SalesID)); dim(matched)
# qplot(YearMade.tt, YearMade.mac, data=matched, alpha=I(1/2), geom="jitter")
# If one dataset is saying made > sale but the other isn't, use the other instead.
YearMade.bad.tt_good.mac <- which((matched$Year < matched$YearMade.tt-1) & (matched$Year > matched$YearMade.mac-1)
                                  & !is.na(matched$Year) & !is.na(matched$YearMade.tt) & !is.na(matched$YearMade.mac))
YearMade.bad.mac_good.tt <- which((matched$Year < matched$YearMade.mac-1) & (matched$Year > matched$YearMade.tt-1)
                                  & !is.na(matched$Year) & !is.na(matched$YearMade.tt) & !is.na(matched$YearMade.mac))
if (length(YearMade.bad.tt_good.mac) > 0)
  matched$YearMade.tt[YearMade.bad.tt_good.mac] <- matched$YearMade.mac[YearMade.bad.tt_good.mac]
if (length(YearMade.bad.mac_good.tt) > 0)
  matched$YearMade.mac[YearMade.bad.mac_good.tt] <- matched$YearMade.tt[YearMade.bad.mac_good.tt]
matched$YearMade.tt <- apply(cbind(matched$YearMade.tt, matched$YearMade.mac), 1, function(x) mean(x, na.rm=T))
names(matched)[which(names(matched) %in% paste0(c("YearMade", "MKey", "MSecond", "MSeries", "MDesc"), ".tt"))] <- c("YearMade", "MKey", "MSecond", "MSeries", "MDesc")
merged <- rbind(merged, matched[, names(merged)])
unmatched <- tt[!(tt$SalesID %in% merged$SalesID),] # 23910
dim(unmatched)
unmatched.ModelID <- unmatched$SalesID

# Relax matching on MacID.  Seems like there are lot of bad MacID.
matched <- merge(unmatched, mac, all.x=F, all.y=F, sort=F, suffix=c(".tt", ".mac"),
                 by=c("MKey", "MBase", "MSecond", "MSeries", "MDesc", "ModelID", "ProdGp", "YearMade"))
length(unique(matched$SalesID)); dim(matched)  ; gc()
matched$MacID.mac<-c()
names(matched)[names(matched)=="MacID.tt"] <- "MacID"
# a lot of duplicated MacID, remove that column and remove the duplicates
matched <- matched[!duplicated(matched),]
matched <- matched[do.call(order, list(matched$SalesID, matched$MacID, matched$MfgID)),]
dup <- matched$SalesID[duplicated(matched$SalesID)]
# matched[matched$SalesID %in% dup,]
length(unique(matched$SalesID)); dim(matched)  ; gc()
matched <- matched[!duplicated(matched[,1:56]),]
length(unique(matched$SalesID)); dim(matched)  ; gc()
merged <- rbind(merged, matched)
unmatched <- tt[!(tt$SalesID %in% merged$SalesID),] # 4358
dim(unmatched)
unmatched.MacID <- unmatched$SalesID

# Relax matching on MacID and YearMade.
# Lot of different mac$YearMade.  Use small differences and try matching again.
for (ydiff in c(-1, -2, 1, 2, -3, 3)){
  unmatched$YearMade <- unmatched$YearMade - ydiff
  matched <- merge(unmatched, mac, all.x=F, all.y=F, sort=F, suffix=c(".tt", ".mac"),
                   by=c("MKey", "MBase", "MSecond", "MSeries", "MDesc", "ModelID", "ProdGp", "YearMade"))  
  names(matched)[names(matched)=="MacID.tt"] <- "MacID"
  matched$MacID.mac <- c()
  matched <- matched[!duplicated(matched[, 1:56]),]
  merged <- rbind(merged, matched[, names(merged)])
  unmatched <- tt[!(tt$SalesID %in% merged$SalesID),]   # 1318
  gc()
}
rm(ydiff)

# Only use MBase & ProdGp & YearMade.
matched <- merge(unmatched, mac, all.x=F, all.y=F, sort=F, suffix=c("", ".mac"),
                 by=c("MBase", "ProdGp", "YearMade"))
length(unique(matched$SalesID)); dim(matched) 
names(matched)[names(matched)=="YearMade.med"] <- "YearMade.med.tt"
names(matched)[names(matched)=="YearMade.min"] <- "YearMade.min.tt"
matched$MacID.mac <- c()
matched <- matched[!duplicated(matched[, 1:56]),]
length(unique(matched$SalesID)); dim(matched) 
merged <- rbind(merged, matched[, names(merged)])
unmatched <- tt[!(tt$SalesID %in% merged$SalesID),] # 196
dim(unmatched)
unmatched.MBase <- unmatched$SalesID

# Relax matching on MacID and YearMade.
matched <- merge(unmatched, mac, all.x=F, all.y=F, sort=F, suffix=c(".tt", ".mac"),
                 by=c("MKey", "MBase", "MSecond", "MSeries", "MDesc", "ModelID", "ProdGp"))
length(unique(matched$SalesID)); dim(matched) 
names(matched)[names(matched)=="MacID.tt"] <- "MacID"
matched$MacID.mac <- c()

rowYearMade.bad <- which(matched$YearMade.mac > matched$Year+1)
matched <- matched[-rowYearMade.bad,]
rowYearMade.bad <- which(matched$YearMade.tt > matched$Year+1)
matched <- matched[-rowYearMade.bad,]

colYearMades <-  grep("(Sale|YearMade)", names(matched))

# if no NA exists, sort and use the closest date in mac
matched.no.na <- matched[!is.na(matched$YearMade.tt) & !is.na(matched$YearMade.mac), ]

# sort by the absolute difference
matched.no.na$absdiff <- abs(matched.no.na$YearMade.tt-matched.no.na$YearMade.mac)
matched.no.na$diff <- matched.no.na$YearMade.tt-matched.no.na$YearMade.mac
matched.no.na <- matched.no.na[do.call(order, list(matched.no.na$SalesID, matched.no.na$absdiff, matched.no.na$diff, matched.no.na$YearMade.mac)),]
matched.no.na <- matched.no.na[!duplicated(matched.no.na[,1:56]),]
length(unique(matched.no.na$SalesID))
names(matched.no.na)[names(matched.no.na)=="YearMade.tt"] <- "YearMade"
merged <- rbind(merged, matched.no.na[, names(merged)]); gc()

# Sort won't work on NA so use median as backoff.
matched <- matched[!(matched$SalesID %in% merged$SalesID),]
matched$YearMade.mac <- matched$YearMade.med.mac
matched$YearMade.tt <- matched$YearMade.med.tt
matched$absdiff <- abs(matched$YearMade.tt-matched$YearMade.mac)
matched$diff <- matched$YearMade.tt-matched$YearMade.mac
matched <- matched[do.call(order, list(matched$SalesID, matched$absdiff, matched$diff, matched$YearMade.mac)),]
matched$YearMade <- floor(mean(c(matched$YearMade.tt, matched$YearMade.mac), na.rm=T))
matched <- matched[!duplicated(matched[, 1:56]),]
length(unique(matched$SalesID)); dim(matched) 
merged <- rbind(merged, matched[, names(merged)])
unmatched <- tt[!(tt$SalesID %in% merged$SalesID),] # 73
dim(unmatched)

# Only use MBase & ProdGp
matched <- merge(unmatched, mac, all.x=F, all.y=F, sort=F, suffix=c(".tt", ".mac"),
                 by=c("MBase", "ProdGp"))
length(unique(matched$SalesID)); dim(matched) 
colYearMades <-  grep("(Sale|YearMade)", names(matched))
# matched[, colYearMades]

matched$YearMade.tt <-  apply(cbind(matched$YearMade.med.tt, matched$YearMade.med.mac), 1, function(x) mean(x, na.rm=T))
matched <- matched[!duplicated(matched[, 1:56]),]
matched <- matched[order(matched$YearMade.tt, decreasing=T),]
matched <- matched[!duplicated(matched$SalesID),]
names(matched)[c(5,6,9,13:16)] <- gsub('\\.tt', '', names(matched)[c(5,6,9,13:16)])
merged <- rbind(merged, matched[, names(merged)])
unmatched <- tt[!(tt$SalesID %in% merged$SalesID),] # 

# Only use MacID & ProdGp.  Probably a new model.
matched <- merge(unmatched, mac, all.x=F, all.y=F, sort=F, suffix=c("", ".mac"),
                 by=c("MacID", "ProdGp"))

matched$YearMade[is.na(matched$YearMade)] <- matched$YearMade.mac[is.na(matched$YearMade)]
names(matched)[names(matched)=="YearMade.med"] <- "YearMade.med.tt"
names(matched)[names(matched)=="YearMade.min"] <- "YearMade.min.tt"
merged <- rbind(merged, matched[, names(merged)])
unmatched <- tt[!(tt$SalesID %in% merged$SalesID),] # 7
dim(unmatched) 

# Finally: Only use MacID
matched <- merge(unmatched, mac, all.x=F, all.y=F, sort=F, suffix=c("", ".mac"),
                 by=c("MacID"))
matched$YearMade <-  apply(cbind(matched$YearMade.med, matched$YearMade.med.mac), 1, function(x) mean(x, na.rm=T))
names(matched)[names(matched)=="YearMade.med"] <- "YearMade.med.tt"
names(matched)[names(matched)=="YearMade.min"] <- "YearMade.min.tt"
merged <- rbind(merged, matched[, names(merged)])
unmatched <- tt[!(tt$SalesID %in% merged$SalesID),] # 0
dim(unmatched)

# Clean up any duplicates
merged <- merged[!duplicated(merged$SalesID), ]
rm(YearMade.bad.tt_good.mac, YearMade.bad.mac_good.tt, matched.no.na, dup, matched, unmatched, colYearMades, rowYearMade.bad)
rm(mac, tt)

##########################################################
# combine YearMade
##########################################################
merged$YearMade.min <- apply(cbind(merged$YearMade.min.tt, merged$YearMade.min.mac), 1, function(x) mean(x, na.rm=T))
merged$YearMade.med <- apply(cbind(merged$YearMade.med.tt, merged$YearMade.med.mac), 1, function(x) mean(x, na.rm=T))
merged <- subset(merged, select=-c(YearMade.min.tt, YearMade.med.tt, YearMade.min.mac, YearMade.med.mac))

# some minimum years probably picking up previous models
# p2 <- ggplot(ds, aes(x=s))
# p2 <- p2 + geom_point(aes(y = y-med, colour = "med"), position="jitter")
# p2 <- p2 + geom_point(aes(y = y-min, colour = "min"), position="jitter")
# qplot(y-min, data=ds[(ds$y-ds$min)>35 &!is.na(ds$y) & !is.na(ds$min),], geom="density")
bad.min <- which((merged$YearMade-merged$min)>40 &!is.na(merged$YearMade) & !is.na(merged$min))
merged$YearMade.min[bad.min] <- NA
rm(bad.min)

# replace YearMade with median if sold later than made (idx)
idx <- which((merged$YearMade > merged$Year) & !is.na(merged$YearMade) & !is.na(merged$YearMade.med))
merged$YearMade[idx] <- merged$YearMade.med[idx]

# library(ggplot2)
# ds<-merged[,grep("Year|SalePrice",names(merged))]
# names(ds) <- c('y','p', 's', 'min','med')
# sapply(ds, function(x) sum(is.na(x)))
# 
# qplot(y, p, data=ds, alpha=I(1/10), geom="jitter")
# qplot(med, p, data=ds, alpha=I(1/10), geom="jitter", color=is.na(y))
# qplot(min, p, data=ds, alpha=I(1/10), geom="jitter", color=is.na(y))

# qplot(s,y,data=ds, alpha=I(1/10), geom="jitter")
# qplot(s,med,data=ds, alpha=I(1/10), geom="jitter", color=is.na(y))
# qplot(s,min,data=ds, alpha=I(1/10), geom="jitter", color=is.na(y))
# qplot(y,med,data=ds, alpha=I(1/10), geom="jitter")
# missing<-row.names(ds)[which(is.na(ds$y))]

# if med > SaleYear, average med and min to get good YearMade prediction
# but first, see if the average will also be greater than YearMade (idx3).  If yes, just use min. (idx2)
# ds<-merged[,grep("Year|SalePrice",names(merged))]
# names(ds) <- c('y','p', 's', 'min','med')
# sapply(ds, function(x) sum(is.na(x)))
# ds2 <- ds[(ds$med > ds$s) & !is.na(ds$med) &!is.na(ds$y), ]
# p <- ggplot(ds2, aes(x = s))
# p <- p + geom_point(aes(y = y, colour = "y"), position="jitter")
# p <- p + geom_point(aes(y = min, colour = "min"), position="jitter") +  geom_point(aes(y = med, colour = "med"), position="jitter")
# p <- p + geom_point(aes(y = .5*min + .5*med, colour = "avg"), position="jitter")
# if avg(med,min) > SaleYear, use min to get good YearMade prediction
# ds3 <- ds2[(.5*ds2$min+.5*ds2$med > ds2$s) & !is.na(ds2$med) & !is.na(ds2$min) &!is.na(ds2$y), ]
# avg.idx <- which(merged$YearMade.med > merged$Year & !is.na(merged$YearMade.med) & !is.na(merged$Year) & is.na(merged$YearMade))
idx2 <- which(.5*merged$YearMade.med + .5*merged$YearMade.min > merged$Year  & !is.na(merged$YearMade.med) & !is.na(merged$YearMade.min) & is.na(merged$YearMade))
merged$YearMade[idx2] <- merged$YearMade.min[idx2]
idx3 <- which(merged$YearMade.med > merged$Year & !is.na(merged$YearMade.med)& is.na(merged$YearMade))
merged$YearMade[idx3] <- .5*merged$YearMade.med[idx3] + .5*merged$YearMade.min[idx3]

# finally, just use median if it doesn't become too old
idx4 <- which(!is.na(merged$YearMade.med) & is.na(merged$YearMade))
# qplot(Year, YearMade.med, data=merged[idx4,], geom="jitter")
# qplot(YearMade.med, YearMade.min, data=merged[idx4,], geom="jitter")
# qplot(Year-YearMade.med, log(SalePrice), data=merged[idx4,], geom="jitter")
idx4 <- which(!is.na(merged$YearMade.med) & is.na(merged$YearMade) & (merged$Year-merged$YearMade.med < 50))
merged$YearMade[idx4] <- merged$YearMade.med[idx4]

# and if there are still made > sale, set it to 10 years earlier.  Possible an earlier model.
idx5 <- which(merged$YearMade-1 > merged$Year & !is.na(merged$YearMade))
merged$YearMade[idx5] <- merged$YearMade[idx5] - 10

##########################################################
# create ages
##########################################################
merged$Age <- get_age(round(merged$YearMade), merged$SaleDate, 365)
merged$Age.med <- get_age(round(merged$YearMade.med), merged$SaleDate, 365)
merged$Age.min <- get_age(round(merged$YearMade.min), merged$SaleDate, 365)

# qplot(Age, SalePrice, data=merged, alpha=I(1/10), geom="jitter")
# qplot(YearMade, Age, data=merged, alpha=I(1/10), geom="jitter")
# qplot(YearMade.med.tt, Age, data=merged, alpha=I(1/10), geom="jitter")
# qplot(YearMade.med.mac, Age, data=merged, alpha=I(1/10), geom="jitter")
# make remaining negative ages 0
merged$Age[merged$Age<0 & !is.na(merged$Age)] <- 0
merged$Age.med[merged$Age.med <0 & !is.na(merged$Age.med)] <- 0
merged$Age.min[merged$Age.min <0 & !is.na(merged$Age.min)] <- 0

rm(list=ls(pattern="^(idx|ds)"), p, p2)


##########################################################
# Add in cpi columns
##########################################################
load(paste0(TRAIN_DATA_PATH, "cpi.rda"))
cpi.clean <- subset(cpi, select=-date)
names(cpi.clean) <- c("Year", "Month", "cpi_us", "cpi_trucks", "pc_agri", "pc_contr", "wp_agri", "wp_contr")
merged <- merge(merged, cpi.clean, all.x=T, all.y=F, sort=F)
rm(cpi, cpi.clean)

##########################################################
# perform transformations of SalePrice and Hours
##########################################################
colHours <- grep("Hours", names(merged))
colAges <-  grep("Age", names(merged))
colCPI <-  grep("_", names(merged))
merged$SalePrice <- log(merged$SalePrice)
merged[, colHours] <- sapply(merged[, colHours], function(x) sqrt(x/5/250))
merged[, colAges] <- sapply(merged[, colAges], sqrt)
merged[, colCPI] <- sapply(merged[, colCPI], log)
rm(colHours, colAges, colCPI)
row.names(merged) <- merged$SalesID
save(merged, file=paste0(TRAIN_DATA_PATH, "merged.rda"))
save(list=ls(pattern="unmatched."), file=paste0(TRAIN_DATA_PATH, "unmatched.rda"))
rm(list=ls(pattern="unmatched."))
