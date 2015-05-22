# load(paste0(TRAIN_DATA_PATH, "All-Data-Before-Preprocess.rda"))
##########################################################
# fix bad values
##########################################################
# look at missing and bad Hours
tt$Hours[tt$Hours==0 & !is.na(tt$Hours)] <- NA       

# Look at distribution of Source 136 with "00" and conclude that we should divide by 100.
getRowSource136_00 <- which(tt$Hours<100000 & tt$Hours > 20000 & !is.na(tt$Hours) & substr(tt$Hours, nchar(tt$Hours)-1, nchar(tt$Hours))=="00" & tt$Source==136)
if (length(getRowSource136_00)>0)
  tt$Hours[getRowSource136_00] <- tt$Hours[getRowSource136_00] / 100

# Divide tt$Hours > 100000 by 100.  Most likely reading the minutes.  426.
getRow100000 <- which(tt$Hours>=100000 & !is.na(tt$Hours))
if (length(getRow100000)>0)
  tt$Hours[getRow100000] <- tt$Hours[getRow100000] / 100    

# reduce really high hours.  Maybe reading a decimal.
tt$HoursCap <- tt$Hours
getRow32767 <- which(tt$Hours>32767 & !is.na(tt$Hours))
if (length(getRow32767)>0)
  tt$HoursCap[getRow32767] <- tt$Hours[getRow32767] / 10
rm(getRowSource136_00, getRow100000, getRow32767)

# parSapply(cl, tt[,c("BladeW","UnderW","StickL","Size","Tire")], function(x) sort(unique(x[!is.na(x)])))
tt$AucID[is.na(tt$AucID)] <- 0
tt$UseBand[tt$UseBand==""] <- NA
tt$Size[tt$Size==""] <- NA
tt$Enc[tt$Enc=="EROPS AC"] <- "EROPS w AC"
tt$Enc[tt$Enc=="NO ROPS"] <- ""
tt$Tire <- gsub(' inch', '', tt$Tire)
tt$Tire <- gsub('"', '', tt$Tire)
tt$Tire <- as.numeric(tt$Tire)
tt$UnderW <- as.numeric(gsub(' inch', '', tt$UnderW))
tt$BladeW[tt$BladeW=="<12'"] <- 10
tt$BladeW <- as.numeric(gsub("'", '', tt$BladeW))
tt$StickL <- strsplit(gsub('"', '', tt$StickL), "' ")
tt$StickL <- sapply(tt$StickL, function(x) {as.numeric(x[1]) + as.numeric(x[2])/12})
tt$GrouserType[tt$GrouserType=="Single"] <- ""
tt$Differ[tt$Differ=="Locking"] <- "No Spin"
tt$Steering[tt$Steering=="No"] <- ""
tt$Year <- as.numeric(tt$Year)

##########################################################
# fix bad text
##########################################################
# Clean text based fields  (will redo MKey later)
textCol <- c("MBase", "MSecond", "MSeries", "MDesc")
textCol2 <- names(which(parSapply(cl, tt[, 18:50], is.character)))

#Capitalize
mac[, textCol] <- parSapply(cl, mac[, textCol], toupper)
tt[, c(textCol, textCol2)] <- parSapply(cl, tt[, c(textCol, textCol2)], toupper)

# Collapse the spaces
# Change .00E+00 to number
# pattern <- "(\\s|\\.00E\\+00)"
mac[, textCol] <- parSapply(cl, mac[, textCol], function(x) gsub("(\\s|\\.00E\\+00)", "", x))
tt[, c(textCol, textCol2)] <- parSapply(cl, tt[, c(textCol, textCol2)], function(x) gsub("(\\s|\\.00E\\+00)", "", x))
rm(textCol, textCol2)

# common fields
mac <- fix_bad_values(mac)
tt <- fix_bad_values(tt)

# ##########################################################
# # Remap some ModelID?
# # some model ID have same MBase, MSecond, etc.
# # however, new models have later year mades, so useful info
# ##########################################################
# mac$ModelID[mac$ModelID==35801] <- 35802
# mac$ModelID[mac$ModelID==35803] <- 35804
# mac$ModelID[mac$ModelID==16467] <- 28540
# mac$ModelID[mac$ModelID==16484] <- 23608
# mac$ModelID[mac$ModelID==424] <- 28592

mac <- fix_replace(mac)
tt <- fix_replace(tt)

rn <- c("(^|-)(II|III|IV|V|VI|VII|VIII|IX)(-|$)")
rn.idx <- grep(rn, mac$MSeries)
rn.Mfg <- unique(paste(mac$ProdGp[rn.idx], mac$MfgID[rn.idx], sep="="))
rn.mac <- paste(mac$ProdGp, mac$MfgID, sep="=") %in% rn.Mfg
sum(rn.mac)
rn.ProdGpMBase <- unique(paste(mac$ProdGp[rn.mac], mac$MBase[rn.mac], sep="="))
rn.tt <- paste(tt$ProdGp, tt$MBase, sep="=") %in% rn.ProdGpMBase
sum(rn.tt)
rn.idx2 <- grep(rn, tt$MSeries)
rn.tt2 <- paste(tt$ProdGp, tt$MBase, sep="=") %in% unique(paste(tt$ProdGp[rn.idx2], tt$MBase[rn.idx2], sep="="))
sum(rn.tt2)

mac[rn.mac, c("MSecond","MSeries","MDesc")] <- clusterMap(cl, rn_convert, mac[rn.mac, c("MSecond","MSeries","MDesc")], SIMPLIFY=T)
tt[rn.tt, c("MSecond","MSeries","MDesc")] <- clusterMap(cl, rn_convert, tt[rn.tt, c("MSecond","MSeries","MDesc")], SIMPLIFY=T)
tt[rn.tt2, c("MSecond","MSeries","MDesc")] <- clusterMap(cl, rn_convert, tt[rn.tt2, c("MSecond","MSeries","MDesc")], SIMPLIFY=T)

mac[, c("MSecond","MSeries","MDesc")] <- clusterMap(cl, dash_convert, mac[, c("MSecond","MSeries","MDesc")], SIMPLIFY=T)
tt[, c("MSecond","MSeries","MDesc")] <- clusterMap(cl, dash_convert, tt[, c("MSecond","MSeries","MDesc")], SIMPLIFY=T)

mac <- fix_bad_values2(mac)
tt <- fix_bad_values2(tt)

tt <- redo_MKey(tt)
length(unique(tt$MKey)) # 4955
mac <- redo_MKey(mac)
length(unique(mac$MKey)) # 5469

rm(i, idx, r, s, u, n, desc, drops)
rm(ds, cols, make_null, rn, rn.Mfg, rn.ProdGpMBase, rn.idx, rn.idx2, rn.mac, rn.tt, rn.tt2)

# ##########################################################
# # Change Type
# ##########################################################
# # make into factors
# # sapply(mac, class)
# mac <- mac[, c("MacID", "ModelID", "MKey", "MBase", "MSecond", "MSeries", "MDesc", "MfgID", "ProdGp", "YearMade", "SizeL", "SizeU")]
MakeNumeric <- c("SizeL", "SizeU")
mac[, MakeNumeric] <- sapply(mac[, MakeNumeric], as.numeric)
# 
# MakeFactors <- c("MfgID", names(which(parSapply(cl, mac, class)=="character")))
# for (col in MakeFactors){
#   mac[, col] <- factor(mac[,col])  
# }
# 
# # ordered factors:  UseBand, Size, GrouserType
# # sapply(tt, class)
# tt$UseBand <- factor(tt$UseBand, levels=c("Low", "Medium", "High"), ordered=T)
# tt$Size <- factor(tt$Size, levels=c("Mini", "Compact", "Small", "Medium", "Large / Medium", "Large"), ordered=T)
# tt$GrouserType <- factor(tt$GrouserType, levels=c("Double", "Triple"), ordered=T)
# tt$Month <- factor(tt$Month, ordered=T)
# 
# # factors unordered
# MakeFactors <- c("Source", "AucID", names(which(parSapply(cl, tt, class)=="character")))
# names.tt <- names(tt)
# for (i in 1:ncol(tt)){
#   if (names.tt[i] %in% MakeFactors) tt[,i] <- factor(tt[,i])
# }
# rm(i, col, MakeNumeric, MakeFactors, names.tt)
# parSapply(cl, tt, class)
# 
save(mac, file=paste0(TRAIN_DATA_PATH, "mac-after-preprocess.rda"))
save(tt, file=paste0(TRAIN_DATA_PATH, "tt-after-preprocess.rda"))