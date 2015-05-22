# Use minimum year or median year since unknown year would also lower the price?
# To impute YearMade, first gather the earliest that model, mfg, etc combo appeared in appendix
# load(paste0(TRAIN_DATA_PATH, "mac-after-preprocess.rda"))
# load(paste0(TRAIN_DATA_PATH, "tt-after-preprocess.rda"))

mac.YearMade.min <- ddply(mac, .(ModelID, MBase, ProdGp, MfgID), summarize,
                          YearMade.med=median(YearMade, na.rm=T),
                          YearMade.min=min(YearMade, na.rm=T))
mac.YearMade.min <- mac.YearMade.min[!is.infinite(mac.YearMade.min$YearMade.min)                                   
                                     & !is.infinite(mac.YearMade.min$YearMade.med),]
mac <- merge(mac, mac.YearMade.min, by=c("ModelID", "MBase", "ProdGp", "MfgID"), all.x=T, all.Y=F, sort=F)

tt.YearMade.min <- ddply(tt, .(ModelID, MBase, ProdGp), summarize, 
                         YearMade.min=min(YearMade, na.rm=T),
                         YearMade.med=median(YearMade, na.rm=T))
tt.YearMade.min <- tt.YearMade.min[!is.infinite(tt.YearMade.min$YearMade.min)
                                   & !is.infinite(tt.YearMade.min$YearMade.med),]
tt <- merge(tt, tt.YearMade.min, by=c("ModelID", "MBase", "ProdGp"), all.x=T, all.Y=F, sort=F)
rm(mac.YearMade.min, tt.YearMade.min)

# impute with median of MBase.  Down to 4827 NA.
groupCols <- c("ProdGp", "MfgID", "MBase")
mac <- merge(mac, impute_by_group(mac, groupCols, "SizeL", fun=median),
             by=groupCols, suffixes=c("", ".med"), all.x=T, all.y=F, sort=F)
mac$SizeL.med[!is.na(mac$SizeL)] <- mac$SizeL[!is.na(mac$SizeL)]
mac <- merge(mac, impute_by_group(mac, groupCols, "SizeU", fun=median),
             by=groupCols, suffixes=c("", ".med"), all.x=T, all.y=F, sort=F)
mac$SizeU.med[!is.na(mac$SizeU)] <- mac$SizeU[!is.na(mac$SizeU)]
rm(groupCols)

# impute by inspection.  Down to 2333 NA.
mac[mac$ProdGp=="BL" & mac$MfgID=="25" & mac$MBase=="680",c("SizeL.med", "SizeU.med")] <- c(15, 16)
mac[mac$ProdGp=="BL" & mac$MfgID=="55" & mac$MBase=="455",c("SizeL.med", "SizeU.med")] <- c(14, 15)
mac[mac$ProdGp=="BL" & mac$MfgID=="55" & mac$MBase=="575",c("SizeL.med", "SizeU.med")] <- c(14, 15)
mac[mac$ProdGp=="BL" & mac$MfgID=="92" & mac$MBase=="3D",c("SizeL.med", "SizeU.med")] <- c(15, 16)
mac[mac$ProdGp=="MG" & mac$MfgID=="103" & mac$MBase=="GD355",c("SizeL.med", "SizeU.med")] <- c(45, 130)
mac[mac$ProdGp=="SSL" & mac$MfgID=="8" & mac$MBase=="RC150",c("SizeL.med", "SizeU.med")] <- c(2701, 2701)
mac[mac$ProdGp=="SSL" & mac$MfgID=="121" & mac$MBase=="533",c("SizeL.med", "SizeU.med")] <- c(701, 701)
mac[mac$ProdGp=="SSL" & mac$MfgID=="121" & mac$MBase=="631",c("SizeL.med", "SizeU.med")] <- c(976, 1251)
mac[mac$ProdGp=="SSL" & mac$MfgID=="121" & mac$MBase=="775",c("SizeL.med", "SizeU.med")] <- c(1601, 1751)
mac[mac$ProdGp=="SSL" & mac$MfgID=="135" & mac$MBase=="LX855",c("SizeL.med", "SizeU.med")] <- c(1751, 2201)
mac[mac$ProdGp=="SSL" & mac$MfgID=="135" & mac$MBase=="LX985",c("SizeL.med", "SizeU.med")] <- c(2201, 2701)
mac[mac$ProdGp=="SSL" & mac$MfgID=="609" & mac$MBase=="2070",c("SizeL.med", "SizeU.med")] <- c(2201, 2701)
mac[mac$ProdGp=="TEX" & mac$MfgID=="25" & mac$MBase=="220",c("SizeL.med", "SizeU.med")] <- c(21, 24)
mac[mac$ProdGp=="TEX" & mac$MfgID=="25" & mac$MBase=="9021",c("SizeL.med", "SizeU.med")] <- c(14, 16)
mac[mac$ProdGp=="TEX" & mac$MfgID=="74" & mac$MBase=="UH02",c("SizeL.med", "SizeU.med")] <- c(6, 8)
mac[mac$ProdGp=="TEX" & mac$MfgID=="74" & mac$MBase=="UH181",c("SizeL.med", "SizeU.med")] <- c(40, 50)
mac[mac$ProdGp=="TEX" & mac$MfgID=="103" & mac$MBase=="PC310",c("SizeL.med", "SizeU.med")] <- c(33, 40)
mac[mac$ProdGp=="TEX" & mac$MfgID=="103" & mac$MBase=="PC710",c("SizeL.med", "SizeU.med")] <- c(66, 90)
mac[mac$ProdGp=="TEX" & mac$MfgID=="121" & mac$MBase=="56",c("SizeL.med", "SizeU.med")] <- c(1, 2)
mac[mac$ProdGp=="TEX" & mac$MfgID=="121" & mac$MBase=="76",c("SizeL.med", "SizeU.med")] <- c(2, 3)
mac[mac$ProdGp=="TEX" & mac$MfgID=="121" & mac$MBase %in% c("X328","X331","X334"), c("SizeL.med", "SizeU.med")] <- c(3, 4)
mac[mac$ProdGp=="TEX" & mac$MfgID=="158" & mac$MBase=="MX14",c("SizeL.med", "SizeU.med")] <- c(14, 15)
mac[mac$ProdGp=="TEX" & mac$MfgID=="427" & mac$MBase=="HW180",c("SizeL.med", "SizeU.med")] <- c(19, 21)
mac[mac$ProdGp=="TFB" & mac$MfgID=="43" & mac$MBase=="853",c("SizeL.med", "SizeU.med")] <- c(190, 323)
mac[mac$ProdGp=="TFB" & mac$MfgID=="43" & mac$MBase=="903",c("SizeL.med", "SizeU.med")] <- c(214, 334)
mac[mac$ProdGp=="TFB" & mac$MfgID=="43" & mac$MBase=="953",c("SizeL.med", "SizeU.med")] <- c(214, 339)
mac[mac$ProdGp=="TH" & mac$MfgID=="121" & mac$MBase=="V518",c("SizeL.med", "SizeU.med")] <- c(2, 3.3)
mac[mac$ProdGp=="TTT" & mac$MfgID=="54" & mac$MBase=="11",c("SizeL.med", "SizeU.med")] <- c(105, 130)
mac[mac$ProdGp=="TTT" & mac$MfgID=="158" & mac$MBase=="SD250",c("SizeL.med", "SizeU.med")] <- c(190, 259)
mac[mac$ProdGp=="TTT" & mac$MfgID=="166" & mac$MBase=="8240",c("SizeL.med", "SizeU.med")] <- c(260, 260)
mac[mac$ProdGp=="VDDA" & mac$MfgID=="103" & mac$MBase=="JW33",c("SizeL.med", "SizeU.med")] <- c(8, 8)
mac[mac$ProdGp=="WEX" & mac$MfgID=="40" & mac$MBase=="DH130",c("SizeL.med", "SizeU.med")] <- c(17, 20)
mac[mac$ProdGp=="WEX" & mac$MfgID=="103" & mac$MBase=="PW05",c("SizeL.med", "SizeU.med")] <- c(13, 13)
mac[mac$ProdGp=="WEX" & mac$MfgID=="103" & mac$MBase=="PW128",c("SizeL.med", "SizeU.med")] <- c(13, 13)
mac[mac$ProdGp=="WL" & mac$MfgID=="43" & mac$MBase=="840",c("SizeL.med", "SizeU.med")] <- c(275, 350)
mac[mac$ProdGp=="WL" & mac$MfgID=="55" & mac$MBase=="A62",c("SizeL.med", "SizeU.med")] <- c(80, 90)
mac[mac$ProdGp=="WL" & mac$MfgID=="74" & mac$MBase=="LX200",c("SizeL.med", "SizeU.med")] <- c(200, 210)
mac[mac$ProdGp=="WL" & mac$MfgID=="95" & mac$MBase=="KLD88",c("SizeL.med", "SizeU.med")] <- c(230, 255)
mac[mac$ProdGp=="WL" & mac$MfgID=="103" & mac$MBase=="507",c("SizeL.med", "SizeU.med")] <- c(100, 110)
mac[mac$ProdGp=="WL" & mac$MfgID=="103" & mac$MBase=="550",c("SizeL.med", "SizeU.med")] <- c(200, 300)
mac[mac$ProdGp=="WL" & mac$MfgID=="166" & mac$MBase=="60c",c("SizeL.med", "SizeU.med")] <- c(150, 175)
mac[mac$ProdGp=="WL" & mac$MfgID=="448" & mac$MBase=="h100",c("SizeL.med", "SizeU.med")] <- c(225, 250)

# names(mac);names(tt)
save(mac, file=paste0(TRAIN_DATA_PATH, "mac-after-impute-manual.rda"))
save(tt, file=paste0(TRAIN_DATA_PATH, "tt-after-impute-manual.rda"))