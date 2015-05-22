fix_bad_values <- function(ds){
  
  # look at bad YearMade
  if (!is.null(ds$YearMade))
    ds$YearMade[(ds$YearMade<1940 | ds$YearMade==9999) & !is.na(ds$YearMade)] <- NA
  if (!is.null(ds$YearMade.x))
    ds$YearMade.x[(ds$YearMade.x<1940 | ds$YearMade.x==9999) & !is.na(ds$YearMade.x)] <- NA
  if (!is.null(ds$YearMade.y))
    ds$YearMade.y[(ds$YearMade.y<1940 | ds$YearMade.y==9999) & !is.na(ds$YearMade.y)] <- NA  
  
  # MBase - replace null with blank
  if (!is.null(ds$MBase))
    ds$MBase[is.na(ds$MBase)] <- ""
  
  # MSecond - look at -, 0., and #
  if (!is.null(ds$MSecond)){
    ds$MSecond[is.na(ds$MSecond)] <- ""
    ds$MSecond <- gsub("^#NAME\\?", "EX", ds$MSecond)
    ds$MSecond <- gsub("^0\\.", "\\.", ds$MSecond)   
  }
  
  # MSeries - look at -, -NA, #, and E+01
  if (!is.null(ds$MSeries)){    
    ds$MSeries[is.na(ds$MSeries)] <- ""
    ds$MSeries <- gsub("^\\-1\\.50E\\+01", "-15E0", ds$MSeries)
    ds$MSeries <- gsub("^#NAME\\?", "-IV", ds$MSeries)
    ds$MSeries[grep("[0-9]E0$", ds$MSeries)] <- gsub("E0", "", ds$MSeries[grep("[0-9]E0$", ds$MSeries)])    
  }
  
  # ds$MDesc of NA for MfgID==26, MSecond==H ProdGp=="MG", is not NA but the text "NA"
  # tt$MDesc of "" for MSecond==H ProdGp=="MG", is not NA but the text "NA"
  if (!is.null(ds$MDesc)){
    ds$MDesc[is.na(ds$MDesc)] <- ""
    ds$MDesc[ds$MDesc=="" & ds$ProdGp=="MG" & ds$MSecond=="H" & grepl("HNA", ds$MKey)] <- "NA"
    ds$MDesc[grep("[0-9]E0$", ds$MDesc)] <- gsub("E0", "", ds$MDesc[grep("[0-9]E0$", ds$MDesc)])    
  }  
  
  # SizeL and SizeU 5150 NA
  # SizeL of 0 and SizeU of 1e6 seem to indicate only 1 size necessary
  if (!is.null(ds$SizeL)){
    ds$SizeL[ds$SizeL==0 & !is.na(ds$SizeU)] <- ds[ds$SizeL==0 & !is.na(ds$SizeU), "SizeU"]
    ds$SizeU[ds$SizeU==1e6 & !is.na(ds$SizeL)] <- ds[ds$SizeU==1e6 & !is.na(ds$SizeL), "SizeL"]
  }  
  
  return(ds)
}

fix_bad_values2 <- function(ds){
  # ds<-mac
  # split MSecond -> MDesc MSeries # LC6LE
  idx <- grep("([A-Z]+)([0-9]+)([A-Z]+)$", ds$MSecond)
  # ds[idx,]
  for (i in idx){
    s <- ds$MSecond[i]
    r <- regexec("([A-Z]+)([0-9]+)([A-Z]+)$", s)
    ds$MSecond[i] <- regmatches(s,r)[[1]][2]
    ds$MSeries[i] <- regmatches(s,r)[[1]][3]
    ds$MDesc[i] <- regmatches(s,r)[[1]][4]          
  }
  
  # move numeric MSecond and add it to MSeries
  idx <- grep("(^|\\.)([0-9]+)([A-Z]*)", ds$MSecond)
  # ds[idx,]
  ds$MSeries[idx] <- paste0(ds$MSecond[idx],ds$MSeries[idx])
  ds$MSecond[idx] <- ""
  
  # split MSeries with numbers followed by 2 or more letters
  idx <- grep("([0-9]+)([A-Z]{2,})", ds$MSeries)
  # ds[idx,]
  for (i in idx){
    s <- ds$MSeries[i]
    r <- regexec("([0-9]+)([A-Z]{2,})", s)
    ds$MSeries[i] <- regmatches(s,r)[[1]][2]
    ds$MDesc[i] <- regmatches(s,r)[[1]][3]      
  }
  
  # switch numeric Desc to Series
  idx <- grep("^([0-9]+)([A-Z]*)", ds$MDesc)
  # ds[idx,]
  ds[idx, c("MSeries", "MDesc")] <- ds[idx, c("MDesc", "MSeries")]
  
  # numeric MSeries with 1 letter in MDesc (target Komatsu)
  idx <- ds$ProdGp=="TEX" & grepl("^PC", ds$MBase) & grepl("^([0-9]+)$", ds$MSeries) & grepl("^[A-Z]{1}$", ds$MDesc)
  # ds[idx,]
  ds$MSeries[idx] <- paste0(ds$MSeries[idx],ds$MDesc[idx])
  ds$MDesc[idx] <- ds$MSecond[idx]
  ds$MSecond[idx] <- ""
  
  # change Komatsu 3NO
  idx <- ds$ProdGp=="TEX" & grepl("^PC", ds$MBase) & grepl("^([0-9]+[A-Z]{2}$)", ds$MSeries)
  # ds[idx,]
  ds$MSeries[idx] <- 3
  
  # MSeries 2 letters or more
  idx <- grepl("([A-Z]{2,})", ds$MSeries) & ds$MSecond==""
  # ds[idx,]
  ds$MSecond[idx] <- ds$MDesc[idx]
  ds$MDesc[idx] <- ds$MSeries[idx]
  ds$MSeries[idx] <- ""
  
  # move ZTS from Second to Desc if Desc is blank
  idx <- ds$MSecond=='ZTS' & ds$MDesc==""
  ds[idx, c("MSecond", "MDesc")] <- ds[idx, c("MDesc", "MSecond")]  
  idx <- ds$MSeries=='ZTS' & ds$MDesc==""
  ds[idx, c("MSeries", "MDesc")] <- ds[idx, c("MDesc", "MSeries")]
  
  # MSeries 2 letters or more
  idx <- grepl("([A-Z]{2,})", ds$MSeries) & ds$MDesc==""
  # ds[idx,]
  ds$MDesc[idx] <- ds$MSeries[idx]
  ds$MSeries[idx] <- ""
  
  # MSeries 2000 -> 2
  ds$MSeries <- sub("2000", "2", ds$MSeries)
  ds$MSeries <- sub("1970", "", ds$MSeries)
  
  # MSeries not number
  idx <- grep("^D7$", ds$MSeries)
  # ds[idx,]
  ds$MDesc[idx] <- "D"
  ds$MSeries[idx] <- 7
  
  idx <- grepl("^[A-Z]", ds$MSeries) & ds$MDesc==""
  # ds[idx,]
  ds[idx, c("MSeries", "MDesc")] <- ds[idx, c("MDesc", "MSeries")]
  
  idx <- grepl("^(ZTS|ZHS)", ds$MSeries)
  # ds[idx,]
  ds$MDesc[idx] <- ds$MSeries[idx]
  ds$MSeries[idx] <- ""
  
  idx <- grepl("^LP|TC", ds$MSeries) & ds$MDesc!=""
  # ds[idx,]
  ds$MSeries[idx] <- ""
  
  idx <- grepl("^[A-Z]", ds$MSeries) & ds$MDesc!=""
  # ds[idx,]
  ds$MSeries[idx] <- ""
  
  # MSecond
  idx <- grep("^(ZTS|ZHS)", ds$MSecond)
  # ds[idx,]
  ds$MDesc[idx] <- ds$MSecond[idx]
  ds$MSecond[idx] <- ""
  
  idx <- grepl("LGP", ds$MSecond) & ds$MDesc==""
  # ds[idx,]
  for (i in which(idx)){  
    s <- ds$MSecond[i]
    r <- regexec("([A-Z]*)(LGP[A-Z]*)", s)
    ds$MSecond[i] <- regmatches(s,r)[[1]][2]
    ds$MDesc[i] <- regmatches(s,r)[[1]][3]      
  }
  
  
  ##################
  # BL
  ##################    
  idx <- ds$ProdGp=="BL" & ds$MSecond=="" & ds$MDesc!=""  & !(ds$MDesc %in% c("FOURWD", "LL"))
  # ds[idx,]
  ds$MSecond[idx] <- ds$MDesc[idx]
  ds$MDesc[idx] <- ""
  
  idx <- ds$ProdGp=="BL" & ds$MSecond=="E" & ds$MDesc=="ST" 
  # ds[idx,]
  ds$MSecond[idx] <- paste0(ds$MSecond[idx],ds$MDesc[idx])
  ds$MDesc[idx] <- ""
  
  idx <- ds$ProdGp=="BL" & grepl("^[0-9]+$", ds$MSeries) & ds$MBase=="CX"
  # ds[idx,]
  ds$MBase[idx] <- paste0(ds$MBase[idx],ds$MSeries[idx])
  ds$MSeries[idx] <- ""
  
  idx <- ds$ProdGp=="BL" & grepl("^[0-9]+$", ds$MBase) & ds$MSecond=="CX"
  # ds[idx,]
  ds$MBase[idx] <- paste0(ds$MSecond[idx],ds$MBase[idx])
  ds$MSecond[idx] <- ds$MDesc[idx]
  ds$MDesc[idx] <- ""
  
  idx <- ds$ProdGp=="BL" & grepl("^MINICX+$", ds$MBase)
  # ds[idx,]
  ds$MBase[idx] <- "CX0"
  
  ##################
  # MG
  ##################
  idx <- ds$ProdGp=="MG" & ds$MBase=="ZSUPER300"
  # ds[idx,]
  ds$MBase[idx] <- "300"
  ds$MSecond[idx] <- "ZSUPER300"
  
  
  idx <- ds$ProdGp=="MG" & grepl("^GD[0-9]+", ds$MBase) & ds$MSecond=="AW"
  # ds[idx,]
  ds$MSecond[idx] <- "A"
  
  idx <- ds$ProdGp=="MG" & grepl("^[A-Z]{1}H$", ds$MSecond)
  # ds[idx,]
  for (i in which(idx)){  
    s <- ds$MSecond[i]
    r <- regexec("^([A-Z]{1})H$", s)
    ds$MSecond[i] <- regmatches(s,r)[[1]][2]
    ds$MDesc[i] <- "H"
  }
  
  idx <- ds$ProdGp=="MG" & grepl("^D[0-9]{3}$", ds$MBase)
  # ds[idx,]
  for (i in which(idx)){  
    s <- ds$MBase[i]
    r <- regexec("^D([0-9]{3})$", s)
    ds$MBase[i] <- regmatches(s,r)[[1]][2]
  }
  
  ##################
  # SSL
  ##################
  idx <- ds$ProdGp=="SSL" & grepl("^([0-9]+)([A-Z]+[0-9]+)$", ds$MBase)
  # ds[idx,]  
  for (i in which(idx)){  
    s <- ds$MBase[i]
    r <- regexec("^([0-9]+)([A-Z]+[0-9]+)$", s)
    ds$MBase[i] <- regmatches(s,r)[[1]][3]
    ds$MSeries[i] <- regmatches(s,r)[[1]][2]
  }
  
  idx <- ds$ProdGp=="SSL" & ds$MBase=="RCV"
  # ds[idx,]
  ds$MBase[idx] <- "RC5"
  
  idx <- ds$ProdGp=="SSL" & grepl("^[0-9]+[A-Z]+$", ds$MBase)
  # ds[idx,]
  for (i in which(idx)){  
    s <- ds$MBase[i]
    r <- regexec("^([0-9]+)([A-Z]+)$", s)
    ds$MBase[i] <- regmatches(s,r)[[1]][2]
    ds$MSecond[i] <- regmatches(s,r)[[1]][3]
  }
  
  # MSeries number with 1 letter in MDesc - target komatsu
  idx <- ds$ProdGp=="SSL" & grepl("^SK", ds$MBase) & grepl("^([0-9]+)$", ds$MSeries) & grepl("^[A-Z]{1}$", ds$MDesc)
  # ds[idx,]
  ds$MSeries[idx] <- paste0(ds$MSeries[idx],ds$MDesc[idx])
  ds$MDesc[idx] <- ""
  
  idx <- ds$ProdGp=="SSL" & ds$MDesc=="TURBO"
  # ds[idx,]
  ds$MSecond[idx] <- "ZTURBO"
  
  ##################
  # TEX
  ##################
  # desc <- c("CK","CR","CRSB","HD","HDSL", "K", "L","LC","LN","LR","LCD","LCM","LCR","LCRTS", "LRR","LT","NC","NL","NLC","SA","SR","SRLC","SRDZ","SSR","RTS","U","UR","TK","XD","Z","ZTS")
  desc <- c("CR","L","LC","LN","LR","LT","SB","SR","RTS","U","UR","ZTS")
  desc.reg <- paste0(desc,collapse="|")
  
  desc2 <- c("CR","LC","LN","LR","LT","SR","RTS","UR","ZTS")
  desc2.reg <- paste0(desc2,collapse="|")
  
  idx <- ds$ProdGp=="TEX" & ds$MBase=="VC20/20"
  # ds[idx,]  
  ds$MBase[idx] <- 'VC20.5'
  
  # replace variations with more common categories
  idx <- ds$ProdGp=="TEX" & grepl("^(LCH|LCR|LRR|RR|LE|LK|LL|LM|LN)$", ds$MDesc)
  ds$MDesc[idx] <- "L"
  idx <- ds$ProdGp=="TEX" & grepl("^(LCH|LCR|LRR|RR|LE|LK|LL|LM|LN)$", ds$MSecond)
  ds$MDesc[idx] <- "L"
  ds$MSecond[idx] <- ""
  
  idx <- ds$ProdGp=="TEX" & grepl("^(LCLR)$", ds$MDesc)
  ds$MDesc[idx] <- "LR"
  idx <- ds$ProdGp=="TEX" & grepl("^(LCLR)$", ds$MSecond)
  ds$MDesc[idx] <- "LR"
  ds$MSecond[idx] <- ""
  
  idx <- ds$ProdGp=="TEX" & grepl("^(SSR|SRDZ)$", ds$MDesc)
  ds$MDesc[idx] <- "SR"
  idx <- ds$ProdGp=="TEX" & grepl("^(SSR|SRDZ)$", ds$MSecond)
  ds$MDesc[idx] <- "SR"
  ds$MSecond[idx] <- ""
  
  idx <- ds$ProdGp=="TEX" & grepl("^(LU)$", ds$MDesc)
  ds$MDesc[idx] <- "U"
  idx <- ds$ProdGp=="TEX" & grepl("^(LU)$", ds$MSecond)
  ds$MDesc[idx] <- "U"
  ds$MSecond[idx] <- ""
  
  idx <- ds$ProdGp=="TEX" & grepl("^KA$", ds$MDesc)
  ds$MDesc[idx] <- ""
  ds$MSecond[idx] <- "K"
  idx <- ds$ProdGp=="TEX" & grepl("^KA$", ds$MSecond)
  ds$MDesc[idx] <- ""
  ds$MSecond[idx] <- "K"
  
  idx <- ds$ProdGp=="TEX" & grepl("^(BH|BN)$", ds$MDesc)
  ds$MDesc[idx] <- ""
  ds$MSecond[idx] <- "B"
  idx <- ds$ProdGp=="TEX" & grepl("^(BH|BN)$", ds$MSecond)
  ds$MDesc[idx] <- ""
  ds$MSecond[idx] <- "B"
  
  idx <- ds$ProdGp=="TEX" & grepl("^CKB$", ds$MDesc)
  ds$MDesc[idx] <- ""
  ds$MSecond[idx] <- "CK"
  idx <- ds$ProdGp=="TEX" & grepl("^CKB$", ds$MSecond)
  ds$MDesc[idx] <- ""
  ds$MSecond[idx] <- "CK"
  
  idx <- ds$ProdGp=="TEX" & grepl("^CLR$", ds$MDesc)
  ds$MDesc[idx] <- "LR"
  ds$MSecond[idx] <- "C"
  idx <- ds$ProdGp=="TEX" & grepl("^CLR$", ds$MSecond)
  ds$MDesc[idx] <- "LR"
  ds$MSecond[idx] <- "C"
  
  idx <- ds$ProdGp=="TEX" & grepl("^DC$", ds$MDesc)
  ds$MDesc[idx] <- ""
  ds$MSecond[idx] <- "D"
  idx <- ds$ProdGp=="TEX" & grepl("^DC$", ds$MSecond)
  ds$MDesc[idx] <- ""
  ds$MSecond[idx] <- "D"
  
  idx <- ds$ProdGp=="TEX" & (grepl("^(DZLC)$", ds$MDesc) | grepl("^(DZLC)$", ds$MSecond))
  
  ds$MDesc[idx] <- "LC"
  ds$MSecond[idx] <- "DZ"
  
  # drop rare categories
  drops <- c("BE", "EN", "M", "NA", "PRO", "SA", "TH", "V", "XD")
  ds$MDesc[ds$ProdGp=="TEX" & ds$MDesc %in% drops] <- ""
  ds$MSecond[ds$ProdGp=="TEX" & ds$MSecond %in% drops] <- ""
  
  # clean MBase
  idx <- ds$ProdGp=="TEX" & grepl(paste0("([A-Z]*[0-9]+)(",desc.reg,")$"), ds$MBase)
  for (i in which(idx)){  
    s <- ds$MBase[i]
    r <- regexec(paste0("([A-Z]*[0-9]+)(",desc.reg,")$"), s)
    ds$MBase[i] <- regmatches(s,r)[[1]][2]
    ds$MDesc[i] <- regmatches(s,r)[[1]][3]
  }
  
  idx <- ds$ProdGp=="TEX" & grepl("([A-Z]*[0-9]+)-([A-Z0-9]+)$", ds$MBase)
  # ds[idx,]
  for (i in which(idx)){  
    s <- ds$MBase[i]
    r <- regexec("([A-Z]*[0-9]+)-([A-Z0-9]+)$", s)
    ds$MBase[i] <- regmatches(s,r)[[1]][2]
    ds$MSecond[i] <- regmatches(s,r)[[1]][3]
  }
  
  idx <- ds$ProdGp=="TEX" & grepl("([A-Z]*[0-9]+)([A-Z]+)$", ds$MBase)
  # ds[idx,]
  for (i in which(idx)){  
    s <- ds$MBase[i]
    r <- regexec("([A-Z]*[0-9]+)([A-Z]+)$", s)
    ds$MBase[i] <- regmatches(s,r)[[1]][2]
    ds$MSecond[i] <- regmatches(s,r)[[1]][3]
  }
  
  idx <- ds$ProdGp=="TEX" & ds$MSecond=="B" & grepl("(^B[0-9]{2})2$", ds$MBase)
  # ds[idx,]
  for (i in which(idx)){  
    s <- ds$MBase[i]
    r <- regexec("(^B[0-9]{2})2$", s)
    ds$MBase[i] <- regmatches(s,r)[[1]][2]
    ds$MSecond[i] <- "2B"
  }
  
  # split Second that contain a suffix L
  idx <- ds$ProdGp=="TEX" & grepl("^([A-Z]{1})L$", ds$MSecond)
  # ds[idx,]
  for (i in which(idx)){  
    s <- ds$MSecond[i]
    r <- regexec("^([A-Z]{1})L$", s)
    ds$MSecond[i] <- regmatches(s,r)[[1]][2]
    ds$MDesc[i] <- "L"
  }
  
  idx <- ds$ProdGp=="TEX" & grepl("^CL$", ds$MDesc)
  # ds[idx,]
  ds$MSecond[idx] <- "C"
  ds$MDesc[idx] <- "L"
  
  idx <- ds$ProdGp=="TEX" & grepl(paste0("^([A-Z]+?)(LR|LC)$"), ds$MSecond)
  #   ds[idx,]
  for (i in which(idx)){  
    s <- ds$MSecond[i]
    r <- regexec("^([A-Z]+?)(LR|LC)$", s)
    ds$MSecond[i] <- regmatches(s,r)[[1]][2]
    ds$MDesc[i] <- regmatches(s,r)[[1]][3]
  }
  
  idx <- ds$ProdGp=="TEX" & !grepl(paste0("^(",desc.reg,")$"), ds$MDesc) & ds$MDesc!="" & ds$MSecond==""
  # ds[idx,]
  ds[idx, c("MDesc","MSecond")] <- ds[idx, c("MSecond","MDesc")]
  
  idx <- ds$ProdGp=="TEX" & grepl(paste0("^(",desc.reg,")$"), ds$MSecond) & ds$MDesc==""
  ds[idx, c("MDesc","MSecond")] <- ds[idx, c("MSecond","MDesc")]
  
  idx <- ds$ProdGp=="TEX" & grepl(paste0("^(",desc.reg,")$"), ds$MSecond) & !grepl(paste0("^(",desc.reg,")$"), ds$MDesc)
  ds[idx, c("MDesc","MSecond")] <- ds[idx, c("MSecond","MDesc")]
  
  # do more splitting
  idx <- ds$ProdGp=="TEX" & (ds$MDesc=="LC8" | ds$MSecond=="LC8")
  ds$MDesc[idx] <- "LC"
  ds$MSeries[idx] <- 8
  idx <- ds$ProdGp=="TEX" & (ds$MDesc %in% c("LCN", "NLC") | ds$MSecond %in% c("LCN", "NLC"))
  # ds[idx,]
  ds$MDesc[idx] <- "LC"
  ds$MSecond[idx] <- "N"
  idx <- ds$ProdGp=="TEX" & (ds$MDesc=="SRLC" | ds$MSecond=="SRLC")
  # ds[idx,]
  ds$MDesc[idx] <- "LC"
  ds$MSecond[idx] <- "SR"
  idx <- ds$ProdGp=="TEX" & (ds$MDesc=="USLC" | ds$MSecond=="USLC")
  # ds[idx,]
  ds$MDesc[idx] <- "LC"
  ds$MSecond[idx] <- "US"
  idx <- ds$ProdGp=="TEX" & (ds$MDesc=="ZHSG" | ds$MSecond=="ZHSG")
  # ds[idx,]
  ds$MDesc[idx] <- ""
  ds$MSecond[idx] <- "ZHS"
  
  idx <- ds$ProdGp=="TEX" & !grepl(paste0("^(",desc.reg,")$"), ds$MDesc) & ds$MDesc!="" & ds$MSecond==""
  # ds[idx,]
  ds[idx, c("MDesc","MSecond")] <- ds[idx, c("MSecond","MDesc")]
  
  rm(desc, desc.reg, desc2, desc2.reg, drops)
  
  ##################
  # TTT
  ##################
  idx <- ds$ProdGp=="TTT"
  
  # ds[idx,]
  desc <- c("LGP","LT","SA","SLGP","WT","XL","XLT","XLVP","XP","XR","XW")
  desc.reg <- paste0(desc,collapse="|")
  
  idx <- ds$ProdGp=="TTT" & grepl(paste0("^(",desc.reg,")$"), ds$MSecond) 
  ds[idx, c("MDesc","MSecond")] <- ds[idx, c("MSecond","MDesc")]
  # ds[idx,]
  
  idx <- ds$ProdGp=="TTT" & ds$MDesc %in% c("LGPVP", "SLGP", "LGPPS")
  ds$MDesc[idx] <- "LGP"
  
  idx <- ds$ProdGp=="TTT" & ds$MDesc %in% c("XLVP")
  ds$MDesc[idx] <- "XL"
  
  idx <- ds$ProdGp=="TTT" & ds$MDesc %in% c("BE")
  ds$MDesc[idx] <- ""
  ds$MSecond[idx] <- "B"
  
  idx <- ds$ProdGp=="TTT" & ds$MDesc %in% c("WLT")
  ds$MDesc[idx] <- "WT"
  
  idx <- ds$ProdGp=="TTT" & ds$MDesc %in% c("LRC")
  ds$MDesc[idx] <- ""
  
  idx <- ds$ProdGp=="TTT" & ds$MDesc=="L" & ds$MSecond==""
  ds[idx, c("MDesc","MSecond")] <- ds[idx, c("MSecond","MDesc")]
  # ds[idx,]
  
  rm(desc, desc.reg)
  
  ##################
  # WL
  ##################
  desc <- c('LL', 'LR', 'HT', 'TC', 'TM', 'TP', 'P', 'XR', 'XLT')
  
  idx <- ds$ProdGp=="WL" & ds$MSecond %in% desc
  ds[idx, c("MDesc","MSecond")] <- ds[idx, c("MSecond","MDesc")]
  
  idx <- ds$ProdGp=="WL" & grepl("^[0-9]+$", ds$MBase) & ds$MSecond %in% c("XT","XL","XLT") & ds$MDesc=="" 
  # ds[idx,]
  ds[idx, c("MDesc","MSecond")] <- ds[idx, c("MSecond","MDesc")]
  
  idx <- ds$ProdGp=="WL" & grepl("^HL[0-9]+$", ds$MBase) & ds$MSecond=="XTD"
  # ds[idx,]
  ds$MSecond[idx] <- "XT"
  ds$MDesc[idx] <- "D"
  
  drops <- c("PRO", "V", 'H90')
  idx <- ds$ProdGp=="WL" & ds$MSecond %in% drops
  # ds[idx,]
  ds$MSecond[idx] <- ""
  
  idx <- ds$ProdGp=="WL" & ds$MDesc %in% drops
  # ds[idx,]
  ds$MDesc[idx] <- ""
  
  idx <- ds$ProdGp=="WL" & grepl("^4[0-9]{2}$", ds$MBase) & ds$MSecond %in% c("ZX","Z") & ds$MSeries=="" & ds$MDesc==""
  # ds[idx,]
  ds[idx, c("MDesc","MSecond")] <- ds[idx, c("MSecond","MDesc")]
  
  idx <- ds$ProdGp=="WL" & ds$MSecond=="HZ" & ds$MSeries=="" & ds$MDesc==""
  ds$MSecond[idx] <- "H"
  ds$MDesc[idx] <- "Z"
  
  idx <- ds$ProdGp=="WL" & ds$MSecond=="A" & ds$MSeries==7 & ds$MDesc=="TM"
  # ds[idx,]
  ds$MSecond[idx] <- ''
  ds$MSeries[idx] <- '7A'
  
  idx <- ds$ProdGp=="WL" & ds$MSecond=="ZTM" & ds$MDesc==""
  # ds[idx,]
  ds$MSecond[idx] <- "Z"
  ds$MDesc[idx] <- "TM"
  
  idx <- ds$ProdGp=="WL" & grepl("^WA", ds$MBase) & (
    (ds$MSecond=="" & ds$MDesc %in% c("PT", "PTC")) | (ds$MSecond=="MC" & ds$MDesc==""))
  # ds[idx,]
  ds[idx, c("MDesc","MSecond")] <- ds[idx, c("MSecond","MDesc")]
  
  idx <- ds$ProdGp=="WL" & grepl("^WA", ds$MBase) & ds$MSecond %in% c("PTC", 'PZ')
  # ds[idx,]
  ds$MSecond[idx] <- "PT"
  
  idx <- ds$ProdGp=="WL" & grepl("^WA", ds$MBase) & grepl("^([0-9]{1})([A-Z]{1})$", ds$MSeries) & ds$MDesc==""
  # ds[idx,]
  for (i in which(idx)){  
    s <- ds$MSeries[i]
    r <- regexec("^([0-9]{1})([A-Z]{1})$", s)
    ds$MSeries[i] <- regmatches(s,r)[[1]][2]
    ds$MDesc[i] <- regmatches(s,r)[[1]][3]
  }
  
  # final clean of MBases
  idx <- grep("[0-9]+-[0-9]+", ds$MBase)
  ds[idx,]
  ds$MBase[idx] <- sub('-', '.', ds$MBase[idx])
  
  idx <- grep("^([0-9]+)([A-Z]+)([0-9]*)$", ds$MBase)
  ds[idx,]
  for (i in idx){  
    s <- ds$MBase[i]
    r <- regexec("^([0-9]+)([A-Z]+)([0-9]*)$", s)
    ds$MSeries[i] <- regmatches(s,r)[[1]][2]
    ds$MBase[i] <- paste0(regmatches(s,r)[[1]][3],regmatches(s,r)[[1]][4])
  }
  
  #   which(is.na(ds[, c("MBase","MSecond","MSeries","MDesc")]))
  
  return(ds)
}

redo_MKey <- function(ds){
  model <- c("MBase", "MSecond", "MSeries", "MDesc")
  model <- model[model %in% names(ds)]
  ds$MKey <- apply(ds[, model], 1, function(x) paste(x, collapse="="))  
  ds
}

fix_replace <- function(ds){
  # ds <- mac
  # ds <- tt
  cols <- which(names(ds) %in% c("MBase","MSecond","MSeries","MDesc"))
  ds[, cols] <- parApply(cl, ds[,cols], 2, function(x) sub("SUPER", "ZSUPER", x))
  # Some values either add no value or too rare  
  ds[, cols] <- parApply(cl, ds[,cols], 2, function(x) gsub("-*MARK|21KOMSTAT|SERIES|ELITE|STROKE|DELIMBER|BOSS|DELUXE|FASTRACK|FASTRRACK|HIGHLIFT|GALEO|PLUS|SITEMASTER|AWS|AVANCE", "", x))
  ds[, cols] <- parApply(cl, ds[,cols], 2, function(x) sub("LITRONIC", "LC", x))
  ds[, cols] <- parApply(cl, ds[,cols], 2, function(x) sub("LONGREACH", "LR", x))
  ds[, cols] <- parApply(cl, ds[,cols], 2, function(x) sub("LOGLOADER", "LL", x))
  ds[, cols] <- parApply(cl, ds[,cols], 2, function(x) sub("4X4X4|4WD", "FOURWD", x))  
  ds[, cols] <- parApply(cl, ds[,cols], 2, function(x) sub("14FT", "FFT", x))
  ds[, cols] <- parApply(cl, ds[,cols], 2, function(x) sub("AE0", "A", x))
  ds[, cols] <- parApply(cl, ds[,cols], 2, function(x) sub("FASTTRACK|FASTRRACK", "FAST", x))
  ds[, cols] <- parApply(cl, ds[,cols], 2, function(x) sub("(LGP|XL)VP", "\\1", x))
  
  switch_col <- grepl("ZSUPER", ds$MSeries)
  ds[switch_col, c("MSecond", "MSeries")] <- ds[switch_col, c("MSeries", "MSecond")]
  switch_col <- grepl("ZSUPER", ds$MDesc)
  ds[switch_col, c("MSecond", "MDesc")] <- ds[switch_col, c("MDesc", "MSecond")]
  
  switch_col <- grepl("ZSUPER", ds$MSecond) & grepl("^[A-Z]$", ds$MSeries)
  ds$MSecond[switch_col] <- paste0(ds$MSecond[switch_col], ds$MSeries[switch_col])
  ds$MSeries[switch_col] <- ds$MDesc[switch_col]
  ds$MDesc[switch_col] <- ""
  
  switch_col <- grepl("VHP", ds$MSecond)
  ds[switch_col, c("MSecond", "MDesc")] <- ds[switch_col, c("MDesc", "MSecond")]
  switch_col <- grepl("VHP", ds$MSeries)
  ds[switch_col, c("MSeries", "MDesc")] <- ds[switch_col, c("MDesc", "MSeries")]
  
  ds
}

rn_convert <- function(s){
  #   switch(s,"II"=2, "III"=3, "IV"=4, "V"=5, "VI"=6, "VII"=7, "VIII"=8, "Is"=9, s)
  s <- sub("(^|-)VIII(-|$)", "8\\2", s)
  s <- sub("(^|-)VII(-|$)", "7\\2", s)
  s <- sub("(^|-)VI(-|$)", "6\\2", s)
  s <- sub("(^|-)IV(-|$)", "4\\2", s)
  s <- sub("(^|-)V(-|$)", "5\\2", s)
  s <- sub("(^|-)III(-|$)", "3\\2", s)
  s <- sub("(^|-)II(-|$)", "2\\2", s)
  s <- sub("(^|-)Is(-|$)", "9\\2", s)
  s <- sub("(^|-)I(-|$)", "1\\2", s)
  s
}

dash_convert <- function(s){  
  sub("^([0-9]+)-([0-9]*)$","\\1.\\2", s)  
  #   sub("-([A-Z0-9]+)$","\\1", s)  
  sub("-","", s)  
} 

split_base <- function(s){
  r <- regexec("^([A-Z]*)([0-9]*)(\\.?[0-9]*)$", as.character(s))
  pre <- regmatches(s,r)[[1]][2] # pre
  num <- regmatches(s,r)[[1]][3] # num
  dec <- regmatches(s,r)[[1]][4] # dec    
  l <- nchar(num)    
  
  nums <- vector(mode="character", length=4)
  if (l >=4){
    nums[1] <- substr(num, 1, l-3)
    nums[2] <- substr(num, l-2, l-2)
    nums[3] <- substr(num, l-1, l-1)
    nums[4] <- paste0(substr(num, l, l), dec)
  } else {
    if (l>=3){
      nums[2] <- substr(num, l-2, l-2)
      nums[3] <- substr(num, l-1, l-1)
      nums[4] <- paste0(substr(num, l, l), dec)
    } else {
      if (l>=2){
        nums[3] <- substr(num, l-1, l-1)
        nums[4] <- paste0(substr(num, l, l), dec)
      } else {
        nums[4] <- paste0(substr(num, l, l), dec)}}}
  
  return(list(pre, nums))
}

split_series <- function(s){
  r <- regexec("^([0-9]*)([A-Z]*)$", as.character(s))
  s1 <- regmatches(s,r)[[1]][2] # number
  s2 <- regmatches(s,r)[[1]][3] # letter
  return(list(s1, s2))
}

get_age <- function(BeginYear, EndDate, PadDays){  
  # create age at sale variable; add 6 months since models come out before the calendar year
  require(lubridate)
  age <- as.numeric(as.POSIXlt(EndDate)
                    - ymd(paste(BeginYear, '01', '01', sep='-'))
                    + PadDays)/365
  #age[age<0] <- NA
  return(age)
}

impute_by_group <- function(ds, groups, values, fun=median, na.rm=T){
  # IN:  Dataframe, grouping variable names, values, aggregation function, NA flag.
  # DO:  Aggregates the values by the grouping levels.  Removes NA rows before and after if flagged.
  # OUT: Dataframe of grouping variables and aggregated values.  
  #NOTE: User responsible for passing values as numbers if that's needed.
  require(plyr)
  # remove NA values if desired  
  if (na.rm) ds <- ds[!is.na(ds[,values]),]
  agg <- ddply(ds, groups, function(x, idx){fun(x[,idx])}, values)
  names(agg)[ncol(agg)] <- values
  # remove NA before returning
  if (na.rm) agg <- agg[!is.na(agg[, ncol(agg)]), ]
  return(agg)  
}

rms <- function (o, p){
  sqrt(mean((o-p)^2))  
}
