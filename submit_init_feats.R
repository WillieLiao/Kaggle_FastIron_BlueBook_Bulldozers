# Feature groupings
rm(list=ls(pattern="feats"))
feats.mac <- c('MBase','MSecond','MSeries','MDesc','pre','MfgID')
feats.mkt <- c('market','models')
feats.auc <- c('Source','AucID','State')
feats.sale <- c('Year','Month')
feats.cpi <- c('cpi_us.imp','cpi_trucks.imp','pc_agri.imp','pc_contr.imp','wp_agri.imp','wp_contr.imp')

feats.age <- c('Age','YearMade')
feats.age.imp <- sapply(feats.age, function(x) paste0(x, '.imp'))
feats.age.med.min <- c('Age.med', 'YearMade.med', 'Age.min', 'YearMade.min') 
feats.age.med.min.imp <- sapply(feats.age.med.min, function(x) paste0(x, '.imp'))

feats.sizeLU.med <- c('SizeL.med','SizeU.med')
feats.sizeLU.med.imp <-  sapply(feats.sizeLU.med, function(x) paste0(x, '.imp'))
feats.sizeLU <- c('SizeL','SizeU')
feats.sizeLU.imp <-  sapply(feats.sizeLU, function(x) paste0(x, '.imp'))

feats.specific <- c('DriveSys','Enc','Forks','Pad','Ride','Stick','Trans','Turbo','Blade','EncType','HP','Hydraulics','Pushblock','Ripper','Scarifier','Tip','Tire','Coupler','CouplerSys','Grouser','HydraFlow','Track','Thumb','Pattern','GrouserType','Backhoe','BladeType','Travel','Differ','Steering')
feats.specific.imp <- c('DriveSys','Enc','Forks','Pad','Ride','Stick','Trans','Turbo','Blade','EncType','HP','Hydraulics','Pushblock','Ripper','Scarifier','Tip','Tire.imp','Coupler','CouplerSys','Grouser','HydraFlow','Track','Thumb','Pattern','GrouserType','Backhoe','BladeType','Travel','Differ','Steering')

feats.imp <- c(feats.mac, feats.sale, feats.age.imp, feats.sizeLU.med.imp, feats.specific.imp, feats.cpi)
names(feats.imp) <- c()
feats.orig <- c(feats.mac, feats.mkt, feats.sale, feats.age, feats.sizeLU.med, feats.specific, feats.cpi)
names(feats.orig) <- c()

feats.imp2 <- c(feats.imp, feats.age.med.min.imp, feats.sizeLU.imp, feats.auc, feats.mkt)
names(feats.imp2) <- c()
feats.orig2 <- c(feats.orig, feats.age.med.min, feats.sizeLU, feats.auc)
names(feats.orig2) <- c()

feats.focused <- c('MKey', 'MfgID', feats.auc, feats.sale, feats.age, feats.sizeLU, feats.specific, feats.cpi, 'HoursCap')
names(feats.orig) <- c()
feats.many <- c('MacID', 'MKey', 'MfgID', feats.sale, feats.age, feats.age.med.min, feats.sizeLU, feats.specific, 'HoursCap', 'Size', 'UseBand', feats.cpi, feats.auc)
names(feats.many) <- c()

rm(feats.mac, feats.mkt, feats.auc, feats.sale, feats.cpi, feats.age, feats.age.imp, feats.age.med.min, feats.age.med.min.imp, feats.sizeLU.med, feats.sizeLU.med.imp, feats.sizeLU.imp, feats.sizeLU, feats.specific, feats.specific.imp)
