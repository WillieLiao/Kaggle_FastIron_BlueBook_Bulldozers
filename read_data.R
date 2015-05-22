# source('settings.R')
# source('util.R')
##########################################################
# read data
##########################################################
# Descriptions are not needed in modelling
drops <- c("fiProductClassDesc",'ProductGroupDesc','fiManufacturerDesc', 'PrimarySizeBasis')

mac <- read.csv(paste0(TRAIN_DATA_PATH, "Machine_Appendix.csv"), as.is=T, strip.white=T)
mac <- mac[,!(names(mac) %in% drops)]
mac[mac=="None or Unspecified"] <- ""

unzip(paste0(TRAIN_DATA_PATH, train_zip), exdir=gsub("/", "", TRAIN_DATA_PATH))
unzip(paste0(TRAIN_DATA_PATH, test_zip), exdir=gsub("/", "", TRAIN_DATA_PATH))

train <- read.csv(paste0(TRAIN_DATA_PATH, train_csv), as.is=T, strip.white=T)
test <- read.csv(paste0(TEST_DATA_PATH, test_csv), as.is=T, strip.white=T)

SalePrice <- train[, c("SalesID", "SalePrice")]
train$SalePrice <- c()
tt <- rbind(train, test)
# tt <- train 
rm(train, test)

tt <- tt[,!(names(tt) %in% drops)]
tt[tt=="None or Unspecified"] <- ""
tt$saledate <- as.Date(tt$saledate, "%m/%d/%Y")
tt$Year <- format(tt$saledate, "%Y")
tt$Month <- format(tt$saledate, "%m")

names(mac) <- c('MacID','ModelID','MKey','MBase','MSecond','MSeries','MDesc','ProdGp','YearMade','MfgID','SizeL','SizeU')
names(tt) <- c('SalesID','MacID','ModelID', 'Source','AucID', 'YearMade', 'Hours','UseBand','SaleDate', 'MKey', 'MBase', 'MSecond', 'MSeries', 'MDesc', 'Size','State','ProdGp','DriveSys','Enc','Forks','Pad','Ride','Stick','Trans','Turbo','Blade','BladeW','EncType','HP','Hydraulics','Pushblock','Ripper','Scarifier','Tip','Tire','Coupler','CouplerSys','Grouser','HydraFlow','Track','UnderW','StickL','Thumb','Pattern','GrouserType','Backhoe','BladeType','Travel','Differ','Steering','Year','Month')
rm(drops)
save(tt, mac, SalePrice, file=paste0(TRAIN_DATA_PATH, "All-Data-Before-Preprocess.rda"))
save(SalePrice, file=paste0(TRAIN_DATA_PATH, "SalePrice.rda"))

##########################################################
# Create CPI tables
##########################################################
# Include various price indices and let ML models use if they want.
# Take logs of both SalePrice and cpi so that models will look at ratios.

# Consumer Price Index for All Urban Consumers: All Items (CPIAUCNS)
# http://research.stlouisfed.org/fred2/series/CPIAUCNS/downloaddata?rid=10&soid=22
# 2013-01: 230.280 available 2013-02-01
cpi <- read.csv(paste0(TRAIN_DATA_PATH, "CPIAUCNS.csv"), as.is=T)

# Consumer Price Index for All Urban Consumers: Used cars and trucks (CUUR0000SETA02)
# http://research.stlouisfed.org/fred2/series/CUUR0000SETA02/downloaddata?rid=10&soid=22
# http://research.stlouisfed.org/fred2/series/CUUR0000SETA02/downloaddata?cid=32418
# 2013-01: 145.260 available on 2013-02-21
cpi2 <- read.csv(paste0(TRAIN_DATA_PATH, cpi_csv), as.is=T)
cpi <- rbind(data.frame(series=rep("CPIAUCNS", nrow(cpi)), cpi),
             data.frame(series=rep("CUUR0000SETA02", nrow(cpi2)), cpi2))
rm(cpi2)
names(cpi) <- c("series", "date", "val")
cpi$Year <- substr(cpi$date, 1, 4)
cpi$Month <- substr(cpi$date, 6, 7)
cpi <- cpi[, c(1, 4, 5, 3)]

# Producer Price Index
download.file(urlPC, paste0(TRAIN_DATA_PATH, ppi_industry_txt))
download.file(urlWP, paste0(TRAIN_DATA_PATH, ppi_commodity_txt))
pc <- read.delim(paste0(TRAIN_DATA_PATH, ppi_industry_txt), as.is=T, strip.white=T)
wp <- read.delim(paste0(TRAIN_DATA_PATH, ppi_commodity_txt), as.is=T, strip.white=T)
names(pc) <- c("series", "Year", "Month", "val", "foot")
names(wp) <- c("series", "Year", "Month", "val", "foot")
pc <- pc[grepl("^PCU3331(20|11)3331(20|11)$", pc$series) & pc$Year>=1988 & pc$Month!="M13",1:4]
wp <- wp[grepl("^WPU11[12]$", wp$series) & wp$Year>=1988 & wp$Month!="M13",1:4]
ppi <- rbind(pc, wp)
rm(pc, wp)
ppi$Month <- substr(ppi$Month, 2, 3)

cpi <- rbind(cpi, ppi)
rm(ppi)
cpi <- cpi[!(cpi$Year==1988 & as.numeric(cpi$Month)<10), ]

# reshape cpi and add a 2 month lag to data
cpi$date <- ymd(paste(cpi$Year, cpi$Month, '01', sep='-'))
cpi.lag <- 2
cpi$Year <- format(as.POSIXlt(cpi$date) + months(cpi.lag), "%Y")
cpi$Month <- format(as.POSIXlt(cpi$date) + months(cpi.lag), "%m")


cpi <- dcast(cpi, Year + Month + date ~ series, value.var="val")
save(cpi, file=paste0(TRAIN_DATA_PATH, "cpi.rda"))
rm(urlPC, urlWP, cpi.lag)