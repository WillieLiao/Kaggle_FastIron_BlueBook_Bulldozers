# install.packages('plyr')
# install.packages('ggplot2')
# install.packages('reshape2')
# install.packages('lubridate')
# install.packages('imputation')
# install.packages('rpart')
# install.packages('gbm')
# install.packages('parallel')

library(plyr)
library(ggplot2)
library(reshape2)
library(lubridate)
library(imputation)
library(rpart)
library(gbm)
library(doParallel)

# data paths and files
TRAIN_DATA_PATH <- 'Data/'
TEST_DATA_PATH <- 'Data/' # not used...
MODEL_PATH <- 'Model/' # not used...
SUBMISSION_PATH <- 'Submission/'

urlPC <- "ftp://ftp.bls.gov/pub/time.series/pc/pc.data.19.Machinery"
urlWP <- "ftp://ftp.bls.gov/pub/time.series/wp/wp.data.12a.Machinery11-113"

train_zip <- "TrainAndValid.zip"
test_zip <- "Test.zip"
train_csv <- "TrainAndValid.csv"
test_csv <- "Test.csv"
cpi_csv <- "CUUR0000SETA02.csv"
ppi_industry_txt <- "PPI_Industry.txt"
ppi_commodity_txt <- "PPI_Commodity.txt"

