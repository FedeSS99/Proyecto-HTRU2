#################
# Configuration #
#################

# Libraries
library(MASS)
library(psych)
library(gridExtra)
library(ggplot2)
library(CCA)
library(caret)
library(klaR)
library(microbenchmark)



# Routes
rute_data = './data/'
rute_img = './images/'
rute_code = './code/'

# Extract Transform and Load data (ETL)
source(paste0(rute_code, "ETL.R"))

# EDA
source(paste0(rute_code, "EDA.R"))

# Base model (Logistic Regression)
source(paste0(rute_code, "LR.R"))

# LDA & QDA
source(paste0(rute_code, "LDA_QDA.R"))

# PCA
source(paste0(rute_code, "PCA.R"))

# FA
source(paste0(rute_code, "FA.R"))

# Results
source(paste0(rute_code, "RESULTS.R"))