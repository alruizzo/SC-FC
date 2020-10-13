####========================================= A.L.R.R.2020 - 2021
### DESCRIPTION
## This script performs correlations between ROI time courses
## ...derived from 'meants' as well as 'dual regression'


####==========================================================
### INSTALL PACKAGES
#install.packages("pacman")
#require(pacman)
# pacman::p_load(ggplot2, dplyr, ggthemes, ggvis, plotly, rio,
#                stringr, tidyr, readxl, xlsx, ggpubr, psych,
#                car, tidyverse, rstatix, cocor, ppcor,
#                RColorBrewer, Hmisc, DescTools)


####==========================================================
### WORKING DIRECTORY
setwd('/cloud/project/timecourses/')


####==========================================================
### REQUIRED FILES
## Read text files and create temporal data frames for
## ...correlation

# read file content
filenames <- list.files()


####==========================================================
### DATA FRAME CREATION
## Create a "total" file after having extracted appropriate
## ...info

# create data frame
total <- data.frame(filename="txt",
                    is_SCD="MCI",
                    timepoint=0,
                    #age=100,
                    #sex="X",
                    #MMSE=99,
                    LINS_LIFG=99,
                    ACC_LIFG=99,
                    RIFG_LIFG=99,
                    RINS_LIFG=99,
                    ACC_LINS=99,
                    RIFG_LINS=99,
                    RINS_LINS=99,
                    RIFG_ACC=99,
                    RINS_ACC=99,
                    RINS_RIFG=99)[1:length(filenames), ]
#adjust row names
rownames(total) <- NULL

# populate that data frame by creating individual ones
for (i in filenames) {
  temp <- read.table(i, header=T)
  #exclude cerebellum ROIs:
  temp <- temp[, -grep("CBL_gm.txt", colnames(temp))]
  #exclude WM and CSF:
  temp <- temp[, grep("_gm.txt", colnames(temp))]
  #variance normalize time series:
  temp <- data.frame(scale(temp))
  #correlation matrix:
  temp_mat <- data.frame(cor(temp))
  Ztemp_mat <- FisherZ(temp_mat) #Fisher z transformation
  #delete one side of diagonal:
  Ztemp_mat[upper.tri(Ztemp_mat)] <- 0
  #convert Inf values (diag) to zero:
  Ztemp_mat[!lower.tri(Ztemp_mat)] <- 0
  for (j in names(Ztemp_mat)) { #select column to delete
    #message(j)
    y <- ifelse(length(IsZero(
      Ztemp_mat[[j]]))==nrow(Ztemp_mat), j, 0)
  }
  rm(j)
  #reduce Z matrix:
  Ztemp_mat <- Ztemp_mat[-which(
    IsZero(Ztemp_mat[1,1])==TRUE), -which(
      colnames(Ztemp_mat)==y)]
  rm(y)
  #pass info on to a "total", summary data frame:
  total$filename[match(i, filenames)] <- i
  for (roicolumn in names(Ztemp_mat)) {
    roinumber <- grep(roicolumn, colnames(Ztemp_mat))
    if (grepl("LIFG", roicolumn)) {
    total$LINS_LIFG[which(
      total$filename==i)] <- Ztemp_mat[grep(
        "LINS", rownames(Ztemp_mat)), roinumber]
    total$ACC_LIFG[which(
      total$filename==i)] <- Ztemp_mat[grep(
        "ACC", rownames(Ztemp_mat)), roinumber]
    total$RIFG_LIFG[which(
      total$filename==i)] <- Ztemp_mat[grep(
        "RIFG", rownames(Ztemp_mat)), roinumber]
    total$RINS_LIFG[which(
      total$filename==i)] <- Ztemp_mat[grep(
        "RINS", rownames(Ztemp_mat)), roinumber]
    } else if (grepl("LINS", roicolumn)) {
      total$ACC_LINS[which(
        total$filename==i)] <- Ztemp_mat[grep(
          "ACC", rownames(Ztemp_mat)), roinumber]
      total$RIFG_LINS[which(
        total$filename==i)] <- Ztemp_mat[grep(
          "RIFG", rownames(Ztemp_mat)), roinumber]
      total$RINS_LINS[which(
        total$filename==i)] <- Ztemp_mat[grep(
          "RINS", rownames(Ztemp_mat)), roinumber]
    } else if (grepl("ACC", roicolumn)) {
      total$RIFG_ACC[which(
        total$filename==i)] <- Ztemp_mat[grep(
          "RIFG", rownames(Ztemp_mat)), roinumber]
      total$RINS_ACC[which(
        total$filename==i)] <- Ztemp_mat[grep(
          "RINS", rownames(Ztemp_mat)), roinumber]
    } else {
      total$RINS_RIFG[which(
        total$filename==i)] <- Ztemp_mat[grep(
          "RINS", rownames(Ztemp_mat)), roinumber]
    }
  }
  #delete the ".txt" from filename
  total$filename[which(total$filename==i)] <- substr(i,1,15)
}

# populate SCD status from filename in "total"
total$is_SCD <- ifelse(grepl(
  'ci', total$filename), "SCD", "CON")
total$is_SCD[grepl('mci', total$filename)] <- "MCI"
total$is_SCD <- factor(total$is_SCD,
                       levels = c("MCI", "SCD", "CON"))

# populate time point from filename in "total"
total$timepoint <- as.numeric(substring(
  total$filename, 15, 15))
total$timepoint <- factor(total$timepoint,
                          levels = c(1, 2, 3))

# fill in cells to replace NAs
#total$age <- 100
#total$sex <- "X"
#total$MMSE <- 99

# clean work space
rm(list = c("roicolumn",
            "roinumber",
            "i", "temp",
            "temp_mat",
            "Ztemp_mat"))

# round up figures in ROI-to-ROI variables
total[, 4:ncol(total)] <- round(total[, 4:ncol(total)], 2)

# Average FC
total$SN_FC <- (total$LINS_LIFG +
                  total$ACC_LIFG + total$RIFG_LIFG +
                  total$RINS_LIFG + total$ACC_LINS +
                  total$RIFG_LINS + total$RINS_LINS +
                  total$RIFG_ACC + total$RINS_ACC +
                  total$RINS_RIFG) /10

# save to txt
write.csv(total, file = "/cloud/project/ROI-FC-all.csv")
