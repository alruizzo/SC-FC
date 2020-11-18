####========================================= A.L.R.R.2020 - 2021
### DESCRIPTION
## This script performs correlations between ROI time courses
## ...derived from FSL's meants'


####==========================================================
### INSTALL PACKAGES
## The package 'psych' is especially necessary for the...
## ...Fisher-Z function
# install.packages("pacman")
# require(pacman)
# pacman::p_load(ggplot2, dplyr, ggthemes, ggvis, plotly,
#                 rio, stringr, tidyr, readxl, ggpubr,
#                 psych, car, tidyverse, rstatix, R.utils)


####==========================================================
### WORKING DIRECTORY
setwd(paste('~/Documents/Adriana/LMUFellowship/Projects/',
            'Goal_A/', sep=""))


####==========================================================
### REQUIRED FILES
## Read text files and create temporal data frames for
## ...correlation

# read file content all ROIs
setwd('./timecourses')
filenames <- list.files()


####==========================================================
### DATA FRAME CREATION
## Create a "total" file after having extracted appropriate
## ...info: Anterior Salience Network ROIs

# create data frame
total <- data.frame(filename="txt",
                    is_SCD="MCI",
                    timepoint=0,
                    LPIN_RPIN=99,
                    LPIN_LINS=99,
                    LPIN_LIFG=99,
                    LPIN_ACC=99,
                    LPIN_RIFG=99,
                    LPIN_RINS=99,
                    RPIN_LINS=99,
                    RPIN_LIFG=99,
                    RPIN_ACC=99,
                    RPIN_RIFG=99,
                    RPIN_RINS=99)[1:length(filenames), ]
# adjust row names
rownames(total) <- NULL

# create ROI list
roi_list <- c("LINS", "LIFG", "ACC", "RIFG", "RINS")
con_roi <- c("LPIN", "RPIN")

# create colnames list
total_col_names <- colnames(total)[4:length(total)]

# populate that data frame by creating individual ones
for (i in filenames) {
  temp <- read.table(i, header=T)
  #add control ROIs
  temp1 <- read.table(paste('.././timecourses_con/',
                            i, sep=""), header=T)
  #change colname of con rois
  substr(colnames(temp1)[1], 12, 15) <- "LPIN"
  substr(colnames(temp1)[2], 12, 15) <- "RPIN"
  #merge the two temp data frames
  temp <- cbind(temp, temp1)
  #delete temp1
  rm(temp1)
  #variance normalize time series:
  temp <- data.frame(scale(temp))
  #correlation matrix:
  temp_mat <- data.frame(cor(temp))
  Ztemp_mat <- fisherz(temp_mat) #Fisher z transformation
  #delete one side of diagonal:
  Ztemp_mat[upper.tri(Ztemp_mat)] <- 0
  #convert Inf values (diag) to zero:
  Ztemp_mat[!lower.tri(Ztemp_mat)] <- 0
  for (j in names(Ztemp_mat)) { #select column to delete
    #message(j)
    y <- ifelse(length(isZero(
      Ztemp_mat[[j]]))==nrow(Ztemp_mat), j, 0)
  }
  rm(j)
  #reduce Z matrix:
  Ztemp_mat <- Ztemp_mat[-which(
    isZero(Ztemp_mat[1,1])==TRUE), -which(
      colnames(Ztemp_mat)==y)]
  rm(y)
  #pass info on to a "total", summary data frame:
  total$filename[match(i, filenames)] <- i
  for (con in con_roi) {
    for (roi in roi_list) {
      total[which(total$filename==i),
                which(colnames(total)==paste(con, "_",
                                             roi,
                                             sep = ""))] <-
            Ztemp_mat[grep(con, rownames(Ztemp_mat)),
                      grep(roi, colnames(Ztemp_mat))]
      }
    }
  total[which(total$filename==i),
        which(colnames(total)==paste(con_roi[1], "_",
                                     con_roi[2],
                                     sep = ""))] <-
    Ztemp_mat[grep(con_roi[2], rownames(Ztemp_mat)),
              grep(con_roi[1], colnames(Ztemp_mat))]
  }
  #delete the ".txt" from filename
  total$filename[which(total$filename==i)] <- substr(i,1,15)

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

# clean work space
rm(list = c("i", "temp",
            "temp_mat",
            "Ztemp_mat"))

# round up figures in ROI-to-ROI variables
total[, 4:ncol(total)] <- round(total[, 4:ncol(total)], 2)

# save to txt
write.csv(total, file = "../CON-ROI-FC-all.csv")
