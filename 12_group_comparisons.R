####========================================= A.L.R.R.2020 - 2021
### DESCRIPTION
## This script performs group comparisons between the ROI Z-
## ...transformed correlations


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
setwd(paste('/Users/lmuresearchfellowship/Documents/',
  'Adriana/LMUFellowship/Projects/Goal_A/',
  sep = ""))


####==========================================================
### MAIN FILE
## Read text files and create temporal data frames for
## ...correlation

# "Total" file in wide format
if (!exists('total')){
  total <- read.csv("ROI-FC-all.csv",
           header = T, row.names = 1)
}

# Adjust total if retrieved from read.csv
total$filename <- factor(substr(total$filename, 1, 12))
total$is_SCD <- factor(total$is_SCD)
levels(total$is_SCD)["SCD"] <- "SCD"
levels(total$is_SCD)["CON"] <- "CON"
levels(total$is_SCD)["MCI"] <- "MCI"
total$is_SCD <- factor(total$is_SCD,
                       levels = c("MCI", "SCD", "CON"))


####==========================================================
### TIME POINT FILES

## "Total" file in wide format separately for each time point
  # Baseline (T0)
total_t0 <- total[which(total$timepoint==1),]
  rownames(total_t0) <- NULL

  # T1
total_t1 <- total[which(total$timepoint==2),]
  rownames(total_t1) <- NULL
  levels(total_t1$is_SCD)["SCD"] <- "SCD"
  levels(total_t1$is_SCD)["CON"] <- "CON"
  total_t1$is_SCD <- factor(total_t1$is_SCD,
                            levels = c("SCD", "CON"))
  
  # T2
total_t2 <- total[which(total$timepoint==3),]
  rownames(total_t2) <- NULL
  levels(total_t2$is_SCD)["SCD"] <- "SCD"
  levels(total_t2$is_SCD)["CON"] <- "CON"
  total_t2$is_SCD <- factor(total_t2$is_SCD,
                            levels = c("SCD", "CON"))

## Adjust column names for each time point file
  # T0
colnames(total_t0)[4:14] <- paste(
  colnames(total_t0)[4:14], 1, sep = "_")

  # T1
colnames(total_t1)[4:14] <- paste(
  colnames(total_t1)[4:14], 2, sep = "_")

  # T2
colnames(total_t2)[4:14] <- paste(
  colnames(total_t2)[4:14], 3, sep = "_")

## Delete "timepoint" column (column 3)
  # T0
total_t0 <- total_t0[, -which(
  colnames(total_t0)=="timepoint")]

  # T1
total_t1 <- total_t1[, -which(
  colnames(total_t1)=="timepoint")]

  # T2
total_t2 <- total_t2[, -which(
  colnames(total_t2)=="timepoint")]

# Delete MCI participants from T0 for 3-way mixed ANOVA
total_t0 <- data.frame(total_t0[-which(
  total_t0$is_SCD=='MCI'),])
total_t0$is_SCD <- factor(total_t0$is_SCD,
                            levels = c("SCD", "CON"))
rownames(total_t0) <- NULL

## Set "filename" column to having the same name across...
# ...data frames
total_t0$filename <- substr(total_t0$filename, 1, 12)
total_t1$filename <- substr(total_t1$filename, 1, 12)
total_t2$filename <- substr(total_t2$filename, 1, 12)


####==========================================================
### WIDE FORMAT

## Merge data frames according to file names (2 steps)
temp <- merge(data.frame(total_t0, row.names=NULL),
               data.frame(total_t1, row.names=NULL),
               by = "filename",
               all = TRUE)

total_wide <- merge(data.frame(temp, row.names=NULL),
              data.frame(total_t2, row.names=NULL),
              by = "filename",
              all = TRUE)

## Adjust column names, especially to remove duplicates
total_wide <- total_wide[, -which(
  grepl(".y", colnames(total_wide))==TRUE)]

colnames(total_wide)[which(
  colnames(total_wide)=="is_SCD.x")] <- "is_SCD"

total_wide <- total_wide[, -which(
  duplicated(colnames(total_wide))==TRUE)]

## Create one separate data frame for the average FC
total_wide_fc_sn <- total_wide[, c(1, 2, which(
  grepl("SN_FC_", colnames(total_wide))==TRUE))]

# Delete the average FC columns from "total_wide"
total_wide <- total_wide[, -which(
  grepl("SN_FC_", colnames(total_wide))==TRUE)]

rm(temp)


####==========================================================
### THREE-WAY MIXED ANOVA ACROSS ROIS
### WS: time point and ROI / BS: SCD

## Prepare the data
  # Convert data frame to long format
total_all_long <- pivot_longer(
  total_wide,
  names_to = "ROI_pair",
  cols = LINS_LIFG_1:RINS_RIFG_3,
  values_to = "Z_FC")

  # Make some adjustments to the new data frame
total_all_long <- data.frame(total_all_long)
total_all_long$filename <- factor(total_all_long$filename)

  # Create a column for time point based on "ROI_pairs"
total_all_long$timepoint <- "0"
total_all_long$timepoint[which(
  grepl("1", total_all_long$ROI_pair)==T)] <- "1"
total_all_long$timepoint[which(
  grepl("2", total_all_long$ROI_pair)==T)] <- "2"
total_all_long$timepoint[which(
  grepl("3", total_all_long$ROI_pair)==T)] <- "3"

  # Delete the number from the ROI pairs
total_all_long$ROI_pair <- sub("_[0-9]", "",
                               total_all_long$ROI_pair)
total_all_long$ROI_pair <- factor(total_all_long$ROI_pair)

## Actual ANOVA
res.aov.total_all_long <- anova_test(data = total_all_long,
                               dv = Z_FC,
                               wid = filename,
                               between = is_SCD,
                               within = c(timepoint,
                                          ROI_pair),
                               effect.size = "pes")
get_anova_table(res.aov.total_all_long, correction = "auto")


####==========================================================
### TWO-WAY MIXED ANOVA AVERAGE FC
### WS: time point / BS: SCD (same as the previous section)

## Prepare the data
# Convert data frame to long format
total_long_fc_sn <- pivot_longer(
  total_wide_fc_sn,
  names_to = "timepoint",
  cols = SN_FC_1:SN_FC_3,
  values_to = "Z_FC")

# Make some adjustments to the new data frame
total_long_fc_sn <- data.frame(total_long_fc_sn)
total_long_fc_sn$filename <- factor(total_long_fc_sn$filename)

# Create a column for time point
total_long_fc_sn$timepoint <- as.factor(substr(total_long_fc_sn$timepoint,
                                               7, 7))

## Actual ANOVA
res.aov.total_long_fc_sn <- anova_test(data = total_long_fc_sn,
                                     dv = Z_FC,
                                     wid = filename,
                                     between = is_SCD,
                                     within = timepoint,
                                     effect.size = "pes")
get_anova_table(res.aov.total_long_fc_sn, correction = "auto")


####==========================================================
### DATA PREPARATION FOR SEPARATE MIXED ANOVAS
### ...WITHIN EACH TIME POINT

## Transform data frame for mixed model
# Total with all time points
total_long <- pivot_longer(total, names_to = "ROI_pair",
                           cols = LINS_LIFG:RINS_RIFG,
                           values_to = "Z_FC")
total_long <- data.frame(total_long)
total_long$filename <- factor(
  substr(total_long$filename, 1, 12))
total_long$ROI_pair <- factor(total_long$ROI_pair)
total_long$timepoint <- factor(total_long$timepoint)

# Baseline / T0
total_long_t0_all <- data.frame(total_long[which(
  total_long$timepoint==1), ])
total_long_t0$filename <- factor(
  total_long_t0$filename)
total_long_t0$ROI_pair <- factor(
  total_long_t0$ROI_pair)

# Only SCD and CON
total_long_t0 <- total_long[which(total_long$timepoint==1 &
                                    total_long$is_SCD!="MCI"), ]
total_long_t0$is_SCD <- factor(total_long_t0$is_SCD,
                               levels = c("SCD", "CON"))

# T1
total_long_t1 <- data.frame(total_long[which(
  total_long$timepoint==2), ])
total_long_t1$is_SCD <- factor(
  total_long_t1$is_SCD,
  levels = c("SCD", "CON"))
total_long_t1$filename <- factor(
  total_long_t1$filename)
total_long_t1$ROI_pair <- factor(
  total_long_t1$ROI_pair)

# T2
total_long_t2 <- data.frame(total_long[which(
  total_long$timepoint==3), ])
total_long_t2$is_SCD <- factor(
  total_long_t2$is_SCD,
  levels = c("SCD", "CON"))
total_long_t2$filename <- factor(
  total_long_t2$filename)
total_long_t2$ROI_pair <- factor(
  total_long_t2$ROI_pair)


####==========================================================
### ASSUMPTION CHECK FOR MIXED SEPARATE ANOVAs

## Identify outliers
  # Baseline
total_long_t0 %>% group_by(ROI_pair, is_SCD) %>%
  identify_outliers(Z_FC)

  # T1
total_long_t1 %>% group_by(ROI_pair, is_SCD) %>%
  identify_outliers(Z_FC)

  # T2
total_long_t2 %>% group_by(ROI_pair, is_SCD) %>%
  identify_outliers(Z_FC)

## Normality tests
  # Baseline
shapiro.test(total_long_t0$Z_FC)
qqnorm(total_long_t0$Z_FC)

  # T1
shapiro.test(total_long_t1$Z_FC)
qqnorm(total_long_t1$Z_FC)

  # T2
shapiro.test(total_long_t2$Z_FC)
qqnorm(total_long_t2$Z_FC)

## Homogeneity of variance
  # Baseline
levene_test(total_long_t0, Z_FC~is_SCD*ROI_pair)

  # T1
levene_test(total_long_t1, Z_FC~is_SCD*ROI_pair)

  # T2
levene_test(total_long_t2, Z_FC~is_SCD*ROI_pair)


####==========================================================
### MIXED ANOVAS FOR EACH TIME POINT SEPARATELY
## Actual mixed ANOVAs

# Baseline
  # If removing MCI participants is wanted:
total_long <- data.frame(total_long[-which(
  total_long$is_SCD=='MCI'),])
total_long$is_SCD <- factor(total_long$is_SCD,
                            levels = c("SCD", "CON"))
rownames(total_long) <- NULL

res.aov.baseline <- anova_test(data = total_long_t0,
                             dv = Z_FC,
                             wid = filename,
                             between = is_SCD,
                             within = ROI_pair,
                             effect.size = "pes")
get_anova_table(res.aov.baseline, correction = "auto")
write.table(res.aov.baseline$ANOVA,
     file = "/cloud/project/figures/baseline_ANOVA.txt",
     quote = F, sep = " | ", row.names = F)

# T1
res.aov.t1 <- anova_test(data = total_long_t1,
                               dv = Z_FC,
                               wid = filename,
                               between = is_SCD,
                               within = ROI_pair,
                               effect.size = "pes")
get_anova_table(res.aov.t1, correction = "auto")
write.table(res.aov.t1$ANOVA,
            file = "/cloud/project/figures/t1_ANOVA.txt",
            quote = F, sep = " | ", row.names = F)

# T2
res.aov.t2 <- anova_test(data = total_long_t2,
                         dv = Z_FC,
                         wid = filename,
                         between = is_SCD,
                         within = ROI_pair,
                         effect.size = "pes")
get_anova_table(res.aov.t2, correction = "auto")
write.table(res.aov.t2$ANOVA,
            file = "/cloud/project/figures/t2_ANOVA.txt",
            quote = F, sep = " | ", row.names = F)


####==========================================================
### FOLLOW-UP ONE-WAY ANOVAS
## One-way ANOVA to study significant interaction effects

# Within each WS group, comparing between subjects (BS)
bs <- total_long_t0 %>%
  group_by(ROI_pair) %>%
  anova_test(dv = Z_FC, wid = filename,
             between = is_SCD) %>%
  get_anova_table() %>% adjust_pvalue(
    method = "bonferroni")
write.csv(bs,
            file = paste("/cloud/project/figures/",
            "oneway_baseline_ANOVA.csv"),
            quote = F, row.names = F)

## Within each BS group, comparing within subjects 
ws <- total_long_t2 %>%
  group_by(is_SCD) %>%
  anova_test(dv = Z_FC, wid = filename,
             within = ROI_pair, effect.size = "pes") %>%
  get_anova_table() %>% adjust_pvalue(
    method = "bonferroni")

## Pairwise t-tests
pwc <- total_long_t2 %>% 
  pairwise_t_test(
    Z_FC ~ ROI_pair, pool.sd = FALSE,
    p.adjust.method = "bonferroni"
  )
pwc
