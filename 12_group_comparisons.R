####========================================= A.L.R.R.2020 - 2021
### DESCRIPTION
## This script performs group comparisons between the ROI Z-
## ...transformed correlations


####==========================================================
### INSTALL PACKAGES
# install.packages("pacman")
# require(pacman)
# pacman::p_load(ggplot2, dplyr, ggthemes, ggvis, plotly,
#                rio, stringr, tidyr, readxl, ggpubr,
#                psych, car, tidyverse, rstatix, cocor,
#                ppcor, RColorBrewer, Hmisc, DescTools,
#                permuco, CorrMixed)


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

  # Get information on participants to be excluded
if (!exists('exclude')){
  exclude <- data.frame(read.csv(
    "Partic_excluded.txt",
    sep = "\t"))
}

  # Revised reasons for exclusion after discussion with...
  # ...other members of the lab
    # Create column
exclude$revise <- "exclude"
    # Specific reasons after inquiring
exclude$revise[which(exclude$participants=="wsu-con-0004")] <-
  "included in similar project"
exclude$revise[which(exclude$participants=="wsu-con-0024")] <-
  "no obvious effect on rs-fMRI"
exclude$revise[which(exclude$participants=="wsu-con-0025")] <-
  "scored normal in other projects"
exclude$revise[which(exclude$participants=="wsu-con-0030")] <-
  "normal MMSE T1 and T2"
exclude$revise[which(exclude$participants=="wsu-con-0033")] <-
  "no obvious motion in rs-fMRI"
exclude$revise[which(exclude$participants=="wsu-con-0035")] <-
  "scored normal in other projects"
exclude$revise[which(exclude$participants=="wsu-sci-0012")] <-
  "scored normal in other projects"
exclude$revise[which(exclude$participants=="wsu-sci-0015")] <-
  "~75% out of system for fMRI"
exclude$revise[which(exclude$participants=="wsu-sci-0017")] <-
  "Not good enough reason for exclusion"
exclude$revise[which(exclude$participants=="wsu-sci-0019")] <-
  "scored normal in other projects"
exclude$revise[which(exclude$participants=="wsu-sci-0022")] <-
  "overall normal"

  # Create character variable for next step
Excluded <- exclude$participants[which(exclude$revise=="exclude")]


####==========================================================
# ### PARTICIPANT SELECTION (ONE TIME STEP), hence commented
# ## Add demographic information to determine participant...
# ## ...exclusion. Note: there might be other important...
# ## ...sources of information, so this information needs...
# ## ...to be first checked with other folks in the lab.
# ## Skip if "Excluded" exists
# 
#   # Get the demographics file or data frame if existent
# if (!exists('demographics_t0')){
#   demographics_t0 <- data.frame(read_excel(
#     "General/Demographics.xlsx"))
# }
# 
#   # Create a participant list to extract info from the...
#   # ...demographics data frame (at baseline!)
# part_list <- as.character(unique(total$filename))
# 
#   # Make file name in demographics equal to that in total
# demographics_t0$ParticipantID <- gsub("_", "-",
#                                    demographics_t0$ParticipantID)
# 
#   # Subset the demographics data frame to include only those
#   # ...for whom data are available
# demographics_t0 <- demographics_t0[
#   demographics_t0$ParticipantID %in% part_list ,]
# 
#   # Identify possible participants to exclude according to:
#     # MMSE <= 25
# MMSE <- demographics_t0$ParticipantID[which(
#   demographics_t0$Mini.Mental.State.Examination.Total.Score....<=25
#   )]
# 
#     # Age <= 50
# Age <- demographics_t0$ParticipantID[which(
#   demographics_t0$Age<=50)]
# 
#   # Read the file where I wrote the exclusion info from...
#   # ...other sources (i.e., files in the lab)
# exclude <- data.frame(t(data.frame(read.csv("exclude.txt",
#                                             header = T,
#                                             sep = ""))))
#     # Add column with participants' file names
# exclude$participants <- row.names(exclude)
# exclude <- exclude[c(order(exclude$participants)),]
# row.names(exclude) <- NULL
# 
#     # Rename participants' file names to match other...
#     # ...data frames (didn't find a more elegant way...
#     # ...to do it so far)
# exclude$participants <- gsub("u.", "u-",
#                              exclude$participants)
# exclude$participants <- gsub("n.", "n-",
#                              exclude$participants)
# exclude$participants <- gsub("i.", "i-",
#                              exclude$participants)
# 
#     # Organize this data frame
# colnames(exclude)[1] <- "reason"
# exclude <- exclude[, c("participants", "reason")]
# 
#   # Add information extracted from the Demographics...
#   # ...file
# exclude$demographics_t0 <- ""
# exclude$demographics_t0[exclude$participants %in% Age] <-
#   "Age"
# exclude$demographics_t0[exclude$participants %in% MMSE] <-
#   "MMSE"
# 
#   # Clean work space
# rm(list = c("Age", "MMSE", "part_list"))
# 
#   # Save list of possibly to-be-excluded participants...
#   # ...for the ensuing analyses
# Excluded <- exclude$participants
# write.table(exclude,
#             file = "Partic_excluded.txt",
#             quote = F, sep = "\t", row.names = F)


####==========================================================
### ROI-PAIR SELECTION
## Leave in the columns corresponding to the ROI-pairs of...
## ...interest (all involving the ACC): ACC-LIFG; ACC-RIFG;...
## ... ACC-LINS; ACC-RINS.

# Save copy of all ROI pairs
total_allrois <- total

# Get only the ROI-pairs of interest
total <- total[, c(1, 2, 3, grep("ACC", colnames(total)))]

# Obtain average FC for these pairs
total$SN_FC <- round(rowMeans(subset(total,
                               select = c(ACC_LIFG,
                                          ACC_LINS,
                                          RIFG_ACC,
                                          RINS_ACC)),
                        na.rm = TRUE), 2)

# Control ROIs
if (!exists('totalcon')){
  totalcon <- read.csv("CON-ROI-FC-all.csv",
                    header = T, row.names = 1)
}

# Check if the two data frames have equal rows and...
# ...leave only the control ROI-pairs of interest...
# ...i.e., those involving the posterior insula and...
# ...the ACC:
if (is_empty(which(
  substr(totalcon$filename, 1, 12)!=total$filename))){
  totalcon <- totalcon[, grep("ACC", colnames(totalcon))]
}

# Add the control pairs to the total data frame:
total <- cbind(total, totalcon)

# Adjust total columns to have SN-FC at the end
total <- total[, c(1:7, 9:10, 8)]


####==========================================================
### PARTICIPANT EXCLUSION
## Exclude those participants from total

## Total with complete resting-state data only
total_orig <- total
total <- total_orig[-which(
  total_orig$filename %in% Excluded==TRUE),]
rownames(total) <- NULL

## OR Total with complete diffusion AND resting-state data
## Choose only one option, otherwise, you'll have different...
## ...number of participants!
total <- total[-c(
  which(total$filename == "wsu-con-0001" & total$timepoint == "1"),
  which(total$filename == "wsu-con-0002" & total$timepoint == "1"),
  which(total$filename == "wsu-con-0003" & total$timepoint == "1"),
  which(total$filename == "wsu-sci-0001" & total$timepoint == "1"),
  which(total$filename == "wsu-sci-0002" & total$timepoint == "1"),
  which(total$filename == "wsu-sci-0003" & total$timepoint == "1"),
  which(total$filename == "wsu-sci-0004" & total$timepoint == "1"),
  which(total$filename == "wsu-sci-0007" & total$timepoint == "3")
  ),]

## Adjust total to excluding MCI
total_MCI <- total_orig[-which(
  total_orig$filename %in% Excluded==TRUE),]
total <- total[-which(total$is_SCD=="MCI"),]
total$is_SCD <- factor(total$is_SCD)
levels(total$is_SCD)["SCD"] <- "SCD"
levels(total$is_SCD)["CON"] <- "CON"
total$is_SCD <- factor(total$is_SCD,
                       levels = c("SCD", "CON"))
total$filename <- factor(total$filename)
total$timepoint <- factor(total$timepoint)


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
colnames(total_t0)[-c(1,2,3)] <- paste(
  colnames(total_t0)[-c(1,2,3)], 1, sep = "_")

  # T1
colnames(total_t1)[-c(1,2,3)] <- paste(
  colnames(total_t1)[-c(1,2,3)], 2, sep = "_")

  # T2
colnames(total_t2)[-c(1,2,3)] <- paste(
  colnames(total_t2)[-c(1,2,3)], 3, sep = "_")

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

# ## Set "filename" column to having the same name across...
# # ...data frames (ONE TIME STEP), hence, commented
# total_t0$filename <- substr(total_t0$filename, 1, 12)
# total_t1$filename <- substr(total_t1$filename, 1, 12)
# total_t2$filename <- substr(total_t2$filename, 1, 12)


####==========================================================
### WIDE FORMAT
## This format is necessary because of the way how I...
## ...exported the values and how they need to be...
## ...transformed in the next step (long format)

## Merge data frames according to file names (2 steps)
  # Create temporal file to merge first part
temp <- merge(data.frame(total_t0, row.names=NULL),
               data.frame(total_t1, row.names=NULL),
               by = "filename",
               all = TRUE)
  # Merge temporal file with second part
total_wide <- merge(data.frame(temp, row.names=NULL),
              data.frame(total_t2, row.names=NULL),
              by = "filename",
              all = TRUE)

## Adjust column names, especially to remove duplicates
  # Remove the ".y" part from the column names
total_wide <- total_wide[, -which(
  grepl(".y", colnames(total_wide))==TRUE)]

  # Remove the .x from the "is_SCD" variable
colnames(total_wide)[which(
  colnames(total_wide)=="is_SCD.x")] <- "is_SCD"
  
  # Remove duplicated variables
total_wide <- total_wide[, -which(
  duplicated(colnames(total_wide))==TRUE)]

## Create one separate data frame for the average FC
total_wide_fc_sn <- total_wide[, c(1, 2, which(
  grepl("SN_FC_", colnames(total_wide))==TRUE))]

## Delete the average FC columns from "total_wide"
total_wide <- total_wide[, -which(
  grepl("SN_FC_", colnames(total_wide))==TRUE)]

## Remove the previously-created "temp" to clean work space
rm(temp)


####==========================================================
### THREE-WAY MIXED ANOVA ACROSS ROIS
## WS: time point and ROI / BS: SCD
## DV: Z_FC

## Prepare the data
  # Convert data frame to long format
total_all_long <- pivot_longer(
  total_wide,
  names_to = "ROI_pair",
  cols = ACC_LIFG_1:RPIN_ACC_3,
  values_to = "Z_FC",
  values_drop_na = TRUE)

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
total_all_long$timepoint <- factor(total_all_long$timepoint)

  # Delete the number from the ROI pairs
total_all_long$ROI_pair <- sub("_[0-9]", "",
                               total_all_long$ROI_pair)
total_all_long$ROI_pair <- factor(
  total_all_long$ROI_pair,
  levels = c("ACC_LIFG", "RIFG_ACC",
             "ACC_LINS", "RINS_ACC",
             "LPIN_ACC", "RPIN_ACC"))

## Actual ANOVA
res.aov.total_all_long <- anova_test(data = total_all_long,
                               dv = Z_FC,
                               wid = filename,
                               between = is_SCD,
                               within = c(timepoint,
                                          ROI_pair),
                               effect.size = "pes")
get_anova_table(res.aov.total_all_long, correction = "auto")

  # ANOVA Permutations (if you want to compare)
res.aov.total_all_long_perm <- aovperm(
  formula = Z_FC ~ is_SCD * ROI_pair * timepoint
  + Error(filename/(timepoint*ROI_pair)),
          data = total_all_long,
  np = 10000)
res.aov.total_all_long_perm


####==========================================================
### TWO-WAY MIXED ANOVA AVERAGE FC
## WS: time point / BS: SCD (same as the previous section)
## DV: Z_FC

## Prepare the data

# Convert data frame to long format
total_long_fc_sn <- pivot_longer(
  total_wide_fc_sn,
  names_to = "timepoint",
  names_prefix = "SN_FC_",
  cols = SN_FC_1:SN_FC_3,
  values_to = "Z_FC",
  values_drop_na = TRUE)

# Make some adjustments to the new data frame
total_long_fc_sn <- data.frame(total_long_fc_sn)
total_long_fc_sn$filename <- factor(
  total_long_fc_sn$filename)
total_long_fc_sn$timepoint <- factor(
  total_long_fc_sn$timepoint)

## Actual ANOVA
res.aov.total_long_fc_sn <- anova_test(
  data = total_long_fc_sn,
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
total_long <- pivot_longer(total_MCI, names_to = "ROI_pair",
                           cols = ACC_LIFG:RPIN_ACC,
                           values_to = "Z_FC",
                           values_drop_na = TRUE)
total_long <- data.frame(total_long)
total_long$filename <- factor(
  substr(total_long$filename, 1, 12))
total_long$ROI_pair <- factor(total_long$ROI_pair)
total_long$timepoint <- factor(total_long$timepoint)

# Baseline / T0
  # All
total_long_t0_all <- data.frame(total_long[which(
  total_long$timepoint==1), ])
total_long_t0_all$filename <- factor(
  total_long_t0_all$filename)
total_long_t0_all$ROI_pair <- factor(
  total_long_t0_all$ROI_pair,
  levels = c("ACC_LIFG", "RIFG_ACC",
             "ACC_LINS", "RINS_ACC",
             "LPIN_ACC", "RPIN_ACC"))

  # Only SCD and CON
total_long_t0 <- total_long[which(total_long$timepoint==1 &
                                    total_long$is_SCD!="MCI"), ]
total_long_t0$is_SCD <- factor(total_long_t0$is_SCD,
                               levels = c("SCD", "CON"))
rownames(total_long_t0) <- NULL
total_long_t0$ROI_pair <- factor(
  total_long_t0$ROI_pair,
  levels = c("ACC_LIFG", "RIFG_ACC",
             "ACC_LINS", "RINS_ACC",
             "LPIN_ACC", "RPIN_ACC"))

# T1
total_long_t1 <- data.frame(total_long[which(
  total_long$timepoint==2), ])
total_long_t1$is_SCD <- factor(
  total_long_t1$is_SCD,
  levels = c("SCD", "CON"))
total_long_t1$filename <- factor(
  total_long_t1$filename)
total_long_t1$ROI_pair <- factor(
  total_long_t1$ROI_pair,
  levels = c("ACC_LIFG", "RIFG_ACC",
             "ACC_LINS", "RINS_ACC",
             "LPIN_ACC", "RPIN_ACC"))
rownames(total_long_t1) <- NULL

# T2
total_long_t2 <- data.frame(total_long[which(
  total_long$timepoint==3), ])
total_long_t2$is_SCD <- factor(
  total_long_t2$is_SCD,
  levels = c("SCD", "CON"))
total_long_t2$filename <- factor(
  total_long_t2$filename)
total_long_t2$ROI_pair <- factor(
  total_long_t2$ROI_pair,
  levels = c("ACC_LIFG", "RIFG_ACC",
             "ACC_LINS", "RINS_ACC",
             "LPIN_ACC", "RPIN_ACC"))
rownames(total_long_t2) <- NULL


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

  # SN FC
levene_test(total_long_fc_sn, Z_FC~is_SCD*timepoint)


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

  # ANOVA
res.aov.baseline <- anova_test(data = total_long_t0,
                             dv = Z_FC,
                             wid = filename,
                             between = is_SCD,
                             within = ROI_pair,
                             effect.size = "pes")
anovat <- get_anova_table(res.aov.baseline,
                          correction = "auto")
write.table(anovat, file = "baseline_ANOVA.txt",
     quote = F, sep = " | ", row.names = F)

# T1
res.aov.t1 <- anova_test(data = total_long_t1,
                               dv = Z_FC,
                               wid = filename,
                               between = is_SCD,
                               within = ROI_pair,
                               effect.size = "pes")
anovat <- get_anova_table(res.aov.t1,
                          correction = "auto")
write.table(anovat, file = "t1_ANOVA.txt",
            quote = F, sep = " | ", row.names = F)

# T2
res.aov.t2 <- anova_test(data = total_long_t2,
                         dv = Z_FC,
                         wid = filename,
                         between = is_SCD,
                         within = ROI_pair,
                         effect.size = "pes")
anovat <- get_anova_table(res.aov.t2,
                          correction = "auto")
write.table(anovat, file = "t2_ANOVA.txt",
            quote = F, sep = " | ", row.names = F)
  # permuted version, just in case:
res.aov.t2_perm <- aovperm(
  formula = Z_FC ~ is_SCD * ROI_pair
  + Error(filename/(ROI_pair)),
  data = total_long_t2,
  np = 10000)
res.aov.t2_perm


####==========================================================
### FOLLOW-UP ONE-WAY ANOVAS BETWEEN GROUPS (SCD)
## One-way ANOVA to study significant interaction effects...
## ...separately per time point

## Average SN FC
# Within each WS condition, comparing between subjects (BS)
bs <- total_long_fc_sn %>%
  group_by(timepoint) %>%
  anova_test(dv = Z_FC, wid = filename,
             between = is_SCD) %>%
  get_anova_table() %>% adjust_pvalue(
    method = "holm")
bs

## ROIs
# Within each WS condition, comparing between subjects (BS)
  # Baseline (T0)
    # All (including MCI)
bs <- total_long_t0_all %>%
  group_by(ROI_pair) %>%
  anova_test(dv = Z_FC, wid = filename,
             between = is_SCD) %>%
  get_anova_table() %>% adjust_pvalue(
    method = "holm")
bs

    # SCD-CON
bs <- total_long_t0 %>%
  group_by(ROI_pair) %>%
  anova_test(dv = Z_FC, wid = filename,
             between = is_SCD) %>%
  get_anova_table() %>% adjust_pvalue(
    method = "holm")
bs

  # T1
bs <- total_long_t1 %>%
  group_by(ROI_pair) %>%
  anova_test(dv = Z_FC, wid = filename,
             between = is_SCD) %>%
  get_anova_table() %>% adjust_pvalue(
    method = "holm")
bs

  # T2
bs <- total_long_t2 %>%
  group_by(ROI_pair) %>%
  anova_test(dv = Z_FC, wid = filename,
             between = is_SCD) %>%
  get_anova_table() %>% adjust_pvalue(
    method = "holm")
bs
    # Obtain descriptives for significant results:
    # ACC_LIFG and ACC_LINS
describeBy(total_long_t2$Z_FC[which(
  total_long_t2$ROI_pair=="ACC_LINS")],
  total_long_t2$is_SCD[which(
    total_long_t2$ROI_pair=="ACC_LINS")])
  # Save result
write.csv(bs, file = paste("/figures/",
                       "oneway_t2_ANOVA.csv"),
          quote = F, row.names = F)


####==========================================================
### FOLLOW-UP ONE-WAY ANOVAS WITHIN CONDITIONS (ROIs)
## One-way ANOVA to study significant interaction effects...
## separately per time point

## Average FC
# Relevant to understand the interaction above
ws <- total_long_fc_sn %>%
  group_by(is_SCD) %>%
  anova_test(dv = Z_FC, wid = filename,
             within = c(timepoint, type),
             effect.size = "pes") %>%
  get_anova_table() %>% adjust_pvalue(
    method = "holm")
ws
  # Descriptives for SCD across time points
describeBy(total_long_fc_sn$Z_FC[which(
  total_long_fc_sn$is_SCD=="SCD")],
  total_long_fc_sn$timepoint[which(
    total_long_fc_sn$is_SCD=="SCD")])
# Descriptives for CON across time points
describeBy(total_long_fc_sn$Z_FC[which(
  total_long_fc_sn$is_SCD=="CON")],
  total_long_fc_sn$timepoint[which(
    total_long_fc_sn$is_SCD=="CON")])

## ROIs
## Within each BS group, comparing within conditions 
  # Baseline (T0)
ws <- total_long_t0 %>%
  group_by(is_SCD) %>%
  anova_test(dv = Z_FC, wid = filename,
             within = ROI_pair, effect.size = "pes") %>%
  get_anova_table() %>% adjust_pvalue(
    method = "holm")
ws  

  # T1
ws <- total_long_t1 %>%
  group_by(is_SCD) %>%
  anova_test(dv = Z_FC, wid = filename,
             within = ROI_pair, effect.size = "pes") %>%
  get_anova_table() %>% adjust_pvalue(
    method = "holm")
ws

  # T2
ws <- total_long_t2 %>%
  group_by(is_SCD) %>%
  anova_test(dv = Z_FC, wid = filename,
             within = ROI_pair, effect.size = "pes") %>%
  get_anova_table() %>% adjust_pvalue(
    method = "holm")
ws


####==========================================================
### FOLLOW-UP PAIRWISE T-TESTS OF ROI PAIRS ACROSS TIMEPOINTS
## Pairwise t-tests to obtain greater detail into the...
## ...differences

  # Across time points
pwc <- total_all_long %>% 
  pairwise_t_test(
    Z_FC ~ ROI_pair, pool.sd = FALSE,
    p.adjust.method = "holm"
  )
pwc[which(pwc$p.adj.signif!="ns"),]

# Across time points for SCD
pwc <- total_all_long[which(
  total_all_long$is_SCD=="SCD"),] %>% 
  group_by(ROI_pair) %>%
  pairwise_t_test(
    Z_FC ~ timepoint, pool.sd = FALSE,
    p.adjust.method = "holm"
  )
pwc[which(pwc$p.adj.signif!="ns"),]

  # Baseline (T0) -> SCD x ROIpair interaction
pwc <- total_long_t0 %>% 
  pairwise_t_test(
    Z_FC ~ ROI_pair, pool.sd = FALSE,
    p.adjust.method = "holm"
  )
pwc[which(pwc$p.adj.signif!="ns"),]
    # CON
pwc <- total_long_t0[which(
  total_long_t0$is_SCD=="CON"),] %>% 
  pairwise_t_test(
    Z_FC ~ ROI_pair, pool.sd = FALSE,
    p.adjust.method = "holm"
  )
pwc[which(pwc$p.adj.signif!="ns"),]
    # SCD
pwc <- total_long_t0[which(
  total_long_t0$is_SCD=="SCD"),] %>% 
  pairwise_t_test(
    Z_FC ~ ROI_pair, pool.sd = FALSE,
    p.adjust.method = "holm"
  )
pwc[which(pwc$p.adj.signif!="ns"),]

  # T1
pwc <- total_long_t1 %>% 
  pairwise_t_test(
    Z_FC ~ ROI_pair, pool.sd = FALSE,
    p.adjust.method = "holm"
  )
pwc[which(pwc$p.adj.signif!="ns"),]

  # T2
    # All
pwc <- total_long_t2 %>% 
  pairwise_t_test(
    Z_FC ~ ROI_pair, pool.sd = FALSE,
    p.adjust.method = "holm"
  )
pwc[which(pwc$p.adj.signif!="ns"),]


####==========================================================
### PLOTTING
## Box plots of ROI-to-ROI FC per SCD group per time point

# Baseline (T0)
# SCD and CON only
ggplot(total_long_t0,
       aes(x = ROI_pair,
           y = Z_FC,
           fill = is_SCD)
  ) + geom_boxplot(outlier.shape = NA
  ) + scale_fill_manual(values=c(
    "#f1a340", "#998ec3")
  ) + ggtitle("Baseline: T0"
  ) + xlab("ROI pairs"
  ) + ylab("Functional connectivity (Z)"
  ) + labs(fill = "Group"
  ) + theme_bw() + ylim(-0.5, 2.25
  ) + scale_x_discrete(
    labels=c("RINS_ACC" = "ACC-RINS",
             "ACC_LINS" = "ACC-LINS",
             "RIFG_ACC" = "ACC-RMFG",
             "ACC_LIFG" = "ACC-LMFG",
             "LPIN_ACC" = "CON-L",
             "RPIN_ACC" = "CON-R")
  ) + theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12,
                              face = "bold"),
    plot.title = element_text(hjust = 0.5,
    face = "bold")
  ) + geom_vline(xintercept = 1.5:5.5,
                 color = "black",
                 size = 0.2
  ) + stat_compare_means(
    label = "p.signif",
    label.y = 1.7, method = "anova",
    hide.ns = T
  ) + geom_point(aes(fill = is_SCD),
    size = 2, color = "black",
    shape = 21, alpha = 0.8,
    position = position_jitterdodge(
    jitter.width = 0.3)
  ) + geom_hline(
    yintercept = 0,
    color = "gray",
    size = 0.2,
    linetype = "dashed")
ggsave("figures/boxplot_T0.jpg", width = 30,
       height = 20, units = "cm", dpi = 400)

# T1
ggplot(total_long_t1,
       aes(x = ROI_pair,
           y = Z_FC,
           fill = is_SCD)
  ) + geom_boxplot(outlier.shape = NA
  ) + scale_fill_manual(values=c(
                 "#f1a340", "#998ec3")
  ) + ggtitle("Follow-up: T1"
  ) + xlab("ROI pairs"
  ) + ylab("Functional connectivity (Z)"
  ) + labs(fill = "Group"
  ) + theme_bw() + ylim(-0.5, 2.25
  ) + scale_x_discrete(
    labels=c("RINS_ACC" = "ACC-RINS",
             "ACC_LINS" = "ACC-LINS",
             "RIFG_ACC" = "ACC-RMFG",
             "ACC_LIFG" = "ACC-LMFG",
             "LPIN_ACC" = "CON-L",
             "RPIN_ACC" = "CON-R")
  ) + theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text = element_text(size = 12),
            axis.title = element_text(size = 12,
                                      face = "bold"),
            plot.title = element_text(hjust = 0.5,
              face = "bold")
  ) + geom_vline(xintercept = 1.5:5.5,
                 color = "black",
                 size = 0.2
  ) + stat_compare_means(
    label = "p.signif",
    label.y = 1.7, method = "anova",
    hide.ns = T
  ) + geom_point(aes(fill = is_SCD),
                 size = 2, color = "black",
                 shape = 21, alpha = 0.8,
                 position = position_jitterdodge(
                 jitter.width = 0.3)
  ) + geom_hline(yintercept = 0,
    color = "gray",
    size = 0.2,
    linetype = "dashed")
ggsave("figures/boxplot_T1.jpg", width = 30,
       height = 20, units = "cm", dpi = 400)

# T2
ggplot(total_long_t2,
       aes(x = ROI_pair,
           y = Z_FC,
           fill = is_SCD)) + 
  geom_boxplot(outlier.shape = NA
  ) + scale_fill_manual(values=c(
                 "#f1a340", "#998ec3")
  ) + xlab("ROI pairs"
  ) + ylab("Functional connectivity (Z)"
  ) + ggtitle("Follow-up: T2"
  ) + labs(fill = "Group"
  ) + theme_bw() + ylim(-0.5, 2.25
  ) + scale_x_discrete(
    labels=c("RINS_ACC" = "ACC-RINS",
             "ACC_LINS" = "ACC-LINS",
             "RIFG_ACC" = "ACC-RMFG",
             "ACC_LIFG" = "ACC-LMFG",
             "LPIN_ACC" = "CON-L",
             "RPIN_ACC" = "CON-R")
  ) + theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text = element_text(size = 12),
            axis.title = element_text(size = 12,
                                      face = "bold"),
            plot.title = element_text(hjust = 0.5,
                                      face = "bold")
  ) + geom_vline(xintercept = 1.5:5.5,
                 color = "black",
                 size = 0.2
  ) + stat_compare_means(
    label = "p.signif",
    label.y = 1.7, method = "anova",
    hide.ns = T
  ) + geom_point(aes(fill = is_SCD),
                 size = 2, color = "black",
                 shape = 21, alpha = 0.8,
                 position = position_jitterdodge(
                 jitter.width = 0.3)
  ) + geom_hline(yintercept = 0,
                 color = "gray",
                 size = 0.2,
                 linetype = "dashed")
ggsave("figures/boxplot_T2.jpg", width = 30,
       height = 20, units = "cm", dpi = 400)

# Overall across ROI pairs
ggplot(total_all_long,
       aes(x = ROI_pair,
           y = Z_FC,
           fill = ROI_pair)
  ) + geom_violin(
  ) + scale_fill_manual(values=c("#1f78b4", "#1f78b4",
                                 "#a6cee3", "#a6cee3",
                                 "#b2df8a", "#b2df8a")
  ) + xlab("ROI pairs"
  ) + ylab("Functional connectivity (Z)"
  ) + ggtitle("Main effect of ROI pairs"
  ) + ylim(-0.25, 2.25
  ) + scale_x_discrete(
    labels=c("RINS_ACC" = "ACC-RINS",
             "ACC_LINS" = "ACC-LINS",
             "RIFG_ACC" = "ACC-RMFG",
             "ACC_LIFG" = "ACC-LMFG",
             "LPIN_ACC" = "CON-L",
             "RPIN_ACC" = "CON-R")
  ) + theme(panel.grid.minor = element_blank(),
            axis.ticks.x = element_blank(),
            legend.position = "NULL",
            axis.text = element_text(size = 12),
            axis.title = element_text(size = 12,
                                      face = "bold"),
            plot.title = element_text(hjust = 0.5,
                                      face = "bold")
  ) + geom_vline(xintercept = 4.5,
                 color = "black",
                 size = 0.2
  ) + stat_compare_means(
    label = "p",
    label.y = c(1.55, 1.75, 1.75, 1.95, 2.1, 2.25),
    comparisons = list(c("RINS_ACC", "RIFG_ACC"),
                       c("ACC_LINS", "ACC_LIFG"),
                       c("RINS_ACC", "RPIN_ACC"),
                       c("ACC_LINS", "LPIN_ACC"),
                       c("RIFG_ACC", "RPIN_ACC"),
                       c("ACC_LIFG", "LPIN_ACC")),
    tip.length = 0.01,
    hide.ns = T
  ) + geom_hline(yintercept = 0,
                 color = "gray",
                 size = 0.2,
                 linetype = "dashed"
  ) + geom_boxplot(width = 0.1, fill = "white",
                   alpha = 0.7,
                   outlier.shape = NA)
ggsave("figures/boxplot_ROIs.jpg", width = 30,
       height = 20, units = "cm", dpi = 400)

## Box plots of average SN FC per time point

# Turn Timepoint into a factor
if (!is.factor(total$timepoint)){
total$timepoint <- as.factor(total$timepoint)}

# Average FC
ggplot(total,
       aes(x = timepoint, y = SN_FC,
           fill = is_SCD)
  ) + geom_boxplot(outlier.shape = NA,
                   width = 0.7
  ) + scale_fill_manual(values = c(
    "#f1a340", "#998ec3")
  ) + ylab("Average Functional Connectivity (Z)"
  ) + xlab("Timepoints"
  ) + labs(fill = "Group"
  ) + scale_x_discrete(
    labels=c("1" = "T0",
             "2" = "T1",
             "3" = "T2")
  ) + theme_bw() + ylim(0.05, 1.5
  ) + theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text = element_text(size = 12),
            axis.title = element_text(size = 12,
                                      face = "bold")
  ) + geom_point(aes(fill = is_SCD),
                 size = 3,
                 shape = 21, alpha = 0.6,
                 position = position_jitterdodge(
                   jitter.width = 0.3)
  ) + geom_vline(xintercept = 1.5:2.5,
                 color = "black",
                 size = 0.2
  ) + stat_compare_means(
    label = "p.signif",
    label.y = 1.45, method = "anova",
    hide.ns = T)
ggsave("figures/boxplot_avr.jpg", width = 30,
       height = 20, units = "cm", dpi = 400)

# Average FC individual lines
ggplot(total,
       aes(x = timepoint,
           y = SN_FC,
           group = filename,
           fill = is_SCD)
) + scale_fill_manual(values = c(
  "#f1a340", "#998ec3")
) + scale_x_discrete(name = "Timepoints",
  labels=c("1" = "T0",
           "2" = "T1",
           "3" = "T2")
) + scale_y_continuous(name = "Average Functional Connectivity (Z)",
                       breaks=seq(0, 1.50, 0.25)
) + theme_bw(
) + theme(#panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.box.background = element_rect(),
          panel.border = element_blank(),
          axis.line = element_line(color = 'black',
                                   size = 0.4),
          axis.ticks.x = element_blank(),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 12,
                                    face = "bold")
) + geom_point(aes(fill = is_SCD),
               size = 1, alpha = 0.8,
               shape = 21
) + geom_line(aes(color = is_SCD),
              alpha = 0.5
              ) + scale_color_manual(values = c(
   "#f1a340", "#998ec3")
) + labs(color = "Group", fill = "Group"
) + stat_summary(aes(group = is_SCD, color = is_SCD),
                 geom = "line", fun = mean, size = 1.5,
                 show.legend = F
) + geom_smooth(aes(group = is_SCD,
                    color = is_SCD),
                alpha = 0.3)
ggsave("figures/boxplot_indiv_points.jpg", width = 30,
       height = 20, units = "cm", dpi = 400)


####==========================================================
### PARTICIPANT COUNT
## Characterization for each time point

# Create a data frame of participants who had both follow-ups
followup <- data.frame(total_t1$filename[which(
  total_t1$filename %in% total_t2$filename==TRUE)])
colnames(followup)[1] <- "filename"

# Create a data frame from baseline participants who had...
# ...a full follow-up.
all <- data.frame(total_t0$filename[which(
  total_t0$filename %in% followup$filename==TRUE)])
colnames(all)[1] <- "filename"

# Create a data frame from baseline participants who had...
# ...the first follow-up *only*.
tp01 <- data.frame(total_t0$filename[which(
  total_t0$filename %in% total_t1$filename==TRUE &
    total_t0$filename %in% followup$filename==FALSE)])
colnames(tp01)[1] <- "filename"

# Create a data frame from baseline participants who had...
# ...the second follow-up *only*.
tp02 <- data.frame(total_t0$filename[which(
  total_t0$filename %in% total_t2$filename==TRUE &
    total_t0$filename %in% followup$filename==FALSE)])
colnames(tp02)[1] <- "filename"

# Create a data frame from t1 participants who had...
# ...the second follow-up *only*.
tp12 <- data.frame(total_t1$filename[which(
  total_t1$filename %in% total_t2$filename==TRUE &
    total_t1$filename %in% followup$filename==FALSE)])
colnames(tp12)[1] <- "filename"

# Create a data frame from baseline participants who had...
# ...no follow-up.
baseline <- data.frame(total_t0$filename[which(
  total_t0$filename %in% all$filename==FALSE &
    total_t0$filename %in% tp01$filename==FALSE &
    total_t0$filename %in% tp02$filename==FALSE &
    total_t0$filename %in% tp12$filename==FALSE)])
colnames(baseline)[1] <- "filename"
