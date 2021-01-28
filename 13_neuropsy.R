####========================================= A.L.R.R.2020 - 2021
# DESCRIPTION
# In this script, I perform group comparisons for the...
# ...neuropsychological and demographic variables between...
# ...groups and across time points.


####==========================================================
# INSTALL PACKAGES
# install.packages("pacman")
# require(pacman)
# pacman::p_load(ggplot2, dplyr, ggthemes, ggvis, plotly,
#                rio, stringr, tidyr, readxl, ggpubr,
#                psych, car, tidyverse, rstatix, cocor,
#                ppcor, RColorBrewer, Hmisc, DescTools,
#                permuco, CorrMixed, finalfit)


####==========================================================
# SET WORKING DIRECTORY
setwd(paste('/Users/lmuresearchfellowship/Documents/',
  'Adriana/LMUFellowship/Projects/Goal_A/',
  sep = ""))


####==========================================================
# GET MAIN FILE
# Read text files and create temporal data frames

# "Total" file in wide format
if (!exists('total')){
  total <- read.csv("ROI-FC-all.csv",
           header = T, row.names = 1)
}

# Adjust 'total' if retrieved from read.csv
total$filename <- factor(substr(total$filename, 1, 12))
total$is_SCD <- factor(total$is_SCD)
levels(total$is_SCD)["SCD"] <- "SCD"
levels(total$is_SCD)["CON"] <- "CON"
levels(total$is_SCD)["MCI"] <- "MCI"
total$is_SCD <- factor(total$is_SCD,
                       levels = c("MCI", "SCD", "CON"))

# Get information on potentially to-exclude participants
if (!exists('exclude')){
  exclude <- data.frame(read.csv(
    "Partic_excluded.txt",
    sep = "\t"))
}

# Reasons for exclusion
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
# ROI-PAIR SELECTION
# Leave in the columns corresponding to the ROI-pairs of...
# ...interest (all involving the ACC): ACC-LIFG; ACC-RIFG;...
# ... ACC-LINS; ACC-RINS.

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

# Obtain average FC for control ROI pairs
total$CON_FC <- round(rowMeans(
  subset(total,
         select = c(LPIN_ACC,RPIN_ACC)),
  na.rm = TRUE), 2)


####==========================================================
# PARTICIPANT EXCLUSION
# Exclude those participants from 'total'

# 'Total' with complete resting-state data only
total_orig <- total
total <- total_orig[-which(
  total_orig$filename %in% Excluded==TRUE),]
rownames(total) <- NULL

# Adjust total to excluding MCI
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
# OBTAIN NEUROPSYCHOLOGICAL/DEMOGRAPHIC DATA

# Read file
neuropsy <- data.frame(read.csv("./General/neuropsych_R.csv"))

# Adjust participant file name
neuropsy$ParticipantID <- gsub("_", "-",
                               neuropsy$ParticipantID)

# Order data frame according to participant file name
neuropsy <- neuropsy[order(neuropsy$ParticipantID),]
rownames(neuropsy) <- NULL

# Create a 'time point' column based on participant file name
neuropsy$timepoint <- "1"
neuropsy <- neuropsy[, c(1, which(
  colnames(neuropsy)=="timepoint"), 2:(length(neuropsy)-1))]
neuropsy$timepoint[which(grepl(
  "-02", neuropsy$ParticipantID)==T)] <- "2"
neuropsy$timepoint[which(grepl(
  "-03", neuropsy$ParticipantID)==T)] <- "3"
neuropsy$timepoint <- factor(neuropsy$timepoint)

# Check variables to convert them into appropriate format
colnames(neuropsy)
neuropsy[, c(which(colnames(neuropsy)=="AgeatBaseline"),
             which(colnames(neuropsy)=="AgeatTesting"), which(
               colnames(neuropsy)=="GeriatricDepressionScaleTotalScore"
               ):length(neuropsy))] <- sapply(neuropsy[, c(
                 which(colnames(neuropsy)=="AgeatBaseline"),
                 which(colnames(neuropsy)=="AgeatTesting"),
                 which(colnames(neuropsy)=="GeriatricDepressionScaleTotalScore"
                       ):length(neuropsy))], as.numeric)
neuropsy[, c(which(colnames(neuropsy)=="Gender"),
             which(
               colnames(neuropsy)=="DoctorSeen.forMemoryComplaints"):which(
                 colnames(neuropsy)=="VENIHighest.DegreeEarned"))
         ] <- sapply(neuropsy[, c(which(
           colnames(neuropsy)=="Gender"), which(
             colnames(neuropsy)=="DoctorSeen.forMemoryComplaints"
             ):which(colnames(neuropsy)=="VENIHighest.DegreeEarned"))],
           as.numeric)
sapply(neuropsy, class)

# Convert into factors some of the variables
neuropsy$Gender <- factor(neuropsy$Gender)
neuropsy$DoctorSeen.forMemoryComplaints <-
  factor(neuropsy$DoctorSeen.forMemoryComplaints)
neuropsy$FamilyHistoryof.AlzheimersorDementia <-
  factor(neuropsy$FamilyHistoryof.AlzheimersorDementia)

# Select the same participants as in 'total'
neuropsy <- neuropsy[which(neuropsy$ParticipantID %in% paste(
    total$filename, "-", "0", total$timepoint,
    sep = "") == T), ]
rownames(neuropsy) <- NULL

# Change the participant filename to match up 'total'
neuropsy$ParticipantID <- substr(neuropsy$ParticipantID, 1, 12)

# Create an 'is_SCD' variable based on participant file name
neuropsy$is_SCD <- "CON"
neuropsy$is_SCD[which(grepl(
  "con",neuropsy$ParticipantID) == T)] <- "CON"
neuropsy$is_SCD[which(grepl(
  "sci",neuropsy$ParticipantID) == T)] <- "SCD" 
neuropsy$is_SCD <- factor(neuropsy$is_SCD,
                          levels = c("SCD", "CON"))

# Reorganize 'neuropsy'
neuropsy <- neuropsy[, c(which(
  colnames(neuropsy)=="ParticipantID"), which(
    colnames(neuropsy)=="timepoint"), which(
      colnames(neuropsy)=="is_SCD"), which(
        colnames(neuropsy)=="AgeatBaseline"):length(
        neuropsy))]

# Add Age to 'total'
total <- cbind(total, neuropsy$AgeatTesting)
colnames(total)[which(
  colnames(total)=="neuropsy$AgeatTesting")] <- "Age"


####==========================================================
# SEPARATE TIME POINT FILES
# "Total" file in wide format separately for each time point

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

# Adjust column names for each time point file...
#... (excluding demographic variables: not needed here)
colnames(total_t0)[-c(1,2,3)] <- paste(
  colnames(total_t0)[-c(1,2,3)], 1, sep = "_")
colnames(total_t1)[-c(1,2,3)] <- paste(
  colnames(total_t1)[-c(1,2,3)], 2, sep = "_")
colnames(total_t2)[-c(1,2,3)] <- paste(
  colnames(total_t2)[-c(1,2,3)], 3, sep = "_")

# Delete "timepoint" column from each data frame
total_t0 <- total_t0[, -which(
  colnames(total_t0)=="timepoint")]
total_t1 <- total_t1[, -which(
  colnames(total_t1)=="timepoint")]
total_t2 <- total_t2[, -which(
  colnames(total_t2)=="timepoint")]


####==========================================================
# CONVERT FROM CUSTOM TO WIDE FORMAT
# This format is necessary because of the way how I...
# ...exported the values from FSL and how they need to be...
# ...transformed in the next step for ANOVAs ('long' format)

# Merge data frames according to file names (2 steps)
temp <- merge(data.frame(total_t0, row.names=NULL),
               data.frame(total_t1, row.names=NULL),
               by = "filename",
               all = T)
total_wide <- merge(data.frame(temp, row.names=NULL),
              data.frame(total_t2, row.names=NULL),
              by = "filename",
              all = T)

# Adjust column names, especially to remove duplicates
# Remove the ".y" part from the column names
total_wide <- total_wide[, -which(
  grepl(".y", colnames(total_wide))==T)]

# Remove the .x from the "is_SCD" variable
colnames(total_wide)[which(
  colnames(total_wide)=="is_SCD.x")] <- "is_SCD"
  
# Remove duplicated (equal name) variables
total_wide <- total_wide[, -which(
  duplicated(colnames(total_wide))==T)]

# Create one separate data frame for the average FC
total_wide_fc_sn <- total_wide[, c(1, 2, which(
  grepl("_FC_", colnames(total_wide)) == T),
  which(grepl("Age", colnames(total_wide)) == T))]

# Delete the average FC columns from "total_wide"
total_wide <- total_wide[, -which(
  grepl("_FC_", colnames(total_wide)) == T)]

# Reorganize "total_wide"
total_wide <- total_wide[, c(1, 2, which(
  grepl("Age", colnames(total_wide)) == T),
  which(grepl("ACC", colnames(total_wide)) == T))]

# Remove the previously-created "temp" to clean work space
rm(temp)


####==========================================================
# THREE-WAY MIXED ANOVA ACROSS ROIS
# WS: time point and ROI / BS: SCD
# DV: Z_FC

# Convert data frame to long format
total_all_long <- pivot_longer(total_wide,
  names_to = c("ROI_pair", "timepoint"),
  cols = ACC_LIFG_1:RPIN_ACC_3,
  names_pattern = "(.*)_(.*)",
  values_to = "Z_FC",
  values_drop_na = T)

# Make some adjustments to the new data frame
total_all_long <- data.frame(total_all_long)
total_all_long$filename <- factor(total_all_long$filename)
total_all_long$timepoint <- as.numeric(
  total_all_long$timepoint)
total_all_long$timepoint <- factor(total_all_long$timepoint)
total_all_long$ROI_pair <- factor(
  total_all_long$ROI_pair,
  levels = c("ACC_LIFG", "RIFG_ACC",
             "ACC_LINS", "RINS_ACC",
             "LPIN_ACC", "RPIN_ACC"))

# Nest ages at testing into the long data frame
total_all_long <- pivot_longer(total_all_long,
  names_to = "age_tp",
  names_prefix = "Age_",
  cols = Age_1:Age_3,
  values_to = "Age",
  values_drop_na = T)

# Actual ANOVA ROI pair
res.aov.total_all_long <- anova_test(data = total_all_long,
                               dv = Z_FC,
                               wid = filename,
                               between = is_SCD,
                               within = c(timepoint,
                                          ROI_pair),
                               covariate = "Age",
                               effect.size = "pes")
get_anova_table(res.aov.total_all_long, correction = "auto")


####==========================================================
# THREE-WAY MIXED ANOVA AVERAGE FC (MAIN AND CONTROL)
# WS: time point and ROI average type
# ...BS: SCD; DV: Z_FC; COV: Age at testing

# Convert data frame to long format for FC
total_long_fc_sn <- pivot_longer(total_wide_fc_sn,
  names_to = c("ROI", "timepoint"),
  names_pattern = "(.*)_(.*)",
  cols = SN_FC_1:CON_FC_3,
  values_to = "Z_FC",
  values_drop_na = TRUE)

# Make some adjustments to the new data frame
total_long_fc_sn <- data.frame(total_long_fc_sn)
total_long_fc_sn$filename <- factor(
  total_long_fc_sn$filename)
total_long_fc_sn$timepoint <- factor(
  total_long_fc_sn$timepoint)
total_long_fc_sn$ROI <- factor(
  total_long_fc_sn$ROI)

# Nest ages at testing into the long data frame
total_long_fc_sn <- pivot_longer(total_long_fc_sn,
  names_to = "age_tp",
  names_prefix = "Age_",
  cols = Age_1:Age_3,
  values_to = "Age",
  values_drop_na = TRUE)

# ANOVA average FC controlling for age at testing
res.aov.total_long_fc_sn <- anova_test(
  data = total_long_fc_sn,
  dv = Z_FC,
  wid = filename,
  between = is_SCD,
  within = c("timepoint", "ROI"),
  covariate = "Age",
  effect.size = "pes")
get_anova_table(res.aov.total_long_fc_sn, correction = "auto")


####==========================================================
# FOLLOW-UP RESULTS OF MIXED ANOVAS (ROI-pair analysis)

# Result
get_anova_table(res.aov.total_all_long, correction = "auto")

# Main effect of 'Age'
overall <- rcorr(as.matrix(total[, 4:12])); overall

# Main effect of 'ROI pair'
pwc <- total_all_long %>% 
  pairwise_t_test(
    Z_FC ~ ROI_pair, pool.sd = FALSE,
    p.adjust.method = "holm")
pwc[which(pwc$p.adj.signif!="ns"),]

# 'Age:timepoint' effect
cor_t0 <- rcorr(as.matrix(total_t0[, 3:11])); cor_t0
cor_t1 <- rcorr(as.matrix(total_t1[, 3:11])); cor_t1
cor_t2 <- rcorr(as.matrix(total_t2[, 3:11])); cor_t2

# 'SCD:timepoint' effect
# Across Groups (looking at the effect of 'timepoint')
ws <- total_all_long[which(total_all_long$is_SCD=="SCD"),] %>%
  #group_by(ROI_pair) %>%
  anova_test(dv = Z_FC, wid = filename,
             within = timepoint, covariate = Age,
             effect.size = "pes") %>%
  get_anova_table() %>% adjust_pvalue(
    method = "holm"); ws
total_all_long[which(total_all_long$is_SCD=="SCD"),] %>%
pairwise_t_test(
  Z_FC ~ timepoint, pool.sd = FALSE,
  p.adjust.method = "holm")
ws <- total_all_long[which(total_all_long$is_SCD=="CON"),] %>%
  #group_by(ROI_pair) %>%
  anova_test(dv = Z_FC, wid = filename,
             within = timepoint, covariate = Age,
             effect.size = "pes") %>%
  get_anova_table() %>% adjust_pvalue(
    method = "holm"); ws
total_all_long[which(total_all_long$is_SCD=="CON"),] %>%
  pairwise_t_test(
    Z_FC ~ timepoint, pool.sd = FALSE,
    p.adjust.method = "holm")

# Across time points (looking at the effect of Group)
bs <- total_all_long %>%
  group_by(timepoint) %>%
  anova_test(dv = Z_FC, wid = filename,
             between = is_SCD, covariate = Age,
             effect.size = "pes") %>%
  get_anova_table() %>% adjust_pvalue(
    method = "holm"); bs

# 'Is_SCD:ROI_pair' effect
bs <- total_all_long %>%
  group_by(ROI_pair) %>%
  anova_test(dv = Z_FC, wid = filename,
             between = is_SCD, covariate = Age,
             effect.size = "pes") %>%
  get_anova_table() %>% adjust_pvalue(
    method = "holm"); bs
describeBy(total$ACC_LINS, total$is_SCD)
describeBy(total$LPIN_ACC, total$is_SCD)
describeBy(total$RPIN_ACC, total$is_SCD)
total_all_long[which(total_all_long$is_SCD=="SCD"),] %>%
  pairwise_t_test(
    Z_FC ~ ROI_pair, pool.sd = FALSE,
    p.adjust.method = "holm")
# partial eta squared
ws <- total_all_long %>%
  group_by(is_SCD) %>%
  anova_test(dv = Z_FC, wid = filename,
             within = ROI_pair, covariate = Age,
             effect.size = "pes") %>%
  get_anova_table() %>% adjust_pvalue(
    method = "holm"); ws
# pairwise, if wanted (replace, for CON)
total_all_long[which(total_all_long$is_SCD=="SCD"),] %>%
  pairwise_t_test(
    Z_FC ~ ROI_pair, pool.sd = FALSE,
    p.adjust.method = "holm")


####==========================================================
# FOLLOW-UP RESULTS OF MIXED ANOVAS (Average FC analysis)

# Result
get_anova_table(res.aov.total_long_fc_sn, correction = "auto")

# Main effect of 'Timepoint'
pwc <- total_long_fc_sn %>% 
  pairwise_t_test(
    Z_FC ~ timepoint, pool.sd = F,
    p.adjust.method = "holm"
  )
pwc[which(pwc$p.adj.signif!="ns"),]

# Main effect of 'ROI'
t.test(total$SN_FC, total$CON_FC, paired = T)

# 'Age:timepoint' effect
cor_t0 <- rcorr(as.matrix(total_t0[, 9:11])); cor_t0
cor_t1 <- rcorr(as.matrix(total_t1[, 9:11])); cor_t1
cor_t2 <- rcorr(as.matrix(total_t2[, 9:11])); cor_t2

# 'is_SCD:timepoint' effect
# Across Groups (looking at the effect of 'timepoint')
ws <- total_long_fc_sn[which(
  total_long_fc_sn$is_SCD=="SCD"),] %>%
  group_by(ROI) %>%
  anova_test(dv = Z_FC, wid = filename,
             within = timepoint, covariate = Age,
             effect.size = "pes") %>%
  get_anova_table() %>% adjust_pvalue(
    method = "holm"); ws
ws <- total_long_fc_sn[which(
  total_long_fc_sn$is_SCD=="CON"),] %>%
  group_by(ROI) %>%
  anova_test(dv = Z_FC, wid = filename,
             within = timepoint, covariate = Age,
             effect.size = "pes") %>%
  get_anova_table() %>% adjust_pvalue(
    method = "holm"); ws

# Across time points (looking at the effect of Group)
bs <- total_long_fc_sn %>%
  group_by(timepoint) %>%
  anova_test(dv = Z_FC, wid = filename,
             between = is_SCD, covariate = Age,
             effect.size = "pes") %>%
  get_anova_table() %>% adjust_pvalue(
    method = "holm"); bs

# 'Is_SCD:ROI' effect (across ROIs, Group effect)
bs <- total_long_fc_sn %>%
  group_by(ROI) %>%
  anova_test(dv = Z_FC, wid = filename,
             between = is_SCD, covariate = Age,
             effect.size = "pes") %>%
  get_anova_table() %>% adjust_pvalue(
    method = "holm"); bs
describeBy(total_long_fc_sn$Z_FC[which(
  total_long_fc_sn$ROI=="CON_FC")],
          total_long_fc_sn$is_SCD[which(
            total_long_fc_sn$ROI=="CON_FC")])
describeBy(total_long_fc_sn$Z_FC[which(
  total_long_fc_sn$ROI=="SN_FC")],
  total_long_fc_sn$is_SCD[which(
    total_long_fc_sn$ROI=="SN_FC")])

# 'timepoint:ROI' effect
# Across ROIs (looking at timepoint effect)
ws <- total_long_fc_sn %>%
  group_by(ROI) %>%
  anova_test(dv = Z_FC, wid = filename,
             within = timepoint, covariate = Age,
             effect.size = "pes") %>%
  get_anova_table() %>% adjust_pvalue(
    method = "holm"); ws

# Across timepoints (looking at ROI effect) corr-p = .017
t.test(total$SN_FC[which(total$timepoint=="1")],
       total$CON_FC[which(total$timepoint=="1")], paired = T)
t.test(total$SN_FC[which(total$timepoint=="2")],
       total$CON_FC[which(total$timepoint=="2")], paired = T)
t.test(total$SN_FC[which(total$timepoint=="3")],
       total$CON_FC[which(total$timepoint=="3")], paired = T)


####==========================================================
# PARTICIPANT COUNT

# Create a data frame of participants who had both follow-ups
followup <- data.frame(total_t1$filename[which(
  total_t1$filename %in% total_t2$filename==T)])
colnames(followup)[1] <- "filename"

# Create a data frame from baseline participants who had...
# ...a full follow-up.
all <- data.frame(total_t0$filename[which(
  total_t0$filename %in% followup$filename==T)])
colnames(all)[1] <- "filename"
print(paste("the number of participants who had",
            "ALL time points was", nrow(all)))

# Create a data frame from baseline participants who had...
# ...the first follow-up *only*.
tp01 <- data.frame(total_t0$filename[which(
  total_t0$filename %in% total_t1$filename==T &
    total_t0$filename %in% followup$filename==F)])
colnames(tp01)[1] <- "filename"
print(paste("the number of participants who had",
            "baseline and the first follow-up only was",
            nrow(tp01)))

# Create a data frame from baseline participants who had...
# ...the second follow-up *only*.
tp02 <- data.frame(total_t0$filename[which(
  total_t0$filename %in% total_t2$filename==T &
    total_t0$filename %in% followup$filename==F)])
colnames(tp02)[1] <- "filename"
print(paste("the number of participants who had",
            "baseline and the 2nd follow-up only was",
            nrow(tp02)))

# Create a data frame from t1 participants who had...
# ...the second follow-up *only*.
tp12 <- data.frame(total_t1$filename[which(
  total_t1$filename %in% total_t2$filename==T &
    total_t1$filename %in% followup$filename==F)])
colnames(tp12)[1] <- "filename"
print(paste("the number of participants who did",
            "not have baseline was", nrow(tp12)))

# Create a data frame from baseline participants who had...
# ...no follow-up.
baseline <- data.frame(total_t0$filename[which(
  total_t0$filename %in% all$filename==F &
    total_t0$filename %in% tp01$filename==F &
    total_t0$filename %in% tp02$filename==F &
    total_t0$filename %in% tp12$filename==F)])
colnames(baseline)[1] <- "filename"
print(paste("the number of participants who only had",
            "baseline (and no follow-ups) was", nrow(baseline)))


####==========================================================
# DESCRIPTIVES - DEMOGRAPHICS

# Age at testing difference between SCD and CON
neuropsy %>% group_by(timepoint) %>%
  t_test(AgeatTesting ~ is_SCD)
describeBy(total_t0$Age_1, total_t0$is_SCD)
describeBy(total_t1$Age_2, total_t1$is_SCD)
describeBy(total_t2$Age_3, total_t2$is_SCD)

# Gender across time points and groups (replace time point)
chisq.test(neuropsy$Gender[which(neuropsy$timepoint=="1")],
           neuropsy$is_SCD[which(neuropsy$timepoint=="1")])
table(neuropsy$Gender[which(neuropsy$timepoint=="1")],
      neuropsy$is_SCD[which(neuropsy$timepoint=="1")])

# Education across time points and groups
neuropsy %>% group_by(timepoint) %>%
  t_test(VENIHighest.DegreeEarned ~ is_SCD)
describeBy(neuropsy$VENIHighest.DegreeEarned[which(
  neuropsy$timepoint=="1")], neuropsy$is_SCD[which(
    neuropsy$timepoint=="1")]) # replace time point

# MFQ & personality difference between SCD and CON
neuropsy %>% group_by(timepoint) %>%
  t_test(MFQGeneral.FrequencyofForgettingFactor ~ is_SCD)
neuropsy %>% group_by(is_SCD) %>%
  pairwise_t_test(
    MFQGeneral.FrequencyofForgettingFactor ~ timepoint,
    pool.sd = F, p.adjust.method = "holm")
  pwc( ~ is_SCD)
neuropsy %>% group_by(timepoint) %>%
  t_test(BigFiveInventoryConscientiousnessTotalScore ~ is_SCD)
neuropsy %>% group_by(timepoint) %>%
  t_test(BigFiveInventoryNeuroticismTotalScore ~ is_SCD)
describeBy(neuropsy$MFQGeneral.FrequencyofForgettingFactor[which(
  neuropsy$timepoint == 1)], # adjust time point here
  neuropsy$is_SCD[which(neuropsy$timepoint == 1)])
describeBy(neuropsy$BigFiveInventoryConscientiousnessTotalScore[which(
  neuropsy$timepoint == 1)], # adjust time point here
  neuropsy$is_SCD[which(neuropsy$timepoint == 1)])
describeBy(neuropsy$BigFiveInventoryNeuroticismTotalScore[which(
  neuropsy$timepoint == 1)], # adjust time point here
  neuropsy$is_SCD[which(neuropsy$timepoint == 1)])


####==========================================================
# MISSING PATTERNS

# Full 'total' with all 'neuropsy'
total_full <- cbind(total, neuropsy)
total_full <- total_full[, -which(
  duplicated(colnames(total_full))==T)]

temp <- total_full[which(
  total_full$ParticipantID %in% baseline$filename==T), ]
temp <- rbind(temp, temp)
temp <- rbind(temp, total_full[which(
  total_full$ParticipantID %in% baseline$filename==T), ])
temp[, which(colnames(temp)=="ACC_LIFG"):length(temp)] <- ""
temp <- reorder(temp$filename)

rownames(temp) <- NULL


dependent <- "SN_FC"
explanatory <- c("AgeatBaseline",
                 "Gender",
                 "GeriatricDepressionScaleTotalScore",
                 "WASIFullScale.IQ.4Subtests",
                 "Mini.MentalStateExaminationTotalScore",
                 "MFQFOFInvertedAverage",
                 "BeckDepressionScaleTotalScore")

total_full %>%
  ff_glimpse(dependent, explanatory)

missing_plot(total_full, dependent, explanatory)

total0_full <- total_full[which(total_full$timepoint=="1"),]
total1_full <- total_full[which(total_full$timepoint=="2"),]
total2_full <- total_full[which(total_full$timepoint=="3"),]

# 
