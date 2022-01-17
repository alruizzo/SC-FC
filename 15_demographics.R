#### DESCRIPTION ====================================================
# A.L.R.R.2021 - GitHub: @alruizzo


#### INSTALL PACKAGES ===============================================
 # install.packages("pacman")
 # require(pacman)
pacman::p_load(ggplot2, dplyr, ggthemes, ggvis, plotly,
               tidyr, readxl, ggpubr, psych, car, GGally,
               tidyverse, rstatix, Hmisc, lme4, gtsummary,
               magrittr, mice, MVN, naniar, dvmisc, gt)


#### GET MAIN FILE ==================================================
# Obtain and adjust the data frame

# Load the data
data <- read.csv("all_data_R.csv", header = T)

# Add the data frame variables to the path
attach(data)

# Create the SCD group (mainly for plotting)
data$is_SCD[grepl("con", data$subjects)] <- "CON" 
data$is_SCD[grepl("sci", data$subjects)] <- "SCD"

# Adjust the type of some of the variables
data$Gender <- factor(Gender)
data$timepoint <- factor(timepoint)
data$is_SCD <- factor(data$is_SCD, levels = c("SCD", "CON"))

# Add SC of specific white-matter tracts (ICFV):
# Load file
dwi <- read.csv(paste("sub_noddi_tract_mean_icvf_veni-lmu_",
                      "weighted_means_no_binarization.csv",
                      sep = ""))
dwi <- dwi[order(dwi$subjects),]
rownames(dwi) <- NULL

# Add specific tracts to data file for control analyses...
# ...(uncomment below as needed). Averaged across hemispheres
# # Superior longitudinal fasciculus (parietal)
# data$slfp[data$subjects %in% dwi$subjects] <-
#   apply(as.matrix(dwi[dwi$subjects %in% data$subjects,
#                       c("lh.slfp",
#                         "rh.slfp")]), 1, mean)
# # Uncinate fasciculus
# data$unc[data$subjects %in% dwi$subjects] <-
#   apply(as.matrix(dwi[dwi$subjects %in% data$subjects,
#                       c("lh.unc",
#                         "rh.unc")]), 1, mean)
# # Anterior thalamic radiation
# data$atr[data$subjects %in% dwi$subjects] <-
#   apply(as.matrix(dwi[dwi$subjects %in% data$subjects,
#                       c("lh.atr",
#                         "rh.atr")]), 1, mean)

# Delete the last two digits from the participant column
# ...to create unique (participant) factors
data$subjects <-
  substr(data$subjects, 1, (nchar(as.character(
    data$subjects[1])))-nchar("-01"))
data$subjects <- factor(data$subjects,
                        levels = unique(data$subjects))

# Create a column with the average FC (obtained...
# ...from all ROIs):
data$AvrFC <- ""

# Read the table containing all ROIs information
SN <- read.csv("ROI-FC-all.csv")
SN <- SN[-which(SN$is_SCD == "MCI"), -1]
# Remove data point without raw structural data:
SN <- SN[-which(SN$filename == "wsu-sci-0009-01"), ]
row.names(SN) <- NULL
# Check that the filenames of the two dfs correspond:
substr(SN$filename, 1, 12) == data$subjects
# Copy the information to the working data frame:
data$AvrFC <- SN$SN_FC

# Some data cleaning
rm(SN)
data_all <- data
# Comment if AvrFC will be used / uncomment if ROI pairs will...
# ...be used (control analyses)
data <- data[, -which(colnames(data) %in% c(
  "fminor_FA", "fminor_MD",
  "ACC_LMFG", "ACC_RMFG", "ACC_LINS", "ACC_RINS"))]
data <- cbind(data[, unlist(lapply(data, is.factor))],
                 data[, unlist(lapply(data, is.numeric))])


#### ADJUST DATA TO WIDE FORMAT  ====================================
# Create 'wide' data frame
data_wide <-
  pivot_wider(data,
              id_cols = which(colnames(data)=="subjects"),
              names_from = which(colnames(data)=="timepoint"),
              values_from = c(-which(
                colnames(data)=="subjects" |
                  colnames(data)=="timepoint")))

# Leave only baseline columns for: Gender, Age, MFQFOF, SCD
data_wide <- data_wide[, -c(which(
  colnames(data_wide)=="Gender_2" |
    colnames(data_wide)=="Gender_3" |
    colnames(data_wide)=="AgeatBaseline_2" |
    colnames(data_wide)=="AgeatBaseline_3" |
    colnames(data_wide)=="MFQFOFInvAver_2" |
    colnames(data_wide)=="MFQFOFInvAver_3" |
    colnames(data_wide)=="is_SCD_2" |
    colnames(data_wide)=="is_SCD_3"))]

# Add missing datum for participant 'sci-0019'; another one,
# ...'sci-0005', does not have this information
data_wide$MFQFOFInvAver_1[which(
  data_wide$subjects == "wsu-sci-0019")] <-
  data$MFQFOFInvAver[which(
    data$subjects == "wsu-sci-0019" &
      data$timepoint == 2)]


#### DESCRIPTIVES ================================================
# Descriptive information and plots
summary(data_wide)

# Plot matrix across all variables (and save if wanted)
ggpairs(data_wide[, -c(1:3)])
#ggsave("ggpairs.jpg", width = 30,
#       height = 20, units = "cm", dpi = 400)

# Plot matrix across all variables, but separated for timepoint
ggpairs(data[, -c(1:2, 4)],
        mapping = ggplot2::aes(colour = timepoint))


#### DATA LONG ===================================================
# Functional connectivity values for each ROI pair - this is...
# ...only for control analyses, as the "AvrFC" is used for the...
# ...main model

# This here requires leaving ROI pairs in (see line 91)
data_long_fc <-
  pivot_longer(data_wide,
               cols = c(grep("ACC", colnames(data_wide))),
               names_to = c("ROI_pair", "timepoint"),
               names_pattern = "(.*)_(.*)",
               values_to = "values")


#### LABELS USED IN LAVAAN ==========================================
# Check column names
colnames(data_wide)

# Change, type by type (to avoid errors) the column names
# ...of "MFQFOFInvAver_1"
colnames(data_wide) <- gsub("MFQFOFInvAver_1", "MFQ",
                            colnames(data_wide))

# Change, type by type (to avoid errors) the column names
# ...of "AgeatBaseline_1" and "Gender_1"
colnames(data_wide) <- gsub("AgeatBaseline_1", "Age",
                            colnames(data_wide))
colnames(data_wide) <- gsub("Gender_1", "Sex",
                            colnames(data_wide))

# Change, type by type (to avoid errors) the column names of SC
# Replace "fminor_ICVF_" below by "slfp_", "unc_", "atr_" as...
# ...needed (especially, for control analyses)
colnames(data_wide) <- gsub("fminor_ICVF_", "SC",
                            colnames(data_wide))

# Change, type by type (to avoid errors) the column names of FC
# Replace "AvrFC_" below by "ACC_LMFG_", "ACC_RMFG_", "ACC_LINS_"...
# ..., "ACC_RINS_" as needed (especially, for control analyses)
colnames(data_wide) <- gsub("AvrFC_", "AvrFC",
                            colnames(data_wide))


#### OBTAIN NEUROPSYCHOLOGICAL/DEMOGRAPHIC DATA =====================
# Get demographic and neuropsychological data for analysis

# Read file
neuropsy <- data.frame(read.csv("neuropsych_R.csv"))

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
neuropsy$RaceEthnicity <- factor(neuropsy$RaceEthnicity)

# Select the same participants as in 'data'
neuropsy <- neuropsy[which(neuropsy$ParticipantID %in% paste(
  data$subjects, "-", "0", data$timepoint,
  sep = "") == T), ]
rownames(neuropsy) <- NULL

# Change the participant filename to match up 'data'
neuropsy$ParticipantID <- substr(neuropsy$ParticipantID, 1, 12)

# Create an 'is_SCD' variable based on participant filename
neuropsy$is_SCD <- "CON"
neuropsy$is_SCD[which(grepl(
  "con",neuropsy$ParticipantID) == T)] <- "CON"
neuropsy$is_SCD[which(grepl(
  "sci",neuropsy$ParticipantID) == T)] <- "SCD" 
neuropsy$is_SCD <- factor(neuropsy$is_SCD,
                          levels = c("SCD", "CON"))

# Reorganize 'neuropsy' to have the most relevant variables first
neuropsy <- neuropsy[, c(which(
  colnames(neuropsy)=="ParticipantID"), which(
    colnames(neuropsy)=="timepoint"), which(
      colnames(neuropsy)=="is_SCD"), which(
        colnames(neuropsy)=="AgeatBaseline"):(length(
          neuropsy)-1))]


#### ADJUST MEASUREMENT DATE ========================================
# Calculate the measurement time difference for participants

# Delete the hour part of each cell, as it is uninformative
neuropsy$MRISessionDate <- gsub(" 00:00", "", neuropsy$MRISessionDate)

# Replace the dots with a dash (in order for it to be recognized with
# the function "as.Date")
neuropsy$MRISessionDate <-
  gsub(".", "-", neuropsy$MRISessionDate, fixed = T)

# Convert variable to date type
neuropsy$MRISessionDate <-
  as.Date(neuropsy$MRISessionDate, format = "%d-%m-%Y")

# Calculate days difference ("difftime")
neuropsy$time_measurements_days <- 0
for (participant in unique(neuropsy$ParticipantID)){
  if (length(which(neuropsy$ParticipantID==participant))==3) {
    rownr <- which(neuropsy$ParticipantID==participant)
    neuropsy$time_measurements_days[rownr[3]] <-
      as.numeric(neuropsy$MRISessionDate[rownr[3]] -
      neuropsy$MRISessionDate[rownr[2]])
    neuropsy$time_measurements_days[rownr[2]] <-
      as.numeric(neuropsy$MRISessionDate[rownr[2]] -
      neuropsy$MRISessionDate[rownr[1]])
  } else if (length(which(neuropsy$ParticipantID==participant))==2) {
    rownr <- which(neuropsy$ParticipantID==participant)
    neuropsy$time_measurements_days[rownr[2]] <-
      as.numeric(neuropsy$MRISessionDate[rownr[2]] -
      neuropsy$MRISessionDate[rownr[1]])
  }
}

# Recode days difference into months
neuropsy$time_measurements_mo <-
  (neuropsy$time_measurements_days / 365) * 12

# Create a list of values to compute the descriptives. Note that the
# two participants who didn't have the second measurement or those
# who didn't have follow-up are excluded not to affect the mean.
avr_measurement_dist_t1 <- neuropsy$time_measurements_mo[which(
  neuropsy$time_measurements_days < 1000 &
    neuropsy$time_measurements_days > 1)]
avr_measurement_dist_t2 <- neuropsy$time_measurements_mo[which(
  neuropsy$time_measurements_days < 1000 &
    neuropsy$time_measurements_days > 1 &
    neuropsy$timepoint == 2)]
avr_measurement_dist_t3 <- neuropsy$time_measurements_mo[which(
  neuropsy$time_measurements_days < 1000 &
    neuropsy$time_measurements_days > 1 &
    neuropsy$timepoint == 3)]

# Calculate the descriptives for the paper
# All time points
psych::describe(avr_measurement_dist_t1)
# From baseline to the second measurement
psych::describe(avr_measurement_dist_t2)
# From the second to the third measurement
psych::describe(avr_measurement_dist_t3)


### PARTICIPANT RETURN RATE =========================================
# Twenty participants were *not* invited for follow-up: con-0001 (1),
# con-0034 to con-0044 (11), and sci-0020 to sci-0027 (8)

invited <- 49

# Number of participants at baseline
length(data$subjects[which(data$timepoint == 1)])

# Number of participants from baseline, who returned at the first
# follow-up
length(data$subjects[which(
  data$subjects[which(
    data$timepoint == 2)] %in% data$subjects[which(
      data$timepoint == 1)] == T)]) / invited * 100

# Number of participants from the first follow-up, who returned at
# the second follow-up
length(data$subjects[which(
  data$subjects[which(
    data$timepoint == 3)] %in% data$subjects[which(
      data$timepoint == 2)] == T)]) / invited * 100

# Number of participants from baseline, who returned at the second
# follow-up
length(data$subjects[which(
  data$subjects[which(
    data$timepoint == 3)] %in% data$subjects[which(
      data$timepoint == 1)] == T)]) / invited * 100

# Number of participants from the first follow-up, who returned at
# the second follow-up
length(data$subjects[which(
  data$subjects[which(
    data$timepoint == 3)] %in% data$subjects[which(
      data$timepoint == 2)] == T)]) / invited * 100

# Identify participants from baseline who did not return for the
# first follow-up but did return for the second
data_wide$subjects[which(is.na(data_wide$AvrFC2) == T &
                           is.na(data_wide$AvrFC1) == F &
                           is.na(data_wide$AvrFC3) == F)]


#### DEMOGRAPHICS TABLE =============================================

# First, let's create another variable with baseline MFQ
neuropsy$MFQ <- neuropsy$MFQFOFInvertedAverage

# Let's only leave MFQ at baseline
for (participant in unique(neuropsy$ParticipantID)){
  if (length(which(neuropsy$ParticipantID==participant))==3) {
    rownr <- which(neuropsy$ParticipantID==participant)
  neuropsy$MFQ[rownr[2:3]] <-
      neuropsy$MFQ[rownr[1]]
  } else if (length(which(neuropsy$ParticipantID==participant))==2) {
    rownr <- which(neuropsy$ParticipantID==participant)
    neuropsy$MFQ[rownr[2]] <-
      neuropsy$MFQ[rownr[1]]
}}

# Let's manually add one value who had MFQ at the first follow-up
# but not at baseline
participant_MFQ <-
  neuropsy$ParticipantID[is.na(neuropsy$MFQ) &
                           !is.na(neuropsy$MFQFOFInvertedAverage)]
neuropsy$MFQ[which(neuropsy$ParticipantID==participant_MFQ)] <-
  neuropsy$MFQFOFInvertedAverage[which(
    neuropsy$ParticipantID==participant_MFQ & neuropsy$timepoint==2)]

# Let's now split the dataframe according to timepoint
baseline <- subset(neuropsy, timepoint == 1)
followup1 <- subset(neuropsy, timepoint == 2)
followup2 <- subset(neuropsy, timepoint == 3)

# Checking the MMSE values
baseline$ParticipantID[which(
  baseline$Mini.MentalStateExaminationTotalScore<25)]

# Next, check whether these values stay "low" in the ensuing
# measurements (for the corresponding participants)
# In the first follow-up:
followup1$Mini.MentalStateExaminationTotalScore[which(
  followup1$ParticipantID=="wsu-con-0030")]
# In the last follow-up:
followup2$Mini.MentalStateExaminationTotalScore[which(
  followup2$ParticipantID=="wsu-sci-0012")]


### MAKE DEMOGRAPHICS TABLE  ========================================

# Make dataset with variables to summarize
summ_vbles <- neuropsy %>% select("Age at baseline" = AgeatBaseline,
                            "Sex" = Gender,
                            "Education level" =
                              VENIHighest.DegreeEarned,
                            "Time since previous measurement (months)" =
                              time_measurements_mo,
                            "MFQ - Frequency of Forgetting" =
                              MFQFOFInvertedAverage,
                            "Family history of dementia" =
                              FamilyHistoryof.AlzheimersorDementia,
                            "Sought medical help due to SCD" =
                            DoctorSeen.forMemoryComplaints,
                            "Ethnicity" = 
                              RaceEthnicity,
                            "Big Five Inventory – Conscientiousness" =
                              BigFiveInventoryConscientiousnessTotalScore,
                            "Big Five Inventory – Neuroticism" =
                              BigFiveInventoryNeuroticismTotalScore,
                            "Geriatric Depression Scale" =
                              GeriatricDepressionScaleTotalScore,
                            "MiniMental State Examination (/30)" =
                              Mini.MentalStateExaminationTotalScore,
                            "General IQ" =
                              WASIFullScale.IQ.4Subtests,
                            "Rey Auditory Verbal Learning (total)" =
                              ReyAuditoryLearningTotalLearningScore,
                            "WMS Auditory Memory Index" =
                              WMSAuditoryMemoryIndex,
                            "WMS Visual Memory Index" =
                              WMSVisualMemoryIndex,
                            "WMS Visual Working Memory Index" =
                              WMSVisualWorkingMemoryIndex,
                            "WMS Immediate Memory Index" =
                              WMSImmediateMemoryIndex,
                            "WMS Delayed Memory Index" =
                              WMSDelayedMemoryIndex,
                            "Trail Making Test A (time in s)" =
                              TrailMakingTestATime,
                            "Trail Making Test B (time in s)" =
                              TrailMakingTestBTime,
                            "Digit Symbol Substitution (total score)" =
                              DigitSymbolSubstitutionTotal,
                            "Stroop Test (ratio score)" =
                              StroopTestRatioScore.StroopTimeColorTime.,
                            "Verbal Fluency (total score)" =
                              VerbalFluency.TestAnimalandOccupationTotal,
                            timepoint) %>%
  tbl_summary(by = "timepoint", missing = "ifany",
              type = list("MiniMental State Examination (/30)" ~ "continuous",
                          "Time since previous measurement (months)" ~
                            "continuous"),
              statistic = list(all_continuous() ~ "{mean} ({sd})"),
              digits = list(all_categorical() ~ c(0, 1),
                            all_continuous() ~ c(2, 2))) %>%
  modify_header(label = "**Variable**") %>%
  as_gt() %>% opt_table_font(font = "Arial") %>%
  tab_options(table_body.hlines.width = 0,
              column_labels.border.top.width = 2,
              column_labels.border.top.color = "black",
              column_labels.border.bottom.width = 2,
              column_labels.border.bottom.color = "black",
              table_body.border.bottom.color = "black",
              table.border.bottom.color = "white",
              table.font.size = px(14))
gtsave(summ_vbles, "demographic_table_raw.html")

# If we want to check proportion of genders for each time point
# Baseline
freq1 <- table(neuropsy$Gender[which(neuropsy$timepoint == 1)])
chisq.test(freq1)
# Follow-up 1
freq2 <- table(neuropsy$Gender[which(neuropsy$timepoint == 2)])
chisq.test(freq2)
# Follow-up 2
freq3 <- table(neuropsy$Gender[which(neuropsy$timepoint == 3)])
chisq.test(freq3)


### ADJUST NEUROPSY TO WIDE FORMAT  =================================
# Create 'wide' data frame
neuropsy_wide <-
  pivot_wider(neuropsy,
              id_cols = which(colnames(neuropsy)=="ParticipantID"),
              names_from = which(colnames(neuropsy)=="timepoint"),
              values_from = c(-which(
                colnames(neuropsy)=="ParticipantID" |
                  colnames(neuropsy)=="timepoint")))

# Leave only baseline columns for: Gender, Age, MFQFOF, SCD
neuropsy_wide <- neuropsy_wide[, -c(which(
  colnames(neuropsy_wide)=="Gender_2" |
    colnames(neuropsy_wide)=="Gender_3" |
    colnames(neuropsy_wide)=="RaceEthnicity_2" |
    colnames(neuropsy_wide)=="RaceEthnicity_3" |
    colnames(neuropsy_wide)=="is_SCD_2" |
    colnames(neuropsy_wide)=="is_SCD_3"))]


#### MIXED EFFECTS MODEL ============================================
# Variable time: coding baseline to 0
neuropsy$time <- as.numeric(neuropsy$timepoint) - 1

# MFQ
mfq_mixed <- lmer(MFQFOFInvertedAverage ~
                   time + is_SCD +
                   time*is_SCD +
                   (1 | ParticipantID),
                 data = neuropsy)
summary(mfq_mixed); confint(mfq_mixed)

# Depression
gds_mixed <- lmer(GeriatricDepressionScaleTotalScore ~
                   time + MFQFOFInvertedAverage +
                   time*MFQFOFInvertedAverage +
                   (1 | ParticipantID),
                 data = neuropsy)
summary(gds_mixed); confint(gds_mixed); anova(gds_mixed)

# Conscientiousness
cons_mixed <- lmer(BigFiveInventoryConscientiousnessTotalScore ~
                   time + MFQFOFInvertedAverage +
                   time*MFQFOFInvertedAverage +
                   (1 | ParticipantID),
                 data = neuropsy)
summary(cons_mixed); confint(cons_mixed); anova(cons_mixed)

# Neuroticism
neurot_mixed <- lmer(BigFiveInventoryNeuroticismTotalScore ~
                   time + MFQFOFInvertedAverage +
                   time*MFQFOFInvertedAverage +
                   (1 | ParticipantID),
                 data = neuropsy)
summary(neurot_mixed); confint(neurot_mixed); anova(neurot_mixed)

# MMSE
mmse_mixed = lmer(Mini.MentalStateExaminationTotalScore ~
                    time + MFQFOFInvertedAverage +
                    time*MFQFOFInvertedAverage +
                    (1 | ParticipantID),
                  data = neuropsy)
summary(mmse_mixed); confint(mmse_mixed)

# IQ
iq_mixed = lmer(WASIFullScale.IQ.4Subtests ~
                    time + MFQFOFInvertedAverage +
                    time*MFQFOFInvertedAverage +
                    (1 | ParticipantID),
                  data = neuropsy)
summary(iq_mixed); confint(iq_mixed)

# TMT
tmta_mixed = lmer(TrailMakingTestATime ~
                    time + MFQFOFInvertedAverage +
                    time*MFQFOFInvertedAverage +
                    (1 | ParticipantID),
                  data = neuropsy)
summary(tmta_mixed); confint(tmta_mixed)

# digit symbol
digit_mixed = lmer(DigitSymbolSubstitutionTotal ~
                    time + MFQFOFInvertedAverage +
                    time*MFQFOFInvertedAverage +
                    (1 | ParticipantID),
                  data = neuropsy)
summary(digit_mixed); confint(digit_mixed)

# rey
rey_mixed = lmer(ReyAuditoryLearningTotalLearningScore ~
                    time + MFQFOFInvertedAverage +
                    time*MFQFOFInvertedAverage +
                    (1 | ParticipantID),
                  data = neuropsy)
summary(rey_mixed); confint(rey_mixed)

# WMS (WMSAuditoryMemoryIndex, WMSVisualMemoryIndex,
# WMSVisualWorkingMemoryIndex, WMSImmediateMemoryIndex,
# WMSDelayedMemoryIndex)
wms_mixed <- lmer(WMSDelayedMemoryIndex ~
                    time + MFQFOFInvertedAverage +
                    time*MFQFOFInvertedAverage +
                    (1 | ParticipantID),
                  data = neuropsy)
summary(wms_mixed); confint(wms_mixed)

# tmtb
tmtb_mixed = lmer(TrailMakingTestBTime ~
                    time + MFQFOFInvertedAverage +
                    time*MFQFOFInvertedAverage +
                    (1 | ParticipantID),
                  data = neuropsy)
summary(tmtb_mixed); confint(tmtb_mixed)

# Stroop
stroop_mixed = lmer(StroopTestRatioScore.StroopTimeColorTime. ~
                    time + MFQFOFInvertedAverage +
                    time*MFQFOFInvertedAverage +
                    (1 | ParticipantID),
                  data = neuropsy)
summary(stroop_mixed); confint(stroop_mixed)

# Verbal fluency
verb_mixed <- lmer(VerbalFluency.TestAnimalandOccupationTotal ~
                    time + MFQFOFInvertedAverage +
                    time*MFQFOFInvertedAverage +
                    (1 | ParticipantID),
                  data = neuropsy)
summary(verb_mixed); confint(verb_mixed)

# Fminor
data$time <- as.numeric(data$timepoint) - 1
fminor_mixed <- lmer(fminor_ICVF ~
                    time + MFQFOFInvAver +
                    time*MFQFOFInvAver +
                    (1 | subjects),
                  data = data)
summary(fminor_mixed); confint(fminor_mixed); anova(fminor_mixed)

# AvrFC
fc_mixed <- lmer(AvrFC ~
                   time + MFQFOFInvAver +
                   time*MFQFOFInvAver +
                   (1 | subjects),
                 data = data)
summary(fc_mixed); confint(fc_mixed); anova(fc_mixed)


#### MIXED EFFECTS PLOTS ============================================
# Plot the interaction (to be able to understand it better)
# Depression
ggplot(data = neuropsy,
       aes(x = MFQFOFInvertedAverage,
           y = GeriatricDepressionScaleTotalScore,
           group = timepoint,
           color = timepoint)) +
  xlab("Baseline MFQ") +
  scale_y_continuous(name = "GDS") +
  theme_bw() +
  theme(text = element_text(size = 18, color = "black")) +
  stat_summary(aes(group = timepoint, color = timepoint),
               fun = mean) +
  geom_smooth(aes(color = timepoint, fill = timepoint),
              method = "lm", alpha = 0.3)

# Depression
ggplot(data = neuropsy,
       aes(x = MFQFOFInvertedAverage,
           y = GeriatricDepressionScaleTotalScore,
           group = timepoint,
           color = timepoint)) +
  xlab("Baseline MFQ") +
  scale_y_continuous(name = "GDS") +
  theme_bw() +
  theme(text = element_text(size = 18, color = "black")) +
  stat_summary(aes(group = timepoint, color = timepoint),
               fun = mean) +
  geom_smooth(aes(color = timepoint, fill = timepoint),
              method = "lm", alpha = 0.3)

# Conscientiousness
ggplot(data = neuropsy,
       aes(x = MFQFOFInvertedAverage,
           y = BigFiveInventoryConscientiousnessTotalScore,
           group = timepoint,
           color = timepoint)) +
  xlab("Baseline MFQ") +
  scale_y_continuous(name = "Conscientiousness") +
  theme_bw() +
  theme(text = element_text(size = 18, color = "black")) +
  stat_summary(aes(group = timepoint, color = timepoint),
               fun = mean) +
  geom_smooth(aes(color = timepoint, fill = timepoint),
              method = "lm", alpha = 0.3)
