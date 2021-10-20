#### DESCRIPTION ====================================================
# A.L.R.R.2021 - GitHub: @alruizzo


#### INSTALL PACKAGES ===============================================
 # install.packages("pacman")
 # require(pacman)
pacman::p_load(ggplot2, dplyr, ggthemes, ggvis, plotly,
               tidyr, readxl, ggpubr, psych, car, GGally,
               tidyverse, rstatix, Hmisc, lme4,
               magrittr, mice, MVN, naniar, dvmisc)


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


#### MISSINGNESS =================================================
# Create graph with missing pattern (when using "AvrFC")
jpeg("missing_pattern.jpeg", width = 3220,
     height = 1880, res = 300)
missing <- md.pattern(data_wide, rotate.names = T)
dev.off()

# Little's test (mcar_test) from the "naniar" pckg
# Missing completely at random
mcar_test(data_wide[,
                    unlist(lapply(data_wide, is.numeric))])

# Dummy variables (1 = missing; 0 = no)
data_wide_missing <-
  data.frame(ifelse(is.na(
    data_wide[, c(grep("fminor", colnames(data_wide)),
                  grep("AvrF", colnames(data_wide)))]), "1", "0"))

# Convert to factor the variables
data_wide_missing <- data.frame(lapply(data_wide_missing,
                            factor))

# Add independent variables
data_wide_missing <- cbind(data_wide[, c(1:5)],
                           data_wide_missing)


#### DESCRIPTIVE PLOTS: STRUCTURAL CONNECTIVITY =====================
# ICVF
ggplot(data,
       aes(x = timepoint,
           y = fminor_ICVF, # "fminor_ICVF", "slfp", "unc", or "atr"
           group = subjects,
           fill = is_SCD)
) + scale_fill_manual(values = c(
         "#f1a340", "#998ec3")
) + scale_x_discrete(name = "Timepoints",
                     labels=c("1" = "T0",
                              "2" = "T1",
                              "3" = "T2")
) + scale_y_continuous(name = "Structural Connectivity - SN ICVF",
                       limits = c(0.4, 0.7)
) + theme_bw(
) + theme(panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          legend.box.background = element_rect(),
          axis.line = element_line(color = 'black',
                           size = 0.4),
          axis.ticks.x = element_blank(),
          axis.text = element_text(size = 24),
          axis.title = element_text(size = 24,
                            face = "bold")
) + geom_point(aes(fill = is_SCD),
               size = 2, alpha = 0.8,
               shape = 21
) + geom_line(aes(color = is_SCD),
              alpha = 0.5, size = 0.9
) + scale_color_manual(values = c(
  "#f1a340", "#998ec3")
) + labs(color = "Group", fill = "Group"
) + stat_summary(aes(group = is_SCD, color = is_SCD),
                 geom = "line", fun = mean, size = 2.5
# ) + stat_summary(aes(group = 1),
#                  geom = "line", fun = mean, size = 1
) + geom_smooth(aes(group = is_SCD,
                    color = is_SCD),
                alpha = 0.3)
ggsave("indiv_points_ICVF.jpg", width = 30,
       height = 20, units = "cm", dpi = 400)


#### DESCRIPTIVE PLOTS: FUNCTIONAL CONNECTIVITY =====================
# Avr FC SN
ggplot(data,
       aes(x = timepoint,
           y = AvrFC, #AvrFC, ACC_LMFG, ACC_RMFG, ACC_LINS, ACC_RINS
           group = subjects,
           fill = is_SCD)
) + scale_fill_manual(values = c(
  "#f1a340", "#998ec3")
) + scale_x_discrete(name = "Timepoints",
                     labels=c("1" = "T0",
                              "2" = "T1",
                              "3" = "T2")
) + scale_y_continuous(name = "Average FC CON",
                       limits = c(0, 1.5, 0.25)
) + theme_bw(
) + theme(panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          legend.box.background = element_rect(),
          axis.line = element_line(color = 'black',
                                   size = 0.4),
          axis.ticks.x = element_blank(),
          axis.text = element_text(size = 24),
          axis.title = element_text(size = 24,
                                    face = "bold")
) + geom_point(aes(fill = is_SCD),
               size = 2, alpha = 0.8,
               shape = 21
) + geom_line(aes(color = is_SCD),
              alpha = 0.5, size = 0.9
) + scale_color_manual(values = c(
  "#f1a340", "#998ec3")
) + labs(color = "Group", fill = "Group"
# ) + stat_summary(aes(group = "subjects"),
#                  geom = "line", fun = mean, size = 1, lty = 2)
) + stat_summary(aes(group = is_SCD, color = is_SCD),
                 geom = "line", fun = mean, size = 2.5
) + geom_smooth(aes(group = is_SCD,
                    color = is_SCD),
                alpha = 0.3)
ggsave("indiv_points_AvrFC.jpg", width = 30,
       height = 20, units = "cm", dpi = 400)


#### MAHALANOBIS DISTANCE ===========================================
# To check for multivariate outliers (use with AvrFC only!)

# Variance-covariance matrix
covar_matrix <- round(cov(
  data_wide[, unlist(lapply(data_wide,
                                    is.numeric))],
  use = "pairwise.complete.obs"), 3)

# Generalized variance
det(covar_matrix)

# Correlation matrix
corr_matrix <- round(cor(
  data_wide[, unlist(lapply(data_wide,
                                    is.numeric))],
  use = "pairwise.complete.obs"), 3)

# Means vector
means_vector <-
  round(apply(
    data_wide[, unlist(lapply(data_wide, is.numeric))], 2,
    mean, na.rm = T), 3)

# Ordering the correlation values
ind <- order(abs(corr_matrix[lower.tri(corr_matrix,
                                       diag = F)]),
             decreasing = T)
val <-
  as.vector(corr_matrix[lower.tri(corr_matrix,
                                  diag = F)])[order(
                                    abs(
                                      corr_matrix[lower.tri(
                                        corr_matrix,
                                        diag = F)]),
                                    decreasing = T)]
rbind("index" = ind, "coeff" = val)

# calculate the Mahalanobis distance (for complete cases, though)
mahalanobisd <- sqrt(mahalanobis(data_wide[,unlist(lapply(
  data_wide, is.numeric))], means_vector, covar_matrix))
mahalanobisd[order(mahalanobisd, decreasing = T)]

# Assess significance with chisq probability distribution
dmahalanobisp <- cbind(data_wide[, unlist(lapply(data_wide,
    is.numeric))], "DM" = round(mahalanobisd, 3),
    "Prob" = round(pchisq(mahalanobisd,
                          length(data_wide[, unlist(
            lapply(data_wide, is.numeric))])), 4))
subset(dmahalanobisp, dmahalanobisp["Prob"] < 0.001)

# Check graphically in a boxplot
boxplot(mahalanobisd)

# MULTIVARIATE NORMALITY (package MVN)
# Henze-Zirkler's test
mvn(data = data_wide[, unlist(lapply(data_wide,is.numeric))],
    mvnTest = "hz")

# Mardia's test
mvn(data = data_wide[, unlist(lapply(data_wide,is.numeric))],
    mvnTest = "mardia")

# Royston's test
jpeg("qqplot_mvn.jpeg", width = 3220,
          height = 1880, res = 300)
mvn(data = data_wide[, unlist(lapply(data_wide, is.numeric))],
    mvnTest = "royston",
    univariatePlot = "qqplot")
dev.off()


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


#### MIXED EFFECTS MODEL ============================================
# Depression
gds_mixed = lmer(GeriatricDepressionScaleTotalScorewithoutQuestion14 ~
                   timepoint + MFQFOFInvertedAverage +
                   timepoint*MFQFOFInvertedAverage +
                   (1 | ParticipantID),
                 data = neuropsy)
summary(gds_mixed)
confint(gds_mixed)

# Neuroticism
neurot_mixed = lmer(BigFiveInventoryNeuroticismTotalScore ~
                   timepoint + MFQFOFInvertedAverage +
                   timepoint*MFQFOFInvertedAverage +
                   (1 | ParticipantID),
                 data = neuropsy, REML=F)
summary(neurot_mixed)
confint(neurot_mixed)

gds_mixed = lmer(MFQFOFInvertedAverage ~ AgeatBaseline +
                   Gender + Mini.MentalStateExaminationTotalScore +
                   timepoint +
                   BigFiveInventoryConscientiousnessTotalScore +
                   BigFiveInventoryNeuroticismTotalScore +
                   GeriatricDepressionScaleTotalScorewithoutQuestion14 +
                   (1 | ParticipantID),
                 data = neuropsy, REML=F)
summary(scd_mixed)
confint(scd_mixed)
