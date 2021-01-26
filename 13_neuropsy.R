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
#                permuco, CorrMixed)


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
neuropsy[, c(3:4, which(
  colnames(neuropsy)=="GeriatricDepressionScaleTotalScore"
  ):length(neuropsy))] <- sapply(neuropsy[, c(3:4, which(
    colnames(neuropsy)=="GeriatricDepressionScaleTotalScore"
  ):length(neuropsy))], as.numeric)
sapply(neuropsy, class)
neuropsy$Gender <- factor(neuropsy$Gender)

# Select the same participants as in 'total'
neuropsy <- neuropsy[which(neuropsy$ParticipantID %in% paste(
    total$filename, "-", "0", total$timepoint,
    sep = "") == TRUE), ]
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

# Reorganize 'neuropsy' (hard coded; be careful!!!)
neuropsy <- neuropsy[, c(1, 2, 43, 3:42)]

# Add Age to 'total'
total <- cbind(total, neuropsy$AgeatTesting)
colnames(total)[12] <- "Age"


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

# Adjust column names, especially to remove duplicates
# Remove the ".y" part from the column names
total_wide <- total_wide[, -which(
  grepl(".y", colnames(total_wide))==TRUE)]

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
  values_drop_na = TRUE)

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
  values_drop_na = TRUE)

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
    p.adjust.method = "holm"
  )
pwc[which(pwc$p.adj.signif!="ns"),]

# 'Age:timepoint' effect
cor_t0 <- rcorr(as.matrix(total_t0[, 3:11])); cor_t0
cor_t1 <- rcorr(as.matrix(total_t1[, 3:11])); cor_t1
cor_t2 <- rcorr(as.matrix(total_t2[, 3:11])); cor_t2

# 'SCD:timepoint' effect
# Across Groups (looking at the effect of 'timepoint')
ws <- total_all_long[which(total_all_long$is_SCD=="SCD"),] %>%
  group_by(ROI_pair) %>%
  anova_test(dv = Z_FC, wid = filename,
             within = timepoint, covariate = Age,
             effect.size = "pes") %>%
  get_anova_table() %>% adjust_pvalue(
    method = "holm"); ws
ws <- total_all_long[which(total_all_long$is_SCD=="CON"),] %>%
  group_by(ROI_pair) %>%
  anova_test(dv = Z_FC, wid = filename,
             within = timepoint, covariate = Age,
             effect.size = "pes") %>%
  get_anova_table() %>% adjust_pvalue(
    method = "holm"); ws

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
# DESCRIPTIVES

# Age at testing difference
total %>% group_by(timepoint) %>%
  t_test(Age ~ is_SCD)

neuropsy %>% group_by(timepoint) %>%
  t_test(AgeatBaseline ~ is_SCD)

describeBy(neuropsy$AgeatBaseline, neuropsy$is_SCD)
describeBy(neuropsy$AgeatTesting[which(
  neuropsy$timepoint=="1")], neuropsy$is_SCD[which(
    neuropsy$timepoint=="1")])

describeBy(neuropsy$AgeatTesting[which(
  neuropsy$timepoint=="1")], neuropsy$is_SCD[which(
    neuropsy$timepoint=="1")])



####==========================================================
# PLOTTING
# Box plots of average SN FC per time point

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

# Average FC
ggplot(total,
       aes(x = timepoint, y = CON_FC,
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
