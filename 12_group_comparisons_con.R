####========================================= A.L.R.R.2020 - 2021
### DESCRIPTION
## This script performs group comparisons between the con ROI...
## ... Z-transformed correlations


####==========================================================
### INSTALL PACKAGES
# install.packages("pacman")
# require(pacman)
# pacman::p_load(ggplot2, dplyr, ggthemes, ggvis, plotly,
#                rio, stringr, tidyr, readxl, ggpubr,
#                psych, car, tidyverse, rstatix, cocor,
#                ppcor, RColorBrewer, Hmisc, DescTools)


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
  total <- read.csv("CON-ROI-FC-all.csv",
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
### SELECTED PARTICIPANTS
## Exclude participants according to exclusion criteria...
## ..."exclude" data frame.

# Get the demographics file or data frame if existent
if (!exists('exclude')){
  exclude <- data.frame(read.csv(
    "Partic_excluded.txt",
    sep = "\t"))
}

# Create character variable for next step
Excluded <- exclude$participants

# Filter out excluded participants from "total"
total <- total[-which(
  total$filename %in% Excluded==TRUE),]
rownames(total) <- NULL

# Save two data frames (1) with and (2) without MCI
total_MCI <- total
total <- total[-which(total$is_SCD=="MCI"),]
total$is_SCD <- factor(total$is_SCD)
levels(total$is_SCD)["SCD"] <- "SCD"
levels(total$is_SCD)["CON"] <- "CON"
total$is_SCD <- factor(total$is_SCD,
                       levels = c("SCD", "CON"))


####==========================================================
### TIME POINT FILES

## "Total" file in wide format separately for each time point
# Baseline (T0)
total_t0 <- total[which(total$timepoint==1),]
  rownames(total_t0) <- NULL

# T1
total_t1 <- total[which(total$timepoint==2),]
  rownames(total_t1) <- NULL
  
# T2
total_t2 <- total[which(total$timepoint==3),]
  rownames(total_t2) <- NULL

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

## Set "filename" column to having the same name across...
## ...data frames
total_t0$filename <- substr(total_t0$filename, 1, 12)
total_t1$filename <- substr(total_t1$filename, 1, 12)
total_t2$filename <- substr(total_t2$filename, 1, 12)


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
  
  # Delete duplicated variables
total_wide <- total_wide[, -which(
  duplicated(colnames(total_wide))==TRUE)]

## Delete the previously-created "temp" to clean work space
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
  cols = LPIN_RPIN_1:RPIN_RINS_3,
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
  # Taking all ROI pairs
res.aov.total_all_long <- anova_test(data = total_all_long,
                               dv = Z_FC,
                               wid = filename,
                               between = is_SCD,
                               within = c(timepoint,
                                          ROI_pair),
                               effect.size = "pes")
get_anova_table(res.aov.total_all_long,
                correction = "auto")

  # Taking only the relevant pair: LPIN_RPIN
res.aov.total_all_long2 <- anova_test(
  data = total_all_long[
  which(total_all_long$ROI_pair=="LPIN_RPIN") ,],
  dv = Z_FC,
  wid = filename,
  between = is_SCD,
  within = timepoint,
  effect.size = "pes")
get_anova_table(res.aov.total_all_long2,
                correction = "auto")


####==========================================================
### DATA PREPARATION FOR SEPARATE MIXED ANOVAS
### ...WITHIN EACH TIME POINT
## Transform data frame for mixed model

# Baseline / T0
total_long_t0 <- total_all_long[which(
  total_all_long$timepoint==1 &
    is.na(total_all_long$Z_FC)==F), ]

# T1
total_long_t1 <- total_all_long[which(
  total_all_long$timepoint==2 &
    is.na(total_all_long$Z_FC)==F), ]

# T2
total_long_t2 <- total_all_long[which(
  total_all_long$timepoint==3 &
      is.na(total_all_long$Z_FC)==F), ]


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
res.aov.baseline <- anova_test(data = total_long_t0,
                             dv = Z_FC,
                             wid = filename,
                             between = is_SCD,
                             within = ROI_pair,
                             effect.size = "pes")
get_anova_table(res.aov.baseline,
                correction = "auto")

# T1
res.aov.t1 <- anova_test(data = total_long_t1,
                               dv = Z_FC,
                               wid = filename,
                               between = is_SCD,
                               within = ROI_pair,
                               effect.size = "pes")
get_anova_table(res.aov.t1,
                correction = "auto")

# T2
res.aov.t2 <- anova_test(data = total_long_t2,
                         dv = Z_FC,
                         wid = filename,
                         between = is_SCD,
                         within = ROI_pair,
                         effect.size = "pes")
get_anova_table(res.aov.t2,
                correction = "auto")


####==========================================================
### FOLLOW-UP ONE-WAY ANOVAS BETWEEN GROUPS (SCD)
## One-way ANOVA to study significant interaction effects...
## ...separately per time point

# Within each WS condition, comparing between subjects (BS)
  # Baseline (T0)
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


####==========================================================
### FOLLOW-UP PAIRWISE T-TESTS OF POST INS ACROSS TIMEPOINTS
## Does the FC of the posterior insula (L and R) differ...
## ...between time points across all participants?

pwc <- total_all_long[which(
  total_all_long$ROI_pair=="LPIN_RPIN"), ] %>%
  pairwise_t_test(
    Z_FC ~ timepoint,
    pool.sd = FALSE,
    p.adjust.method = "holm"
  )
pwc


####==========================================================
### PLOTTING
## Box plots of ROI-to-ROI FC per SCD group per time point

# Baseline (T0)
ggplot(total_long_t0,
       aes(x=reorder(ROI_pair, Z_FC, FUN = median), y=Z_FC,
           fill=is_SCD)) + 
  geom_boxplot() + scale_fill_manual(values=c(
    "tomato", "yellow")
  ) + xlab("ROI pairs") + ylab(
    "Functional connectivity (Z)"
  ) + theme_bw() + ylim(-1, 3
  ) + theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text=element_text(size=12)
  ) + geom_vline(xintercept = 0.5:20,
                 color = "gray")
ggsave("./figures/boxplot_T0_all_con.jpg", width = 30,
       height = 20, units = "cm", dpi = 400)

# T1
ggplot(total_long_t1,
       aes(x=reorder(ROI_pair, Z_FC, FUN = median),
           y=Z_FC,
           fill=is_SCD)) + 
  geom_boxplot() + scale_fill_manual(values=c(
    "tomato", "yellow")
  ) + xlab("ROI pairs") + ylab(
    "Functional connectivity (Z)"
  ) + theme_bw() + ylim(-1, 3
  ) + theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text=element_text(size=12)
            ) + geom_vline(xintercept = 0.5:20,
                           color = "gray")
ggsave("./figures/boxplot_T1_con.jpg",
       width = 30,
       height = 20, units = "cm", dpi = 400)

# T2
ggplot(total_long_t2,
       aes(x=reorder(ROI_pair, Z_FC, FUN = median), y=Z_FC,
           fill=is_SCD)) + 
  geom_boxplot() + scale_fill_manual(values=c(
    "tomato", "yellow")
  ) + xlab("ROI pairs") + ylab(
    "Functional connectivity (Z)"
  ) + theme_bw() + ylim(-1, 3
  ) + theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text=element_text(size=12)
  ) + geom_vline(xintercept = 0.5:20,
                 color = "gray")
ggsave("./figures/boxplot_T2_con.jpg",
       width = 30,
       height = 20, units = "cm", dpi = 400)
