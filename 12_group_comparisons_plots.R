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
#                psych, car, tidyverse, rstatix,
#                RColorBrewer, Hmisc, DescTools)


####==========================================================
### REQUIRED FILES
## Read text files and create temporal data frames for
## ...correlation
if (!exists('total')){
  total <- read.csv("../ROI-FC-all.csv",
           header = T, row.names = 1)
}


####==========================================================
### REPEATED MEASURES
## Transform data frame for repeated measures

# overall long
total_long <- pivot_longer(total, names_to = "ROI_pair",
             cols = LINS_LIFG:RINS_RIFG,
             values_to = "Z_FC")

# baseline (T0)
 # All (including MCI)
total_long_t0_all <- total_long[which(total_long$timepoint==1), ]
 # Only SCD and CON
total_long_t0 <- total_long[which(total_long$timepoint==1 &
                                    total_long$is_SCD!="MCI"), ]
total_long_t0$is_SCD <- factor(total_long_t0$is_SCD,
                               levels = c("SCD", "CON"))

# T1
total_long_t1 <- total_long[which(total_long$timepoint==2), ]
total_long_t1$is_SCD <- factor(total_long_t1$is_SCD,
                               levels = c("SCD", "CON"))

# T2
total_long_t2 <- total_long[which(total_long$timepoint==3), ]
total_long_t2$is_SCD <- factor(total_long_t2$is_SCD,
                               levels = c("SCD", "CON"))


####==========================================================
### PLOTTING
## Box plots of ROI-to-ROI FC per SCD group per time point

# Baseline (T0)
 # All
ggplot(total_long_t0_all,
       aes(x=reorder(ROI_pair, Z_FC, FUN = median), y=Z_FC,
           fill=is_SCD)) + 
  geom_boxplot() + scale_fill_manual(values=c(
    "slateblue1", "tomato", "yellow")
  ) + xlab("ROI pairs") + ylab(
    "Functional connectivity (Z)"
  ) + theme_bw() + ylim(-1, 3
                        ) + theme(panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank(),
                         axis.text=element_text(size=12)
                         ) + geom_vline(xintercept = 0.5:20,
                                        color = "gray")
ggsave("../figures/boxplot_T0_all.jpg", width = 30,
       height = 20, units = "cm", dpi = 400)

  # Without MCI (SCD and CON only)
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
ggsave("../figures/boxplot_T0_all.jpg", width = 30,
       height = 20, units = "cm", dpi = 400)

# T1
ggplot(total_long_t1,
       aes(x=reorder(ROI_pair, Z_FC, FUN = median), y=Z_FC,
           fill=is_SCD)) + 
  geom_boxplot() + scale_fill_manual(values=c(
    "tomato", "yellow")
    ) + xlab("ROI pairs") + ylab(
      "Functional connectivity (Z)"
      ) + theme_bw() + ylim(-1, 3
      ) + theme(panel.grid.major = element_blank(),
panel.grid.minor = element_blank(),
axis.text=element_text(size=12)) + geom_vline(xintercept = 0.5:20,
                                                 color = "gray")
ggsave("../figures/boxplot_T1.jpg", width = 30,
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
ggsave("/cloud/project/figures/boxplot_T2.jpg", width = 30,
       height = 20, units = "cm", dpi = 400)


# Average FC

ggplot(total,
       aes(x=is_SCD, y=SN_FC,
           fill=is_SCD)) +
       #) + geom_violin(scale="count",
                                       #color = "gray") +
  geom_boxplot(#width = 0.5/length(unique(total_t1$is_SCD))
               ) + scale_fill_manual(values=c(
    "tomato", "yellow")
  ) + ylab("Average Functional Connectivity (Z)"
  ) + theme_bw() + ylim(0, 1.5
  ) + theme(axis.text=element_text(size=12),
            axis.title.x=element_blank()
  ) + geom_point(aes(fill = is_SCD), size = 2,
                 shape = 23, alpha = 0.5,
                 position = position_jitterdodge(
                   jitter.width = 0.1
                 ))
ggsave("/cloud/project/figures/boxplot_avr_T1.jpg", width = 30,
       height = 20, units = "cm", dpi = 400)
