####========================================= A.L.R.R.2020 - 2021
### DESCRIPTION
## This script performs group comparisons between the ROI Z-
## ...transformed correlations. It comes from the "group_...
## ...comparisons.R" script.


####==========================================================
### INSTALL PACKAGES
# install.packages("pacman")
# require(pacman)
# pacman::p_load(ggplot2, dplyr, ggthemes, ggvis, plotly,
#                rio, stringr, tidyr, readxl, ggpubr,
#                psych, car, tidyverse, rstatix,
#                RColorBrewer, Hmisc, DescTools)


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


## Box plots of average SN FC per time point

  # Average FC T0
ggplot(total_t0,
       aes(x=is_SCD, y=SN_FC_1,
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
ggsave("../figures/boxplot_avr_T0.jpg", width = 30,
       height = 20, units = "cm", dpi = 400)

  # Average FC T1
ggplot(total_t1,
       aes(x=is_SCD, y=SN_FC_2,
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
ggsave("../figures/boxplot_avr_T1.jpg", width = 30,
       height = 20, units = "cm", dpi = 400)

  # Average FC T2
ggplot(total_t2,
       aes(x=is_SCD, y=SN_FC_3,
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
ggsave("../figures/boxplot_avr_T2.jpg", width = 30,
       height = 20, units = "cm", dpi = 400)
