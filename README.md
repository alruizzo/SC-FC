# **SC-FC**
## **Structural and Functional Connectivity Project**

### **Aim**

Compute ROI-to-ROI functional connectivity of the salience/cingulo-opercular network for correlation with structural connectivity of this network in healthy older adults (with and without subjective cognitive decline).

### **Scripts**

- Looping BASH scripts to apply FSL functions (v.6.0.1) across participants to obtain **ROIs' time courses**.

- The **ROI-to-ROI functional connectivity** was computed in R (see the corresponding R scripts).

- The **script numbering** reflects the order of analysis steps followed.

### **ROI selection**

- ROIs were **obtained and selected** from the 360-area MNI map derived from the multimodal parcellation of Glasser et al. (2016).

- ROIs were **visually inspected** to overlap with the anterior salience network map of Shirer et al. (2012).

- ROI **time course extraction** was run on Ubuntu 16.04.5 LTS (Xenial Xerus) with FSL v.6.0.1.

### **Report**

- Theory, method description, and results can be found [here](https://doi.org/10.1016/j.neuroimage.2022.119662).
