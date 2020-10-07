#!/bin/bash

## A.L.R.R. July 30, 2020; September 29, 2020
## Script to reslice each ROI mask from MNI to the individual functional space

# Specifiy paths
SEARCHFOLDER=/mnt/storage/Jessica_Lab/data/veni/subjects/
OUTPUTFOLDER=/mnt/storage/Jessica_Lab/analyses/veni-lmu/participants/
MASKFOLDER=/mnt/storage/Jessica_Lab/analyses/veni-lmu/masks/functional/rois_combined_selected_std_2mm/

# Create output directory if it doesn't exist
mkdir -p $OUTPUTFOLDER

# Loop to go through each participant's folder and reslice the mask files to each participant's functional native space
for folder in $SEARCHFOLDER*
do
  foldername=`basename $folder`
  if [ -d "$folder" ] # To exclude files
  then
    if [[ $foldername == 'w'* ]] # To include only WSU folders
      then
        echo "creating $foldername..."
        mkdir $OUTPUTFOLDER/$foldername
        for mask in $MASKFOLDER*
        do
          maskname=`basename $mask | sed 's/.nii.gz//'`
          echo "...flirt on $maskname..."
          flirt -in $mask -ref $folder/scan-data/niftis/mb_rest/edited-mb-rest.feat/reg/example_func.nii.gz -applyxfm -init $folder/scan-data/niftis/mb_rest/edited-mb-rest.feat/reg/standard2example_func.mat -out $OUTPUTFOLDER/${foldername}/${maskname}.nii.gz
          echo "...done"
        done
      fi
  fi
done

# Delete empty participants' folders
find /mnt/storage/Jessica_Lab/analyses/veni-lmu/participants/ -type d -empty -delete

echo "* FINISHED $0. See results in $OUTPUTFOLDER *"

# Notes
# Path for EPI (*non*-multiband data)
#$folder/scan-data/niftis/ep2d_rest/unnormalized/fmri.feat/reg/example_func.nii.gz
