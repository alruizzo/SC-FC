#!/bin/bash

# A.L.R.R. November 5, 2020
# Script to reslice each ROI mask from MNI to the individual functional space

## Get directory paths
source var_names.sh

## Create output directory if it doesn't exist
mkdir -p $OUTPUTFOLDER

## Loop to go through each participant's folder and reslice the mask files...
## ...to each participant's functional native space
for folder in $DATAFOLDER*
do
  foldername=`basename $folder`
  if [ -d "$folder" ] # To exclude files and only include folders
  then
    if [[ $foldername == 'w'* ]] # To include only WSU folders
      then
        echo "creating $foldername..."
        mkdir $OUTPUTFOLDER/$foldername
        for mask in $ROIFOLDER*
        do
          maskname=`basename $mask | sed 's/_std_2mm.nii.gz//'`
          echo "...flirt on $maskname..."
          flirt -in $mask -ref $folder/${REGFOLDER}/example_func.nii.gz -applyxfm -init $folder/${REGFOLDER}/standard2example_func.mat -out $OUTPUTFOLDER/${foldername}/${maskname}.nii.gz
          echo "...done"
        done
      fi
  fi
done

## Delete empty participants' folders, as those are missing data (and...
## ...won't be used further)
find $OUTPUTFOLDER -type d -empty -delete

echo "* FINISHED $0 on $(date). See results in $OUTPUTFOLDER *"
