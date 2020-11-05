#!/bin/bash

## A.L.R.R. November 5, 2020
## Script to reslice MNI 1mm ROIs to MNI 2mm ROIs to prepare them for next...
## ...reslice to native (functional) space

## Get paths/directories
source var_names.sh

## Create Output dir if non-existent
mkdir -p $OUTPUTFOLDER

## Loop to extract the reslice from 1mm to 2mm
for nii in $INPUTFOLDER*
do
  roiname=`basename $nii | sed 's/.nii.gz//'`
  echo $roiname
  flirt -usesqform -in $nii -ref $STD_TEMPLATE -applyxfm -out $OUTPUTFOLDER/temp${roiname}_std_2mm.nii.gz
  fslmaths $OUTPUTFOLDER/temp${roiname}_std_2mm.nii.gz -thr 0.3 -bin $OUTPUTFOLDER/${roiname}_std_2mm.nii.gz
done

rm $OUTPUTFOLDER/temp*

echo "* FINISHED $0 on $(date) *"
