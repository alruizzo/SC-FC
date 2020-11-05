#!/bin/bash

# A.L.R.R. Aug 14, 2020 / September 30, 2020 / October 7, 2020
# Script to run whole-brain (WB) dual regression (DR) for each region of...
# ...interest (ROI) for each participant.

## Get directory/file paths
source var_names.sh

## Loop to go through each participant's folder containing each ROI (in...
## ...native space) to use it as a mask in the dual regression aimed at...
## ...calculating the whole brain functional connectivity using that...
## ...ROI as seed ("$mask"), from the preprocessed fMRI data ("$FUNCFILE")
for participant in $SEARCHFOLDER*
do
  foldername=`basename $participant`
  echo "Process for $foldername"
  cd $participant
  for mask in ${participant}/*
  do
    if [[ `basename ${mask}` == *".nii.gz" ]]
    then
      maskname=`basename $mask | sed 's/_.nii.gz//'`
      echo "Now computing WB-DR of $maskname..."
      dual_regression $mask 0 -1 0 SCA_DR_$maskname $DATAFOLDER/${foldername}/$FUNCFILE
      echo "...done"
    fi
  done
done

echo "* FINISHED $0 ON $(date)*"
