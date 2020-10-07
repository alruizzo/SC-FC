#!/bin/bash

## A.L.R.R. Aug 14, 2020; September 30, 2020
## Script to run whole-brain dual regression for each ROI for each participant.

SEARCHFOLDER=/mnt/storage/Jessica_Lab/analyses/veni-lmu/participants/
DATAFOLDER=/mnt/storage/Jessica_Lab/data/veni/subjects/
FUNCFILE=/scan-data/niftis/mb_rest/edited-mb-rest.feat/ICA_AROMA/denoised_func_data_nonaggr.nii.gz

for participant in $SEARCHFOLDER*
do
  foldername=`basename $participant`
  echo "Process for $foldername..."
  cd $participant
  for mask in ${participant}/*
  do
    if [[ `basename ${mask}` == *".nii.gz" ]]
    then
      maskname=`basename $mask | sed 's/_std_2mm.nii.gz//'`
      echo "...Now computing WB-DR of $maskname..."
      dual_regression $mask 0 -1 0 SCA_DR_$maskname $DATAFOLDER/${foldername}/$FUNCFILE
      echo "...done"
    fi
  done
done

echo "* FINISHED $0 ON $(date)*"
