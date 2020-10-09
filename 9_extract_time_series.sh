#!/bin/bash

## A.L.R.R. Aug 5th, 2020; September 30th, 2020
## Script to extract time series from ROIs for whole-brain analysis.

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
    if [[ `basename ${mask} == *"nii.gz" ]]
    then
      maskname=`basename $mask | sed 's/_std_2mm.nii.gz//'`
      echo "...Now extracting time series of $maskname..."
      fslmeants -i $DATAFOLDER/${foldername}/$FUNCFILE -o $participant/wmeants_${maskname}.txt -m ${mask} -w
    fi
  done
  echo "...Time series extraction: done"
done

echo "* FINISHED $0 on $(date)*"
