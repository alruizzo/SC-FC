#!/bin/bash

## A.L.R.R. Aug 5th, 2020
## September 30th, 2020
## October 9, 2020

## Script to extract time series from ROIs for whole-brain analysis.

  # Set relevant paths
SEARCHFOLDER=/mnt/storage/Jessica_Lab/analyses/veni-lmu/individual_rois/
DATAFOLDER=/mnt/storage/Jessica_Lab/data/veni/subjects/
FUNCFILE=/scan-data/niftis/mb_rest/edited-mb-rest.feat/ICA_AROMA/denoised_func_data_nonaggr.nii.gz

  # Loop across participants' folders to extract the mean time series...
  # ...(fslmeants) using each of the ROIs (in native space) as masks. ...
  # ...The mean is weighed by the intensity value of the voxel within...
  # ...the mask
for participant in $SEARCHFOLDER*
do
  foldername=`basename $participant`
  echo "Process for $foldername:"
  cd $participant
  for mask in ${participant}/*
  do
    if [[ `basename ${mask}` == *"nii.gz" ]]
    then
      maskname=`basename $mask | sed 's/.nii.gz//'`
      echo "Now extracting time series of $maskname..."
      fslmeants -i $DATAFOLDER/${foldername}/$FUNCFILE -o $participant/wmeants_${maskname}.txt -m ${mask} -w
      echo "...done"
    fi
  done
  echo "...Time series extraction: done"
done

echo "* FINISHED $0 on $(date) *"
