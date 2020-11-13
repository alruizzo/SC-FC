#!/bin/bash

# A.L.R.R. Aug 5, 2020
# September 30, 2020
# October 9, 2020

# Script to extract time series from ROIs for whole-brain analysis.

  ## Set relevant paths
source var_names.sh

  ## Loop across participants' folders to extract the mean time series...
  ## ...(fslmeants) using each of the ROIs (in native space) as masks. ...
  ## ...The mean is weighed by the intensity value of the voxel within...
  ## ...the mask. This process takes quite a while (though less than...
  ## ...randomise)
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

  ## End message
echo "* FINISHED $0 on $(date) *"
