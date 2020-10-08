#!/bin/bash

## A.L.R.R. October 8, 2020

## Script to check which participants' folders have fewer files than they...
## ..should. Step after WB-DR

  # Define paths
SEARCHFOLDER=/mnt/storage/Jessica_Lab/analyses/veni-lmu/individual_rois/
OUTPUTFILE=/mnt/storage/Jessica_Lab/analyses/veni-lmu/checkfiles.txt

  # Loop through the participants' folder to warp the native whole-brain...
  #...images to MNI space to prepare them for randomise
for participant in $SEARCHFOLDER*
do
  foldername=`basename $participant`
  cd $participant
  for mask in ${participant}/*
  do
    maskname=`basename $mask`
    if [[ ${maskname} == SCA* ]]
    then
      cd $mask
      nrfiles=`ls | wc -l`
      if [ $nrfiles -lt 8 ]
      then
        echo "For $foldername:" >> $OUTPUTFILE
        echo "$maskname folder *only* has $nrfiles file" >> $OUTPUTFILE
      fi
    fi
  done
done

echo "* FINISHED '$0' on $(date). Results in $OUTPUTFILE * "
