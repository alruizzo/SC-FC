#!/bin/bash

# A.L.R.R. Nov 12, 2020

# Script to convert individual images to MNI space, create 4D files (all...
# ...subject MNI converted control ROI-WB FC), and run randomise on those...
# ...files to obtain group-specific (1stt) maps.

## Define paths
source var_names.sh

## Define the (control) ROIs and an array including them
roi1=09-LINS
roi2=12-RINS
ROIS=($roi1 $roi2)

## Define number of time points (according to longitudinal data)
TP=3

## Create the output folder if it doesn't exist
mkdir -p $OUTPUTFOLDER

## Loop through the participants' folder to warp the native whole-brain...
## ...images to MNI space to prepare them for randomise
for participant in $INPUTFOLDER*
do
  foldername=`basename $participant`
  echo "Process for $foldername"
  cd $participant
  for mask in ${participant}/*
  do
    maskname=`basename $mask`
    if [[ ${maskname} == SCA* ]]
    then
      maskname=`basename $mask | sed 's/SCA_DR_//'`
      cd $mask
      if [ ! -f ${maskname}_MNI.nii.gz ]
      then
        echo "flirt for $maskname..."
        flirt -usesqform -in $mask/dr_stage2_ic0000.nii.gz -ref $STD_TEMPLATE -applyxfm -init $DATAFOLDER/${foldername}/${REGFOLDER}/example_func2standard.mat -out $mask/${maskname}_MNI
        echo "...done"
      fi
    fi
  done
done

## Merge all participant's images per ROI and time point (within the ROI)...
## ...for randomization, looping through ROIs in the OUTPUTFOLDER and...
## ...time points
echo "Now continuing with fslmerge..."
for roi in ${ROIS[@]}
do
  mkdir -p $OUTPUTFOLDER/$roi
  echo "...Merging $roi across participants and time points..."
  for timepoint in $(seq 1 $TP)
  do
    echo "...Merging timepoint 0$timepoint..."
    fslmerge -t $OUTPUTFOLDER/${roi}/${roi}_0$timepoint $INPUTFOLDER/wsu-???-00??-0${timepoint}/SCA_DR_${roi}/${roi}_MNI.nii.gz
  echo "...done"
  done
done
echo "Merging of all ROIs, done"

## Loop for randomise through participant folder and time point while...
## ...using the MNI brain mask as mask
echo "Now starting randomise: (warning: it takes a while)"
for map in $OUTPUTFOLDER*
do
  foldername=`basename $map`
  cd $map
  for timepoint in ${map}/*
  do
    tpname=`basename $timepoint | sed 's/.nii.gz//'`
      echo "Randomising across $tpname..."
      randomise -i $timepoint -o ${tpname}_1stt -1 -T -m $MNIBRAIN2MM/MNI152_T1_2mm_brain_mask.nii.gz
      echo "...done"
  done
done

## End message
echo "* FINISHED $0 ON $(date) *"
