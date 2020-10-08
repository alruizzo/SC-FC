#!/bin/bash

## A.L.R.R. Aug 17, 2020
## October 8, 2020

## Script to convert individual images to MNI space, create 4D files (all...
## ...subject MNI converted ROI-WB FC), and run randomise on those files...
##...to obtain group-specific (1stt) maps.

  # Define paths
SEARCHFOLDER=/mnt/storage/Jessica_Lab/analyses/veni-lmu/individual_rois/
DATAFOLDER=/mnt/storage/Jessica_Lab/data/veni/subjects/
REGFOLDER=/scan-data/niftis/mb_rest/edited-mb-rest.feat/reg/
OUTPUTFOLDER=/mnt/storage/Jessica_Lab/analyses/veni-lmu/roi-wb/
MNIBRAIN2MM=/mnt/storage/Jessica_Lab/analyses/veni-lmu/masks/functional/

  # Create the output folder if it doesn't exist
mkdir -p $OUTPUTFOLDER

  # Loop through the participants' folder to warp the native whole-brain...
  #...images to MNI space to prepare them for randomise
for participant in $SEARCHFOLDER*
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
      echo "flirt for $maskname..."
      flirt -usesqform -in $mask/dr_stage2_ic0000.nii.gz -ref $MNIBRAIN2MM/MNI152_T1_2mm_brain.nii.gz -applyxfm -init $DATAFOLDER/${foldername}/${REGFOLDER}/example_func2standard.mat -out $mask/${maskname}_MNI
      echo "...done"
    fi
  done
done

  # Merge all participant's images per ROI and time point (within the ROI)...
  #...for randomization
    #LIFG ROI
 mkdir -p $OUTPUTFOLDER/LIFG
 fslmerge -t $OUTPUTFOLDER/LIFG/LIFG_01 $SEARCHFOLDER/wsu-???-00??-01/SCA_DR_01-LIFG_/01-LIFG_MNI.nii.gz
 fslmerge -t $OUTPUTFOLDER/LIFG/LIFG_02 $SEARCHFOLDER/wsu-???-00??-02/SCA_DR_01-LIFG_/01-LIFG_MNI.nii.gz
 fslmerge -t $OUTPUTFOLDER/LIFG/LIFG_03 $SEARCHFOLDER/wsu-???-00??-03/SCA_DR_01-LIFG_/01-LIFG_MNI.nii.gz

    #LINS ROI
 mkdir -p $OUTPUTFOLDER/LINS
 fslmerge -t $OUTPUTFOLDER/LINS/LINS_01 $SEARCHFOLDER/wsu-???-00??-01/SCA_DR_02-LINS_/02-LINS_MNI.nii.gz
 fslmerge -t $OUTPUTFOLDER/LINS/LINS_02 $SEARCHFOLDER/wsu-???-00??-02/SCA_DR_02-LINS_/02-LINS_MNI.nii.gz
 fslmerge -t $OUTPUTFOLDER/LINS/LINS_03 $SEARCHFOLDER/wsu-???-00??-03/SCA_DR_02-LINS_/02-LINS_MNI.nii.gz

    #ACC ROI
 mkdir -p $OUTPUTFOLDER/ACC
 fslmerge -t $OUTPUTFOLDER/ACC/ACC_01 $SEARCHFOLDER/wsu-???-00??-01/SCA_DR_03-ACC_/03-ACC_MNI.nii.gz
 fslmerge -t $OUTPUTFOLDER/ACC/ACC_02 $SEARCHFOLDER/wsu-???-00??-02/SCA_DR_03-ACC_/03-ACC_MNI.nii.gz
 fslmerge -t $OUTPUTFOLDER/ACC/ACC_03 $SEARCHFOLDER/wsu-???-00??-03/SCA_DR_03-ACC_/03-ACC_MNI.nii.gz

    #RIFG ROI
 mkdir -p $OUTPUTFOLDER/RIFG
 fslmerge -t $OUTPUTFOLDER/RIFG/RIFG_01 $SEARCHFOLDER/wsu-???-00??-01/SCA_DR_04-RIFG_/04-RIFG_MNI.nii.gz
 fslmerge -t $OUTPUTFOLDER/RIFG/RIFG_02 $SEARCHFOLDER/wsu-???-00??-02/SCA_DR_04-RIFG_/04-RIFG_MNI.nii.gz
 fslmerge -t $OUTPUTFOLDER/RIFG/RIFG_03 $SEARCHFOLDER/wsu-???-00??-03/SCA_DR_04-RIFG_/04-RIFG_MNI.nii.gz

    #RINS ROI
 mkdir -p $OUTPUTFOLDER/RINS
 fslmerge -t $OUTPUTFOLDER/RINS/RINS_01 $SEARCHFOLDER/wsu-???-00??-01/SCA_DR_05-RINS_/05-RINS_MNI.nii.gz
 fslmerge -t $OUTPUTFOLDER/RINS/RINS_02 $SEARCHFOLDER/wsu-???-00??-02/SCA_DR_05-RINS_/05-RINS_MNI.nii.gz
 fslmerge -t $OUTPUTFOLDER/RINS/RINS_03 $SEARCHFOLDER/wsu-???-00??-03/SCA_DR_05-RINS_/05-RINS_MNI.nii.gz


 # Loop for randomise through participant folder and time point while...
 # ...using the MNI brain as mask
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

echo " * FINISHED $0 ON $(date)* "
