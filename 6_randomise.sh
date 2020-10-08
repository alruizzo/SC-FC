#!/bin/bash

## A.L.R.R. Aug 17, 2020
## Script to convert individual images to MNI space, create 4D files (all subject MNI converted ROI-WB FC), and run randomise on those files to obtain group-specific (1stt) maps.

SEARCHFOLDER=/mnt/storage/Jessica_Lab/analyses/veni-lmu/participants/
DATAFOLDER=/mnt/storage/Jessica_Lab/data/veni/subjects/
REGFOLDER=/scan-data/niftis/ep2d_rest/unnormalized/fmri.feat/reg/
OUTPUTFOLDER=/mnt/storage/Jessica_Lab/analyses/veni-lmu/ROI-WB/

mkdir -p $OUTPUTFOLDER

for participant in $SEARCHFOLDER*
do
  foldername=`basename $participant`
  echo "Process for $foldername"
  cd $participant
  for mask in ${participant}/*
  do
    maskname=`basename $mask | sed 's/.nii.gz//'`
    if [[ ${maskname} == SCA* ]]
    then
      echo "$maskname"
      flirt -usesqform -in $mask/dr_stage2_ic0000.nii.gz -ref /mnt/storage/Jessica_Lab/analyses/veni-lmu/masks/functional/MNI152_T1_2mm_brain.nii.gz -applyxfm -init $DATAFOLDER/$foldername/$REGFOLDER/example_func2standard.mat -out $mask/${maskname}_MNI
    fi
  done
done

 #LIFG ROI
 mkdir -p $OUTPUTFOLDER/LIFG
 fslmerge -t $OUTPUTFOLDER/LIFG/LIFG_01 /mnt/storage/Jessica_Lab/analyses/veni-lmu/participants/wsu-???-00??-01/SCA_DR_01-LIFG_gm/SCA_DR_01-LIFG_gm_MNI.nii.gz
 fslmerge -t $OUTPUTFOLDER/LIFG/LIFG_02 /mnt/storage/Jessica_Lab/analyses/veni-lmu/participants/wsu-???-00??-02/SCA_DR_01-LIFG_gm/SCA_DR_01-LIFG_gm_MNI.nii.gz
 fslmerge -t $OUTPUTFOLDER/LIFG/LIFG_03 /mnt/storage/Jessica_Lab/analyses/veni-lmu/participants/wsu-???-00??-03/SCA_DR_01-LIFG_gm/SCA_DR_01-LIFG_gm_MNI.nii.gz

 #LINS ROI
 mkdir -p $OUTPUTFOLDER/LINS
 fslmerge -t $OUTPUTFOLDER/LINS/LINS_01 /mnt/storage/Jessica_Lab/analyses/veni-lmu/participants/wsu-???-00??-01/SCA_DR_02-LINS_gm/SCA_DR_02-LINS_gm_MNI.nii.gz
 fslmerge -t $OUTPUTFOLDER/LINS/LINS_02 /mnt/storage/Jessica_Lab/analyses/veni-lmu/participants/wsu-???-00??-02/SCA_DR_02-LINS_gm/SCA_DR_02-LINS_gm_MNI.nii.gz
 fslmerge -t $OUTPUTFOLDER/LINS/LINS_03 /mnt/storage/Jessica_Lab/analyses/veni-lmu/participants/wsu-???-00??-03/SCA_DR_02-LINS_gm/SCA_DR_02-LINS_gm_MNI.nii.gz

 #ACC ROI
 mkdir -p $OUTPUTFOLDER/ACC
 fslmerge -t $OUTPUTFOLDER/ACC/ACC_01 /mnt/storage/Jessica_Lab/analyses/veni-lmu/participants/wsu-???-00??-01/SCA_DR_03-ACC_gm/SCA_DR_03-ACC_gm_MNI.nii.gz
 fslmerge -t $OUTPUTFOLDER/ACC/ACC_02 /mnt/storage/Jessica_Lab/analyses/veni-lmu/participants/wsu-???-00??-02/SCA_DR_03-ACC_gm/SCA_DR_03-ACC_gm_MNI.nii.gz
 fslmerge -t $OUTPUTFOLDER/ACC/ACC_03 /mnt/storage/Jessica_Lab/analyses/veni-lmu/participants/wsu-???-00??-03/SCA_DR_03-ACC_gm/SCA_DR_03-ACC_gm_MNI.nii.gz

 #RIFG ROI
 mkdir -p $OUTPUTFOLDER/RIFG
 fslmerge -t $OUTPUTFOLDER/RIFG/RIFG_01 /mnt/storage/Jessica_Lab/analyses/veni-lmu/participants/wsu-???-00??-01/SCA_DR_04-RIFG_gm/SCA_DR_04-RIFG_gm_MNI.nii.gz
 fslmerge -t $OUTPUTFOLDER/RIFG/RIFG_02 /mnt/storage/Jessica_Lab/analyses/veni-lmu/participants/wsu-???-00??-02/SCA_DR_04-RIFG_gm/SCA_DR_04-RIFG_gm_MNI.nii.gz
 fslmerge -t $OUTPUTFOLDER/RIFG/RIFG_03 /mnt/storage/Jessica_Lab/analyses/veni-lmu/participants/wsu-???-00??-03/SCA_DR_04-RIFG_gm/SCA_DR_04-RIFG_gm_MNI.nii.gz

 #RINS ROI
 mkdir -p $OUTPUTFOLDER/RINS
 fslmerge -t $OUTPUTFOLDER/RINS/RINS_01 /mnt/storage/Jessica_Lab/analyses/veni-lmu/participants/wsu-???-00??-01/SCA_DR_05-RINS_gm/SCA_DR_05-RINS_gm_MNI.nii.gz
 fslmerge -t $OUTPUTFOLDER/RINS/RINS_02 /mnt/storage/Jessica_Lab/analyses/veni-lmu/participants/wsu-???-00??-02/SCA_DR_05-RINS_gm/SCA_DR_05-RINS_gm_MNI.nii.gz
 fslmerge -t $OUTPUTFOLDER/RINS/RINS_03 /mnt/storage/Jessica_Lab/analyses/veni-lmu/participants/wsu-???-00??-03/SCA_DR_05-RINS_gm/SCA_DR_05-RINS_gm_MNI.nii.gz

for map in $OUTPUTFOLDER*
do
  foldername=`basename $map`
  cd $map
  for timepoint in ${map}/*
  do
    tpname=`basename $timepoint | sed 's/.nii.gz//'`
      echo "$tpname"
      randomise -i $timepoint -o ${tpname}_1stt -1 -T -m /mnt/storage/Jessica_Lab/analyses/veni-lmu/masks/functional/MNI152_T1_2mm_brain_mask.nii.gz
  done
done
