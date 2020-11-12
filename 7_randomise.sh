#!/bin/bash

# A.L.R.R. Aug 17, 2020
# October 8, 2020 (update)

# Script to convert individual images to MNI space, create 4D files (all...
# ...subject MNI converted ROI-WB FC), and run randomise on those files...
# ...to obtain group-specific (1stt) maps.

## Define paths
source var_names.sh

## Create the output folder if it doesn't exist
mkdir -p $OUTPUTFOLDER

## Loop through the participants' folder to warp the native whole-brain...
## ...images to MNI space to prepare them for randomise
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
## ...for randomization
  echo "Now continuing with fslmerge..."
 #LIFG ROI
 mkdir -p $OUTPUTFOLDER/LIFG
 echo "...Merging LIFG across participants and time points..."
 fslmerge -t $OUTPUTFOLDER/LIFG/LIFG_01 $SEARCHFOLDER/wsu-???-00??-01/SCA_DR_01-LIFG_/01-LIFG_MNI.nii.gz
 fslmerge -t $OUTPUTFOLDER/LIFG/LIFG_02 $SEARCHFOLDER/wsu-???-00??-02/SCA_DR_01-LIFG_/01-LIFG_MNI.nii.gz
 fslmerge -t $OUTPUTFOLDER/LIFG/LIFG_03 $SEARCHFOLDER/wsu-???-00??-03/SCA_DR_01-LIFG_/01-LIFG_MNI.nii.gz
 echo "...done"

 #LINS ROI
 mkdir -p $OUTPUTFOLDER/LINS
 echo "Merging LINS across participants and time points..."
 fslmerge -t $OUTPUTFOLDER/LINS/LINS_01 $SEARCHFOLDER/wsu-???-00??-01/SCA_DR_02-LINS_/02-LINS_MNI.nii.gz
 fslmerge -t $OUTPUTFOLDER/LINS/LINS_02 $SEARCHFOLDER/wsu-???-00??-02/SCA_DR_02-LINS_/02-LINS_MNI.nii.gz
 fslmerge -t $OUTPUTFOLDER/LINS/LINS_03 $SEARCHFOLDER/wsu-???-00??-03/SCA_DR_02-LINS_/02-LINS_MNI.nii.gz
 echo "...done"

 #ACC ROI
 mkdir -p $OUTPUTFOLDER/ACC
 echo "Merging ACC across participants and time points..."
 fslmerge -t $OUTPUTFOLDER/ACC/ACC_01 $SEARCHFOLDER/wsu-???-00??-01/SCA_DR_03-ACC_/03-ACC_MNI.nii.gz
 fslmerge -t $OUTPUTFOLDER/ACC/ACC_02 $SEARCHFOLDER/wsu-???-00??-02/SCA_DR_03-ACC_/03-ACC_MNI.nii.gz
 fslmerge -t $OUTPUTFOLDER/ACC/ACC_03 $SEARCHFOLDER/wsu-???-00??-03/SCA_DR_03-ACC_/03-ACC_MNI.nii.gz
 echo "...done"

 #RIFG ROI
 mkdir -p $OUTPUTFOLDER/RIFG
 echo "Merging RIFG across participants and time points..."
 fslmerge -t $OUTPUTFOLDER/RIFG/RIFG_01 $SEARCHFOLDER/wsu-???-00??-01/SCA_DR_04-RIFG_/04-RIFG_MNI.nii.gz
 fslmerge -t $OUTPUTFOLDER/RIFG/RIFG_02 $SEARCHFOLDER/wsu-???-00??-02/SCA_DR_04-RIFG_/04-RIFG_MNI.nii.gz
 fslmerge -t $OUTPUTFOLDER/RIFG/RIFG_03 $SEARCHFOLDER/wsu-???-00??-03/SCA_DR_04-RIFG_/04-RIFG_MNI.nii.gz
 echo "...done"

 #RINS ROI
 mkdir -p $OUTPUTFOLDER/RINS
 echo "Merging RINS across participants and time points..."
 fslmerge -t $OUTPUTFOLDER/RINS/RINS_01 $SEARCHFOLDER/wsu-???-00??-01/SCA_DR_05-RINS_/05-RINS_MNI.nii.gz
 fslmerge -t $OUTPUTFOLDER/RINS/RINS_02 $SEARCHFOLDER/wsu-???-00??-02/SCA_DR_05-RINS_/05-RINS_MNI.nii.gz
 fslmerge -t $OUTPUTFOLDER/RINS/RINS_03 $SEARCHFOLDER/wsu-???-00??-03/SCA_DR_05-RINS_/05-RINS_MNI.nii.gz
 echo "...done"

echo "Merging of all ROIs, done"

## Loop for randomise through participant folder and time point while...
## ...using the MNI brain as mask
echo "Now starting randomise:"
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
