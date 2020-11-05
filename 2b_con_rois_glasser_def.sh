#!/bin/bash

## A.L.R.R. November 5, 2020
## Script to select the control ROI masks (posterior insula).

## ROIs
  ## LPINS -> 286, 347
  roi9=09-LINS
  area19=286
  area29=347

  ## RPINS -> 106, 167 (POI2, POI1)
  roi12=12-RINS
  area112=106
  area212=167

## Get paths/directories
source var_names.sh

## Create Output dir
mkdir -p $OUTPUTFOLDER

## Create ROI array to loop for
ROIS=($roi9 $roi12)

## Loop to extract the appropriate areas from the Shirer+Glasser maps
for roi in ${ROIS[@]}
do
  if [[ $roi == $roi9 ]]
  then
    area1=$area19
    area2=$area29
    echo "computing fslmaths for $roi and $area1 and $area2..."
    fslmaths $INPUTFILE -thr $area1 -uthr $area1 $OUTPUTFOLDER/temp$area1
    fslmaths $INPUTFILE -thr $area2 -uthr $area2 $OUTPUTFOLDER/temp$area2
    fslmaths $OUTPUTFOLDER/temp$area1 -add $OUTPUTFOLDER/temp$area2 $OUTPUTFOLDER/temp$roi
    fslmaths $OUTPUTFOLDER/temp$roi -bin $OUTPUTFOLDER/$roi
    rm $OUTPUTFOLDER/temp*
    echo "...done"
  elif [[ $roi == $roi12 ]]
  then
    area1=$area112
    area2=$area212
    echo "computing fslmaths for $roi and $area1 and $area2..."
    fslmaths $INPUTFILE -thr $area1 -uthr $area1 $OUTPUTFOLDER/temp$area1
    fslmaths $INPUTFILE -thr $area2 -uthr $area2 $OUTPUTFOLDER/temp$area2
    fslmaths $OUTPUTFOLDER/temp$area1 -add $OUTPUTFOLDER/temp$area2 $OUTPUTFOLDER/temp$roi
    fslmaths $OUTPUTFOLDER/temp$roi -bin $OUTPUTFOLDER/$roi
    rm $OUTPUTFOLDER/temp*
    echo "...done"
  else
    echo "All ROIs (except $roi) finished. Check results at ----> $OUTPUTFOLDER"
  fi
done

echo "${ROIS[@]} finished. Check results at ----> $OUTPUTFOLDER"
echo "* FINISHED $0 on $(date) *"
