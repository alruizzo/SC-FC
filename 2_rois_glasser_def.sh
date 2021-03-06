#!/bin/bash

## A.L.R.R. September 29, 2020 / October 7, 2020
## Script to select each multimodal ROI mask after visual inspection (for...
## ...overlap with the anterior salience network of Shirer et al., 2012)...
## ...and review of the Supplementary Neuroanatomical Results of Glasser...
## ...et al. (2016).

## ROIs
  ## LIFG -> 264, 266
  roi1=01-LIFG
  area11=264 #(i.e., +180 of those on the RH)
  area21=266

  ## LINS -> 288, 292
  roi2=02-LINS
  area12=288
  area22=292

  ## ACC -> RH: 60 (p32prime), 62 (d32), 179 (a32prime); LH: 240, 242, 359
  roi3=03-ACC
  area13=60
  area23=62
  area33=179
  area43=240
  area53=242
  area63=359

  ## RFIG -> 84, 86
  roi4=04-RIFG
  area14=84
  area24=86

  ## RINS -> 108 (FOP4: opercular), 112 (AAIC: dorsal anterior insula)
  roi5=05-RINS
  area15=108
  area25=112

## Get paths/directories
source var_names.sh

## Create Output dir
mkdir -p $OUTPUTFOLDER

## Create ROI array to loop for
ROIS=($roi1 $roi2 $roi3 $roi4 $roi5)

## Loop to extract the appropriate areas from the Glasser map
for roi in ${ROIS[@]}
do
  if [[ $roi == $roi1 ]]
  then
    area1=$area11
    area2=$area21
    echo "computing fslmaths for $roi and $area1 and $area2..."
    fslmaths $INPUTFILE -thr $area1 -uthr $area1 $OUTPUTFOLDER/temp$area1
    fslmaths $INPUTFILE -thr $area2 -uthr $area2 $OUTPUTFOLDER/temp$area2
    fslmaths $OUTPUTFOLDER/temp$area1 -add $OUTPUTFOLDER/temp$area2 $OUTPUTFOLDER/temp$roi
    fslmaths $OUTPUTFOLDER/temp$roi -bin $OUTPUTFOLDER/$roi
    rm $OUTPUTFOLDER/temp*
    echo "...done"
  elif [[ $roi == $roi2 ]]
  then
    area1=$area12
    area2=$area22
    echo "computing fslmaths for $roi and $area1 and $area2..."
    fslmaths $INPUTFILE -thr $area1 -uthr $area1 $OUTPUTFOLDER/temp$area1
    fslmaths $INPUTFILE -thr $area2 -uthr $area2 $OUTPUTFOLDER/temp$area2
    fslmaths $OUTPUTFOLDER/temp$area1 -add $OUTPUTFOLDER/temp$area2 $OUTPUTFOLDER/temp$roi
    fslmaths $OUTPUTFOLDER/temp$roi -bin $OUTPUTFOLDER/$roi
    rm $OUTPUTFOLDER/temp*
    echo "...done"
  elif [[ $roi == $roi3 ]]
  then
    area1=$area13
    area2=$area23
    area3=$area33
    area4=$area43
    area5=$area53
    area6=$area63
    echo "computing fslmaths for $roi and $area1, $area2, $area3, $area4, $area5, and $area6..."
    fslmaths $INPUTFILE -thr $area1 -uthr $area1 $OUTPUTFOLDER/temp$area1
    fslmaths $INPUTFILE -thr $area2 -uthr $area2 $OUTPUTFOLDER/temp$area2
    fslmaths $INPUTFILE -thr $area3 -uthr $area3 $OUTPUTFOLDER/temp$area3
    fslmaths $INPUTFILE -thr $area4 -uthr $area4 $OUTPUTFOLDER/temp$area4
    fslmaths $INPUTFILE -thr $area5 -uthr $area5 $OUTPUTFOLDER/temp$area5
    fslmaths $INPUTFILE -thr $area6 -uthr $area6 $OUTPUTFOLDER/temp$area6
    fslmaths $OUTPUTFOLDER/temp$area1 -add $OUTPUTFOLDER/temp$area2 -add $OUTPUTFOLDER/temp$area3 -add $OUTPUTFOLDER/temp$area4 -add $OUTPUTFOLDER/temp$area5 -add $OUTPUTFOLDER/temp$area6 $OUTPUTFOLDER/temp$roi
    fslmaths $OUTPUTFOLDER/temp$roi -bin $OUTPUTFOLDER/$roi
    rm $OUTPUTFOLDER/temp*
    echo "...done"
  elif [[ $roi == $roi4 ]]
  then
    area1=$area14
    area2=$area24
    echo "computing fslmaths for $roi and $area1 and $area2..."
    fslmaths $INPUTFILE -thr $area1 -uthr $area1 $OUTPUTFOLDER/temp$area1
    fslmaths $INPUTFILE -thr $area2 -uthr $area2 $OUTPUTFOLDER/temp$area2
    fslmaths $OUTPUTFOLDER/temp$area1 -add $OUTPUTFOLDER/temp$area2 $OUTPUTFOLDER/temp$roi
    fslmaths $OUTPUTFOLDER/temp$roi -bin $OUTPUTFOLDER/$roi
    rm $OUTPUTFOLDER/temp*
    echo "...done"
  elif [[ $roi == $roi5 ]]
  then
    area1=$area15
    area2=$area25
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
