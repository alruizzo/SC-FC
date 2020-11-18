#!/bin/bash

# A.L.R.R. Aug 20, 2020
# October 9, 2020

# Script to select relevant information from the participants' folders...
# ...and pass on to R for ROI-to-ROI correlation analyses

## Script output (for errors), I run it manually in the terminal
#script /mnt/storage/Jessica_Lab/analyses/veni-lmu/checkfile.txt

## Specify the relevant paths/directories or files
source var_names.sh

## Create temporal and output folders
mkdir -p $TEMPFOLDER
mkdir -p $OUTPUTFOLDER

## Loop to copy in a temporal folder the participants' files needed
for participant in $SEARCHFOLDER*
do
    foldername=`basename $participant`
    echo "Checking number of files for $foldername..."
    mkdir -p $TEMPFOLDER/$foldername
    cp $participant/wm*.txt $TEMPFOLDER/$foldername
    cd $TEMPFOLDER/$foldername | echo `ls | wc -l`
    echo "...done"
done

## Loop to create one text file per participant (summary)
echo "Now creating one file per participant"
for participant in $TEMPFOLDER/*
do
  particname=`basename $participant`
  cd $participant
  for file in $participant/*
  do
    filename=`basename $file`
    (echo $filename; cat $file) > ${filename}_temp.txt
    echo "moving files..."
    mv ${filename}_temp.txt $file
    echo "...done"
  done
paste $participant/wmeants_*.txt > $OUTPUTFOLDER/$particname.txt
done

## Compress folder to upload to R
echo "Now compressing the files..."
#uncomment line below if you want the zip file including divided into folders
#zip -r $ROOTPATH/weightedmeantimeseries_folders.zip $TEMPFOLDER
#summarized in text file:
zip -r $ROOTPATH/weightedmeantimeseries.zip $OUTPUTFOLDER/*
echo "...done"
rm -r $TEMPFOLDER

## End message
echo "* FINISHED $0 on $(date) \
see '$OUTPUTFOLDER' to check results *"
#exit #<-don't forget to execute it. Necessary to save the output of the...
#...'script' command to the text file!
