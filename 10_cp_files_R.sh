#!/bin/bash

## A.L.R.R. Aug 20, 2020
## Script to select relevant information from the participants' folders and pass on to R for ROI-to-ROI correlation analyses

# # Script output (for errors), I run it manually in the terminal
#script /mnt/storage/Jessica_Lab/analyses/veni-lmu/checkfile.txt

# Relevant paths/directories or files
SEARCHFOLDER=/mnt/storage/Jessica_Lab/analyses/veni-lmu/participants/
OUTPUTFOLDER=/mnt/storage/Jessica_Lab/analyses/veni-lmu/ROI-TS/
TEMPFOLDER=/mnt/storage/Jessica_Lab/analyses/veni-lmu/temp/

mkdir -p $TEMPFOLDER
mkdir -p $OUTPUTFOLDER

# Loop to copy participants' files needed only
for participant in $SEARCHFOLDER*
do
    foldername=`basename $participant`
    echo $foldername
    mkdir -p $TEMPFOLDER/$foldername
    cp $participant/wm*.txt $TEMPFOLDER/$foldername #to compress all content in this folder, if wanted
    cd $TEMPFOLDER/$foldername | echo `ls | wc -l`
done

# Loop to create one text file per participants (summary)
for participant in $TEMPFOLDER*
do
  particname=`basename $participant`
  cd $participant
  for file in $participant/*
  do
    filename=`basename $file`
    (echo $filename; cat $file) > ${filename}_temp.txt
    mv ${filename}_temp.txt $file
  done
paste $participant/wmeants_0*.txt $participant/wmeants_WM.txt $participant/wmeants_CSF.txt > $OUTPUTFOLDER/$particname.txt
done

# Compress folder to upload to R
#zip -r /mnt/storage/Jessica_Lab/analyses/veni-lmu/weightedmeantimeseries_folders.zip $TEMPFOLDER #uncomment if you want the zip file including divided into folders
zip -r /mnt/storage/Jessica_Lab/analyses/veni-lmu/weightedmeantimeseries.zip $OUTPUTFOLDER #summarized in text file
rm -r $TEMPFOLDER

# End message
echo "finished the `basename $0` script; see '$OUTPUTFOLDER' to check results"
#exit #<-to save the output to the text file!
