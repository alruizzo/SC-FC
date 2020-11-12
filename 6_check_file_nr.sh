#!/bin/bash

# A.L.R.R. October 8, 2020

# Script to check which participants' folders have fewer files than they...
# ..should. Step after WB-DR (SCA folders within each participant's)

## Define paths
source var_names.sh

## Loop through the participants' folder to warp the native whole-brain...
## ...images to MNI space to prepare them for randomise
for participant in $SEARCHFOLDER*
do
  foldername=`basename $participant`
  cd $participant
  for mask in ${participant}/*
  do
    maskname=`basename $mask`
    if [[ ${maskname} == SCA* ]] # To constrain to the just created folder
    then
      cd $mask
      nrfiles=`ls | wc -l`
      if [ $nrfiles -lt 7 ] # Adjust number according to what's expected
      then
        echo "For $foldername:" >> $OUTPUTFILE
        echo "$maskname folder *only* has $nrfiles file(s)" >> $OUTPUTFILE
      fi
    fi
  done
done

## End message
echo "* FINISHED '$0' on $(date). Results in $OUTPUTFILE * "
