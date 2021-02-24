#!/bin/bash

# A.L.R.R. Feb 24th, 2021

# Script to create a list of participant names for extracting the NODDI
# values based on counting the number of files in TRACULA

# Specify the relevant paths/directories or files
source var_names.sh

# Loop through Tracula dir to check whether one example tract in
# dpath has the expected number of files (14). If not, then store
# the names for doing the required step to generate those files.
for participant in $TRACULADIR*
do
  name=`basename $participant`
  if [[ $name == wsu* ]]
  then
    cd $participant/dpath/fmajor_PP_avg33_mni_bbr/
    echo $participant &>> $OUTPUTFOLDER/8checkfiles_tracula.txt
    if [ `ls -1 | wc -l` -lt 14 ]
    then
      echo $name &>> $OUTPUTFOLDER/9missing-noddi.txt
    fi
  fi
  cd ..
done

## End message
echo "* FINISHED $0 on $(date) \
see '$OUTPUTFOLDER' to check results *"
