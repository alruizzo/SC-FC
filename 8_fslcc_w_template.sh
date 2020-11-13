#!/bin/bash

# A.L.R.R. Aug 18, 2020
# October 9, 2020 (update)

# Script to compare FC maps, derived from each ROI (in turn, defined from...
# ...areas taken from the multimodal parcellation of Glasser...
# ...et al. 2016), with the Shirer et al.'s anterior salience network...
# ...template (or posterior template, for the control analyses).

## Relevant paths/directories or files
source var_names.sh

## Loop to binarize the maps that resulted from randomise (1stt) and compute...
## ...fslcc for each time point separately
for ROI in $SEARCHFOLDER*
do
  if [ -d "$ROI" ]
  then
    roiname=`basename $ROI`
    echo "ROI: $roiname"
    cd $ROI
    for (( p=1; p<=3; p=p+1 )) #p = timepoints; max: 3;
    do
      # check whether the tfce_corrp files are not binarized;...
      # ...then, binarize them with fslmaths
      if [ ! -f "${roiname}_0${p}_bin_1stt_tfce_corrp_tstat1.nii.gz" ]
      then
        echo "Now binarizing ${roiname}_0${p}_1stt_tfce_corrp.nii.gz"
        fslmaths ${roiname}_0${p}_1stt_tfce_corrp_tstat1.nii.gz -thr 0.95 -bin ${roiname}_0${p}_bin_1stt_tfce_corrp_tstat1.nii.gz
      fi
    done
    for timepoint in ${ROI}/*
    do
      tpname=`basename $timepoint | sed 's/_1stt_tfce_corrp_tstat1.nii.gz//'`
      if [[ ${tpname} == *bin ]]
      then
        echo "Timepoint '$tpname' of $roiname is binarized" >> $SEARCHFOLDER/temp1.txt
        echo "Calculating fslcc for $tpname"
        fslcc $MASKFOLDER/$TEMPLATE_SAL $timepoint >> $SEARCHFOLDER/temp2.txt
      fi
    done
  fi
done

## Delete first columns and white spaces of fslcc output
awk '!($1="")' $SEARCHFOLDER/temp2.txt > $SEARCHFOLDER/temp3.txt
awk '!($1="")' $SEARCHFOLDER/temp3.txt > $SEARCHFOLDER/temp2.txt
sed -e 's/^[[:space:]]*//' $SEARCHFOLDER/temp2.txt > $SEARCHFOLDER/temp3.txt

## Merge files
paste $SEARCHFOLDER/temp1.txt $SEARCHFOLDER/temp3.txt | column -s $'\t' -t > $OUTPUTFILE

## End message
echo "* The '$0' script finished on $(date). \
Go to '$OUTPUTFILE' to see the results *"

## Delete "temporal" files
rm $SEARCHFOLDER/temp*
