#!/bin/bash

## A.L.R.R. Aug 18, 2020
## Script to compare FC maps, derived from each ROI, with the Shirer et al.'s template.

# Relevant paths/directories or files
SEARCHFOLDER=/mnt/storage/Jessica_Lab/analyses/veni-lmu/ROI-WB/
TEMPLATE_ANT_SAL=/mnt/storage/Jessica_Lab/analyses/veni-lmu/masks/functional/Shirer_et_al-anterior_salience/anterior_salience_std.nii.gz
OUTPUTFILE=/mnt/storage/Jessica_Lab/analyses/veni-lmu/ROI-WB/FSLCC.txt

# Loop to compute fslcc
for ROI in $SEARCHFOLDER*
do
  if [ -d "$ROI" ]
  then
    roiname=`basename $ROI`
    echo $roiname
    cd $ROI
    for (( p=1; p<=3; p=p+1 )) #p = timepoints; max: 3; check whether the tfce_corrp files are not binarized; then, binarize them with fslmaths
    do
      if [ ! -f "${roiname}_0${p}_1stt_tfce_corrp_bin.nii.gz" ]
      then
        echo "binarizing ${roiname}_0${p}_1stt_tfce_corrp_bin.nii.gz"
        fslmaths ${roiname}_${i}_1stt_tfce_corrp.nii.gz -thr 0.95 -bin ${roiname}_01_1stt_tfce_corrp_bin.nii.gz
      fi
    done
    for timepoint in ${ROI}/*
    do
      tpname=`basename $timepoint | sed 's/_tstat1.nii.gz//'`
      if [[ ${tpname} == *tfce_corrp_bin* ]]
      then
        echo "$tpname is binarized" >> $SEARCHFOLDER/temp1.txt
          fslcc $TEMPLATE_ANT_SAL $ROI/$tpname >> $SEARCHFOLDER/temp2.txt
      fi
    done
  fi
done

# Delete first columns and white spaces of fslcc output
awk '!($1="")' $SEARCHFOLDER/temp2.txt > $SEARCHFOLDER/temp3.txt
awk '!($1="")' $SEARCHFOLDER/temp3.txt > $SEARCHFOLDER/temp2.txt
sed -e 's/^[[:space:]]*//' $SEARCHFOLDER/temp2.txt > $SEARCHFOLDER/temp3.txt

# Merge files
paste $SEARCHFOLDER/temp1.txt $SEARCHFOLDER/temp3.txt | column -s $'\t' -t > $OUTPUTFILE

echo "***The '$0' script has finished; go to '$OUTPUTFILE' to see the results***"

rm $SEARCHFOLDER/temp*
