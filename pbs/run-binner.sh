#!/bin/bash

#PBS -q standard
#PBS -l jobtype=cluster_only
#PBS -l select=1:ncpus=4:mem=10gb
#PBS -l walltime=24:00:00
#PBS -l cput=24:00:00

COMMON=$SCRIPTS/common.sh

if [ -e $COMMON ]; then
  source $COMMON
else
  echo COMMON \"$COMMON\" not found
fi

TMP_FILES=$(mktemp)

if [[ ${INPUT_GROUP_FILE:=""} != "" ]] && [[ ${PBS_ARRAY_INDEX:=1} -gt 0 ]]
then
  awk -F"\t" "\$1 == $PBS_ARRAY_INDEX {print \$2}" $INPUT_GROUP_FILE \
    > $TMP_FILES
else
  get_lines $FILES_LIST $TMP_FILES ${PBS_ARRAY_INDEX:=1} ${STEP_SIZE:=1}
fi

NUM_FILES=$(lc $TMP_FILES)

if [ $NUM_FILES -lt 1 ]; then
  echo No files found 
  echo FILES_LIST \"$FILES_LIST\"
  echo PBS_ARRAY_INDEX \"$PBS_ARRAY_INDEX\"
  echo STEP_SIZE \"$STEP_SIZE\"
  exit 1
fi

echo Processing \"$NUM_FILES\" files

i=0
while read FILE; do
  BASENAME=$(basename $FILE)
  SIZE=$(ls -lh $FILE | awk '{print $5}')

  let i++
  printf "%5d: %s (%s)\n" $i $BASENAME $SIZE

  DIR=$OUT_DIR/$BASENAME

  if [[ ! -d $DIR ]]; then
    mkdir -p $DIR
    time jellyfish dump -c $FILE | $SCRIPTS/binner.pl -o $DIR
  fi

  #time $SCRIPTS/kmer-binner -f $FILE -o "$OUT_DIR/$BASE_DIR"
done < $TMP_FILES

rm $TMP_FILES
echo Done.
