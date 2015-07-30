#!/bin/bash

set -u

export BIN="$( readlink -f -- "${0%/*}" )"
export OUT_DIR=/rsgrps/bhurwitz/kyclark/mouse/data/host-binned
export STEP_SIZE=5
export FILES_LIST=~/$$.in
export SCRIPTS=$BIN/../scripts
export INPUT_GROUP_FILE=""

IN_DIRS="/rsgrps/bhurwitz/hurwitzlab/data/reference/jellyfish/mouse-host"
COMMON=$SCRIPTS/common.sh
EMAIL=kyclark@email.arizona.edu
GROUP=mbsulli
PROG=$(basename "$0" ".sh")
PBSOUT_DIR="$BIN/out/$PROG"

if [ -e $COMMON ]; then
  source $COMMON
else
  echo COMMON \"$COMMON\" not found
fi

if [[ ! -d $OUT_DIR ]]; then
  mkdir -p $OUT_DIR
fi

init_dirs "$PBSOUT_DIR" "$OUT_DIR"

TMP=$(mktemp)
find $IN_DIRS -type f > $TMP
while read FILE; do
  if [[ ! -d "$OUT_DIR/$(basename $FILE)" ]]; then
    echo $FILE >> $FILES_LIST
  fi
done < $TMP
rm $TMP

NUM_FILES=$(lc $FILES_LIST)
echo Found \"$NUM_FILES\" files

if [ $NUM_FILES -lt 1 ]; then
  echo Nothing to do
  exit 1
fi

EMAIL_ARG=""
if [[ ! -z $EMAIL ]]; then
  EMAIL_ARG="-M $EMAIL -m ea"
fi

GROUP_ARG="-W group_list=${GROUP:=bhurwitz}"

JOBS_ARG=""
if [ $NUM_FILES -gt 1 ] && [ $STEP_SIZE -gt 1 ]; then
  JOBS_ARG="-J 1-$NUM_FILES:$STEP_SIZE "
fi

DISTRIBUTOR=/rsgrps/bhurwitz/kyclark/file-distributor/distributor.pl

if [ -e $DISTRIBUTOR ]; then
  echo Working to distribute files -- gimme a sec

  FILE_SIZES=$(mktemp)
  while read FILE; do
    ls -l $FILE | awk '{print $5 " " $9}' >> $FILE_SIZES
  done < $FILES_LIST

  INPUT_GROUP_FILE=$HOME/$PROG.input_groups

  $DISTRIBUTOR $FILE_SIZES > $INPUT_GROUP_FILE

  rm $FILE_SIZES
else
  echo Cannot find \"$DISTRIBUTOR\"
fi

if [ ${INPUT_GROUP_FILE:="x"} != "x" ] && [ -e $INPUT_GROUP_FILE ]; then
  LAST_GROUP=$(tail -n 1 $INPUT_GROUP_FILE | cut -f 1)

  JOBS_ARG="-J 1-$LAST_GROUP"
  STEP_SIZE=0
fi

JOB=$(qsub -N kmer-bin $GROUP_ARG $JOBS_ARG $EMAIL_ARG -j oe -o "$PBSOUT_DIR" -v SCRIPTS,BIN,STEP_SIZE,FILES_LIST,OUT_DIR,INPUT_GROUP_FILE $BIN/run-binner.sh)

if [ $? -eq 0 ]; then
  echo Submitted job \"$JOB\" for you in steps of \"$STEP_SIZE.\" Aloha.
else
  echo -e "\nError submitting job\n$JOB\n"
fi
