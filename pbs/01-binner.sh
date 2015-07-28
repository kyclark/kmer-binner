#!/bin/bash

set -u

export BIN="$( readlink -f -- "${0%/*}" )"
export OUT_DIR=$BIN/../out
export STEP_SIZE=5
export FILES_LIST=~/$$.in
export SCRIPTS=$BIN/../scripts

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

REF_DIR=/rsgrps/bhurwitz/hurwitzlab/data/reference
IN_DIRS="$REF_DIR/a_xylosoxidans $REF_DIR/mouse_genome/20141111 $REF_DIR/glycine_max $REF_DIR/yeast $REF_DIR/wheat $REF_DIR/medicago_truncatula $REF_DIR/zea_mays"

#find $IN_DIRS -type f > $FILES_LIST
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

JOBS_ARG=""
if [ $NUM_FILES -gt 1 ] && [ $STEP_SIZE -gt 1 ]; then
  JOBS_ARG="-J 1-$NUM_FILES:$STEP_SIZE "
fi

EMAIL_ARG=""
if [[ ! -z $EMAIL ]]; then
  EMAIL_ARG="-M $EMAIL -m ea"
fi

GROUP_ARG="-W group_list=${GROUP:=bhurwitz}"

JOB=$(qsub -N kmer-bin $GROUP_ARG $JOBS_ARG $EMAIL_ARG -j oe -o "$PBSOUT_DIR" -v SCRIPTS,BIN,STEP_SIZE,FILES_LIST,OUT_DIR $BIN/run-binner.sh)

if [ $? -eq 0 ]; then
  echo Submitted job \"$JOB\" for you in steps of \"$STEP_SIZE.\" Aloha.
else
  echo -e "\nError submitting job\n$JOB\n"
fi
