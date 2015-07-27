#!/bin/bash

set -u

export CWD=$PWD
export OUT_DIR=$CWD/out
export STEP_SIZE=5
COMMON=/home/u20/kyclark/bin/common.sh
EMAIL=kyclark@email.arizona.edu
GROUP=mbsulli

if [ -e $COMMON ]; then
  source $COMMON
else
  echo COMMON \"$COMMON\" not found
fi

IN_DIRS="/rsgrps/bhurwitz/hurwitzlab/data/reference/mouse_genome/20141111 /rsgrps/bhurwitz/hurwitzlab/data/reference/soybean /rsgrps/bhurwitz/hurwitzlab/data/reference/yeast /rsgrps/bhurwitz/hurwitzlab/data/reference/wheat /rsgrps/bhurwitz/hurwitzlab/data/reference/medicago /rsgrps/bhurwitz/hurwitzlab/data/reference/zea_mays/v3"

export FILES_LIST=~/$$.in

find $IN_DIRS -type f > $FILES_LIST

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

PROG=$(basename "$0" ".sh")
PBSOUT_DIR="$CWD/out/$PROG"

init_dirs "$PBSOUT_DIR" "$OUT_DIR"

#qsub -I -N kmer-bin $GROUP_ARG $EMAIL_ARG -j oe -o "$PBSOUT_DIR" -v CWD,STEP_SIZE,FILES_LIST,OUT_DIR $CWD/run.sh

JOB=$(qsub -N kmer-bin $GROUP_ARG $JOBS_ARG $EMAIL_ARG -j oe -o "$PBSOUT_DIR" -v CWD,STEP_SIZE,FILES_LIST,OUT_DIR $CWD/run.sh)

if [ $? -eq 0 ]; then
  echo Submitted job \"$JOB\" for you in steps of \"$STEP_SIZE.\" Aloha.
else
  echo -e "\nError submitting job\n$JOB\n"
fi
