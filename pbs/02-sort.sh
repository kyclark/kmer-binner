#!/bin/bash

set -u

export BIN="$( readlink -f -- "${0%/*}" )"
export SCRIPTS=$BIN/../scripts
export IN_DIR="$BIN../out"
export OUT_DIR="$BIN../sorted"

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

init_dirs "$PBSOUT_DIR" "$OUT_DIR"

EMAIL_ARG=""
if [[ ! -z $EMAIL ]]; then
  EMAIL_ARG="-M $EMAIL -m ea"
fi

GROUP_ARG="-W group_list=${GROUP:=bhurwitz}"

PBS_ARGS="-q standard -l jobtype=cluster_only -l select=1:ncpus=4:mem=10gb -l pvmem=20gb -l walltime=24:00:00 -l cput=24:00:00 -j oe"

JOB=$(qsub -N kmer-bin $GROUP_ARG $EMAIL_ARG $PBS_ARGS -o "$PBSOUT_DIR" "$SCRIPTS/sort.sh $IN_DIR $OUT_DIR")

if [ $? -eq 0 ]; then
  echo Submitted job \"$JOB\" for you in steps of \"$STEP_SIZE.\" Aloha.
else
  echo -e "\nError submitting job\n$JOB\n"
fi
