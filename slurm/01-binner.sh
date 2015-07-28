#!/bin/bash

set -u

BIN="$( readlink -f -- "${0%/*}" )"
SCRIPTS=$BIN/../scripts
OUT_DIR=$SCRATCH/kmer-binner/binned
PROG=$(basename $0 '.sh')
PARAMS_DIR=$BIN/params/$PROG
SLURM_OUT=$BIN/out/$PROG
SLURM_EMAIL="--mail-type=BEGIN,END --mail-user=kyclark@email.arizona.edu"
COMMON=$SCRIPTS/common.sh
REF_DIR=/scratch/03137/kyclark/data/reference
IN_DIRS="$REF_DIR/mouse $REF_DIR/wheat $REF_DIR/yeast $REF_DIR/zea_mays $REF_DIR/glycine_max $REF_DIR/medicago_truncatula"

FILES_LIST=$(mktemp)

if [ -e $COMMON ]; then
  source $COMMON
else
  echo COMMON \"$COMMON\" not found
  exit 1
fi

if [[ ! -d $OUT_DIR ]]; then
  mkdir -p $OUT_DIR
fi

init_dirs "$SLURM_OUT" "$PARAMS_DIR"

find $IN_DIRS -type f > $FILES_LIST

NUM_FILES=$(lc $FILES_LIST)

echo Found \"$NUM_FILES\" files

if [ $NUM_FILES -lt 1 ]; then
  echo Nothing to do
  exit 1
fi

PARAMS_FILE=$PARAMS_DIR/$$

if [ -e $PARAMS_FILE ]; then
  rm $PARAMS_FILE
fi

while read FILE; do
  BASE_DIR=$(dirname $FILE)
  BASE_DIR=$(basename $BASE_DIR)
  DIR=$OUT_DIR/$BASE_DIR

  if [ -e $DIR ]; then
    rm -rf $DIR/*
  fi

  echo "$SCRIPTS/binner.pl -q -f $FILE -o $DIR" >> $PARAMS_FILE
done < $FILES_LIST

sbatch -J binner -o "$SLURM_OUT/%j.out" -e "$SLURM_OUT/%j.err" \
  -n ${NUM_FILES:=1} ${SLURM_EMAIL:=""} \
    $BIN/launcher.sh $PARAMS_FILE
