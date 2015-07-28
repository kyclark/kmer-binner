#!/bin/bash

#SBATCH -p normal           # Queue name
#SBATCH -t 12:00:00         # Run time (hh:mm:ss)

set -u

CONTROL_FILE=${1:-''}

if [ -z $CONTROL_FILE ]; then
  echo No control file provided.
  exit 1
fi

if [[ ! -e $CONTROL_FILE ]]; then
  echo Control file \"$CONTROL_FILE\" does not exit
  exit 1
fi

module load launcher

EXECUTABLE=$TACC_LAUNCHER_DIR/init_launcher 
$TACC_LAUNCHER_DIR/paramrun $EXECUTABLE $CONTROL_FILE

#WORKDIR=$PWD
#
##----------------
## Error Checking
##----------------
#
#if [[ ! -e $WORKDIR ]]; then
#    echo " "
#    echo "Error: unable to change to working directory."
#    echo "       $WORKDIR"
#    echo " "
#    echo "Job not submitted."
#    exit
#fi
#
#if [[ ! -f $EXECUTABLE ]]; then
#    echo " "
#    echo "Error: unable to find launcher executable $EXECUTABLE."
#    echo " "
#    echo "Job not submitted."
#    exit
#fi
#
#if [[ ! -f $CONTROL_FILE ]]; then
#    echo " "
#    echo "Error: unable to find input control file $CONTROL_FILE."
#    echo " "
#    echo "Job not submitted."
#    exit
#fi
#
#
##----------------
## Job Submission
##----------------
#
#cd $WORKDIR/
#echo " WORKING DIR:   $WORKDIR/"
#
#$TACC_LAUNCHER_DIR/paramrun $EXECUTABLE $CONTROL_FILE
#
#echo " "
#echo " Parameteric Job Complete"
#echo " "
