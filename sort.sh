if [ $# -eq 0 ]; then
  echo Usage: $(basename $0) dir
  exit 1
fi

IN_DIR=$1
CWD=$PWD
ADD=$CWD/add.pl
SORTED_DIR=$CWD/sorted

if [[ ! -d $IN_DIR ]]; then
  echo Bad DIR \"$IN_DIR\"
fi

if [[ ! -d $SORTED_DIR ]]; then
  mkdir -p $SORTED_DIR
fi

FILES='/tmp/files'

find $IN_DIR -type f | perl -MFile::Basename -ne 'print basename($_)' \
  | sort | uniq > $FILES

i=0
while read FILE; do
  let i++
  printf "%5d: %s\n" $i $FILE
  LOCS=$(find $IN_DIR -type f -name $FILE)

  for F in $LOCS; do
    sort $F | $ADD -p $FILE > $F.sorted
  done

  SORTED=$(find $IN_DIR -type f -name $FILE.sorted)

  sort -m $SORTED | uniq > $SORTED_DIR/$FILE

  rm $SORTED
done < $FILES

echo Done. 
