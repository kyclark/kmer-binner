if [ $# -eq 0 ]; then
  echo Usage: $(basename $0) dir
  exit 1
fi

IN_DIR=$1
CWD=$PWD
PREFIXER=$CWD/prefixer.pl
SORTED_DIR=$CWD/sorted

if [[ ! -d $IN_DIR ]]; then
  echo Bad DIR \"$IN_DIR\"
fi

if [[ ! -d $SORTED_DIR ]]; then
  mkdir -p $SORTED_DIR
fi

FILES=$(mktemp)

find $IN_DIR -type f | perl -MFile::Basename -ne 'print basename($_)' \
  | sort | uniq > $FILES

i=0
while read FILE; do
  let i++
  printf "%5d: %s\n" $i $FILE
  LOCS=$(find $IN_DIR -type f -name $FILE)

  #
  # The "bin" (e.g., AACTG) isn't stored in the file, so
  # after sorting, we add it back.
  #
  for F in $LOCS; do
    sort $F | $PREFIXER -p $FILE > $F.sorted
  done

  SORTED=$(find $IN_DIR -type f -name $FILE.sorted)

  sort -m $SORTED | uniq > $SORTED_DIR/$FILE

  rm $SORTED
done < $FILES

echo Done. 
