if [ $# -ne 2 ]; then
  echo Usage: $(basename $0) in-dir out-dir
  exit 1
fi

IN_DIR=$1
OUT_DIR=$2

if [[ ! -d $IN_DIR ]]; then
  echo Bad DIR \"$IN_DIR\"
fi

if [[ ! -d $OUT_DIR ]]; then
  mkdir -p $OUT_DIR
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
    sort $F | sed "s/^/$FILE" > $F.sorted
  done

  SORTED=$(find $IN_DIR -type f -name $FILE.sorted)

  sort -m $SORTED | uniq > $OUT_DIR/$FILE

  rm $SORTED
done < $FILES

echo Done. 
