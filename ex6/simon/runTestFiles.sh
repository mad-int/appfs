#!/bin/bash

usage() {
    echo "usage: $0 binary checkFileDir"
    exit -1
}

if [ -z $2 ] ; then
  usage
fi

MAIN=$1
CHECKFILEDIR=$2
TMPFILE=tmpfile

if [ ! -x $MAIN ] ; then
  echo "No binary for checking. Maybe the compilation has errors?"
  exit -1
fi

for FILE in $CHECKFILEDIR/*.dat ; do
  echo "Checking $FILE ..."
  $MAIN $FILE > $TMPFILE
  # remove last line from tmp file, containing the measured time 
  sed -i '$ d' $TMPFILE
  # add command at beginning of each line
  sed -i 's/^/#/' $TMPFILE
  FAILED=0;
  while read LINE
  do
    if ! (grep -q "$LINE" "$FILE") ; then
      # echo "line '$LINE'  was not in file '$FILE'" 
      FAILED=1;
    fi
  done < $TMPFILE
  if [ $FAILED == 0 ] ; then
    echo "Check for file $FILE successful!"
  else
    echo "Check for file $FILE failed!"
  fi
  rm -rf $TMPFILE
done
echo "Checks done!"

