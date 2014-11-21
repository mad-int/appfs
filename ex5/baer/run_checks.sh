#!/usr/bin/bash

for file in `ls ../../data/corrupted_* ../../data/error_*`
do
    ./ex4 $file 2> /dev/null 1> /dev/null
    RETVAL=$?

    if [ $RETVAL -eq 0 ];
    then
      echo "Error -> Parsing $file was successful."
      exit -1
    fi

    # Comment next line if you don't want positive messages.
    [ $RETVAL -ne 0 ] && echo "Ok -> Parsing $file failed.";
done

#TODO: the positives are missing.

echo "All Ok."

