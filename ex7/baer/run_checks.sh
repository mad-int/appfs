#!/usr/bin/bash

# Just cover the usage-lines.
./ex7 2>&1 &> /dev/null

#
# Test if parsing is ok in this cases fail.
#
for file in `ls ../../data/corrupted_* ../../data/error_*` nofile.dat
do
    ./ex7 $file 2>&1 &> /dev/null
    RETVAL=$?

    if [ $RETVAL -eq 0 ];
    then
      echo "Error -> Parsing $file was successful."
      exit -1
    fi

    # Comment next line if you don't want positive messages.
    [ $RETVAL -ne 0 ] && echo "Ok -> Parsing $file failed.";
done

#
# Test if parsing is ok in this cases it should be successful.
#
for file in `ls ../../data/test?*.dat ../../data/markshare_3_?.dat ../../data/double_test1.dat ../../data/geqTest.dat ../../data/ones16.dat ../../data/test4.dat`
do
    ./ex7 $file 2>&1 &> /dev/null
    RETVAL=$?

    if [ $RETVAL -ne 0 ];
    then
      echo "Error -> Parsing $file failed."
      exit -1
    fi

    # Comment next line if you don't want positive messages.
    [ $RETVAL -eq 0 ] && echo "Ok -> Parsing $file was succesful.";
done

# TODO: Test some good cases and verifiy the solutions.
# What testing is suppose to do.

echo "All Ok."

