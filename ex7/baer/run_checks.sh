#!/usr/bin/bash

# Just for coverage of the usage-lines.
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

#
# Test if calculation is correct.
#
# You can add new testcases by putting the solution of say <test>.dat
# in <test>.sol.
# Every line should contain one solution vector in this format e.g.:
# > (1, 0, 0, 0)
# This vectors should be sorted.
#
# This sol-files can be generated from ex7 by:
# > ./ex7 test.dat | grep "^(" | sort > test.sol
#
# Note: I generated the sol-files with an old version (ex5),
# that didn't do is_feasible_bitflip(),
# but the old far more reliable way is_feasible().
# I don't trust is_feasible_bitflip() too much magic involved.
#
# (Needless to say that is_feasible_bitflip() got one right,
# where the old one ex5 failed (test4.dat)).
#

for solfile in `ls sols/*.sol`
do
    # sols/test1.sol --> ../../data/test1.dat
    testfile=`echo $solfile | sed -e 's/^sols\/\(.*\).sol$/..\/..\/data\/\1.dat/'`
    ./ex7 $testfile | grep "^(" | sort | diff - $solfile
    RETVAL=$?

    if [ $RETVAL -ne 0 ];
    then
      echo "Error -> Calculated solution of $testfile seems to be wrong."
      exit -1
    fi

    # Comment next line if you don't want positive messages.
    [ $RETVAL -eq 0 ] && echo "Ok -> Calculated solution of $testfile seems to be correct."

done

echo "All Ok."

