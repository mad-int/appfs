#!/usr/bin/env bash
#
# compile the different codes (Makefiles are assumed to be present)

home=`pwd`

ex="ex5"

students=(inken.gamrath simon steger weltsch lang buerschaper schrezenmaier iupinov)

tests=("$home"/data/corrupt* "$home"/data/error* "$home"/data/test*)

for student in ${students[@]}
do
   cd "$ex/$student"
   echo "compiling code for $ex/$student"
   make clean
   make

   for test in ${tests[@]}
   do
      ./ex5 $test
   done
   mkdir cov/
   # location of compiled code is not known - try some...
   lcov -d obj/ -c -o cov/coverage.info
   lcov -d src/ -c -o cov/coverage.info
   lcov -d . -c -o cov/coverage.info
   genhtml -o cov cov/coverage.info
   cd "$home"
done

