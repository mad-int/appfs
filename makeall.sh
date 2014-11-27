#!/usr/bin/env bash
#
# compile the different codes (Makefiles are assumed to be present)

home=`pwd`

# don't do coverage every time
coverage=0

exes=(ex5 ex6)
tests=("$home"/data/corrupt* "$home"/data/error* "$home"/data/test*)

for ex in ${exes[@]}
do
   students=(`find $ex -maxdepth 1 -mindepth 1 -type d`)

   for student in ${students[@]}
   do
      cd "$student"
      echo "compiling code for $student"
      make clean
      make

      for test in ${tests[@]}
      do
         ./$ex $test
      done

      if [ $coverage -eq 1 ]; then
         mkdir -p cov/
         # location of compiled code is not known - try some...
         lcov -d obj/ -c -o cov/coverage.info
         lcov -d src/ -c -o cov/coverage.info
         lcov -d . -c -o cov/coverage.info
         genhtml -o cov cov/coverage.info
      fi

      cd "$home"
   done
done

exit
