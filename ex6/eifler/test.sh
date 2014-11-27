home='pwd'

tests=(~/appfs/data/corrupt* ~/appfs/data/error* ~/appfs/data/test*)


for test in ${tests[@]}
   do
      ./ex6 $test print_info
   done
   mkdir cov/
   # location of compiled code is not known - try some...
   lcov -d obj/ -c -o cov/coverage.info
   lcov -d src/ -c -o cov/coverage.info
   lcov -d . -c -o cov/coverage.info
   genhtml -o cov cov/coverage.info
