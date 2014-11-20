#!/usr/bin/env bash
#
# compile the different codes (Makefiles are assumed to be present)

ex="ex5"
students=(inken.gamrath simon steger weltsch lang buerschaper schrezenmaier)

home=`pwd`

for student in ${students[@]}
do
   cd "$ex/$student"
   echo "compiling code for $ex/$student"
   make
   cd $home
done
