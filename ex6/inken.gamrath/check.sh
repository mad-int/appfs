#!/bin/bash

TESTSET=$1.test
SOLUS=$1.solu
OUTFILE=check.out

touch $OUTFILE
rm $OUTFILE

TSTNAME=""
SOLS=ERROR
echo "compute solutions"

for INSTANCE in `cat $TESTSET`
   do
   echo "instance $INSTANCE"

#    echo "starting c computation"
   echo "@01 $INSTANCE" >> $OUTFILE
   ./bin/ex6 $INSTANCE &>> $OUTFILE
   echo "@04" >> $OUTFILE
   echo "=ready=" >> $OUTFILE
   echo "" >> $OUTFILE
done

echo "check solutions"

while read line
   do
#    echo $line
   fields=( $line )
   if [ ${#fields[@]} -ge 1 ]
      then
      if [ ${fields[0]} = '@04' ]
         then
#          echo $TSTNAME
#          echo $SOLS

         FOUND=0
         while read line
            do
         #    echo $line
            fields=( $line )
            if [ ${#fields[@]} -ge 2 ]
               then
               if [ ${fields[0]} = $TSTNAME ]
                  then
                  if [ $SOLS != ${fields[1]} ]
                     then
                     echo "Wrong solution for instance $TSTNAME: $SOLS != ${fields[1]}"
                  else
                     echo "Wright solution for instance $TSTNAME: $SOLS = ${fields[1]}"
                  fi
                  FOUND=1
                  break
               fi
            fi
         done < $SOLUS
         if [ $FOUND -ne 1 ]
            then
            echo "Instance $TSTNAME not found in solution file."
         fi

         TSTNAME=""
         SOLS="ERROR"
      else
         if [ ${fields[0]} = '@01' ]
            then
            TSTNAME=${fields[1]}
         else
            if [ ${#fields[@]} -ge 2 ] && [ ${fields[1]} = 'solutions' ]
               then
               SOLS=${fields[0]}
            fi
         fi
      fi
   fi
done < $OUTFILE