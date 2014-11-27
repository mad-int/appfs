for file in `ls ../../data/corrupted_* ../../data/error_*`
do
  echo "executing file " $file
  ./ex5 $file
  
  if [ $? -eq 1 ]
    then
      echo "Success"
    else
      echo "Failure"
  fi
done

# execute all tests that are expected to run without failure except for test1d.dat that is
# specified for double arguments
for file in `ls ../../data/markshare_* ../../data/ones* ../../data/test* | grep -v "test1d.dat"`
do
  echo "executing file " $file
  ./ex5 $file
  
  if [ $? -eq 0 ]
    then
      echo "Success"
    else
      echo "Failure"
  fi
done