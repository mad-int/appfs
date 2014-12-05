#for i in err/* nonexisting_file.dat " "
#do
#   echo ==== Testing $i =====
#   $1 $i 
#done 
for i in dat/*.dat
do
   echo ==== Testing $i =====
   $1 $i 
done 
echo "All tests done"
