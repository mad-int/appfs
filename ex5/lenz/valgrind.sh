# valgrind check
for file in `ls ../../data/* | grep -v "test1d.dat" | grep -v "double_test1.dat"`
do
  echo "valgrind check of file " $file
  valgrind --leak-check=full --show-leak-kinds=all ./ex5 $file
done