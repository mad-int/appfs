#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* bucket sort, save the occurrence of each number between 0 and 2^31 - 1 in an zero/one array of size 2^31. Implemeneted via a char array of size 2^28. In the end output all numbers having a 1 starting by the number 0. */

void usage(){
    printf( "usage: ex1 <filename>\n" );
    exit(-1);
}

int main( int argc, char *argv[] ){
  
  if (argc != 2){
    usage();
  }
  
  unsigned int bucketSize= 268435456 ; /* = (positive numbers of int)/(number of bits in char) = (2 ^ 31) / 8 = 2 ^ 28 */
  
  FILE *fp;
  
  char *filename, *bucket, remainder;
  
  unsigned char code, c;
  
  int a, quotient ,count, i, j;
  
  bucket = malloc(bucketSize * sizeof(char));
  
  /* initialize bucket */
  for (i=0;i<bucketSize;i++){
    bucket[i]=0;
  }
  
  filename=argv[1];
  
  fp = fopen(filename,"rb");
  
  while(fread(&a,1,sizeof(int),fp) == sizeof(int)){
    
    if (a>=0){
      /* get the correct char position in the bucket = quotient 
       * and the correct bit position in the char = remainder */
      remainder = a % 8;
      quotient = a / 8;
      code = 1 << remainder;
      if (!((bucket[quotient] & code) >> remainder)){ /* if the bucket has no 1 on the bit of the code */
        
        bucket[quotient]+=code;

      }
    }
  }
  
  fclose(fp);
  
  count=0;
  
  for (i=0;i<bucketSize;i++){
    
    c=bucket[i];
    
    for (j=0;j<8;j++){
      
      if (c & 1){
        printf("%i\n",count);
      }
      
      count++;
      c=c >> 1;
    }
    
  }
  
  free(bucket);
  
  return 0;
}


