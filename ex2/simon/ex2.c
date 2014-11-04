#include<stdio.h>
void prnt(char* name){
  FILE *f=fopen(name,"r");
  char c;
  while(fread(&c, sizeof(c), 1, f))
    printf("%c", c);
  fclose(f);
}
void cpPrnt(char* name,char fchar){
  FILE *f=fopen(name,"r");
  printf("  fprintf(%c,\"",fchar);
  char c,b=0;
  while(fread(&c, sizeof(c), 1, f)){
    if (1==b){
      printf("  fprintf(%c,\"", fchar);
      b=0;
    }
    if (c=='\n'){
      printf("\\n\");\n");
      b=1;
    }
    else{
      if (c=='"' ||  c=='\\'){
        printf("\\%c", c);}
      else{
        if (c=='%'){
          printf("%%%%");}
        else{
          printf("%c", c);}
      }
    }
  }
  fclose(f);
}
int main(){
  FILE *f=fopen("fst","w");
  FILE *s=fopen("snd","w");
  fprintf(f,"#include<stdio.h>\n");
  fprintf(f,"void prnt(char* name){\n");
  fprintf(f,"  FILE *f=fopen(name,\"r\");\n");
  fprintf(f,"  char c;\n");
  fprintf(f,"  while(fread(&c, sizeof(c), 1, f))\n");
  fprintf(f,"    printf(\"%%c\", c);\n");
  fprintf(f,"  fclose(f);\n");
  fprintf(f,"}\n");
  fprintf(f,"void cpPrnt(char* name,char fchar){\n");
  fprintf(f,"  FILE *f=fopen(name,\"r\");\n");
  fprintf(f,"  printf(\"  fprintf(%%c,\\\"\",fchar);\n");
  fprintf(f,"  char c,b=0;\n");
  fprintf(f,"  while(fread(&c, sizeof(c), 1, f)){\n");
  fprintf(f,"    if (1==b){\n");
  fprintf(f,"      printf(\"  fprintf(%%c,\\\"\", fchar);\n");
  fprintf(f,"      b=0;\n");
  fprintf(f,"    }\n");
  fprintf(f,"    if (c=='\\n'){\n");
  fprintf(f,"      printf(\"\\\\n\\\");\\n\");\n");
  fprintf(f,"      b=1;\n");
  fprintf(f,"    }\n");
  fprintf(f,"    else{\n");
  fprintf(f,"      if (c=='\"' ||  c=='\\\\'){\n");
  fprintf(f,"        printf(\"\\\\%%c\", c);}\n");
  fprintf(f,"      else{\n");
  fprintf(f,"        if (c=='%%'){\n");
  fprintf(f,"          printf(\"%%%%%%%%\");}\n");
  fprintf(f,"        else{\n");
  fprintf(f,"          printf(\"%%c\", c);}\n");
  fprintf(f,"      }\n");
  fprintf(f,"    }\n");
  fprintf(f,"  }\n");
  fprintf(f,"  fclose(f);\n");
  fprintf(f,"}\n");
  fprintf(f,"int main(){\n");
  fprintf(f,"  FILE *f=fopen(\"fst\",\"w\");\n");
  fprintf(f,"  FILE *s=fopen(\"snd\",\"w\");\n");
  fprintf(s,"  fclose(f);\n");
  fprintf(s,"  fclose(s);\n");
  fprintf(s,"  prnt(\"fst\");\n");
  fprintf(s,"  cpPrnt(\"fst\",'f');\n");
  fprintf(s,"  cpPrnt(\"snd\",'s');\n");
  fprintf(s,"  prnt(\"snd\");\n");
  fprintf(s,"  remove(\"fst\");\n");
  fprintf(s,"  remove(\"snd\");\n");
  fprintf(s,"}\n");
  fclose(f);
  fclose(s);
  prnt("fst");
  cpPrnt("fst",'f');
  cpPrnt("snd",'s');
  prnt("snd");
  remove("fst");
  remove("snd");
}
