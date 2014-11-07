#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
/* I use strtok() to split the source multiline-string in single lines
 * on escapeing. But strtok() doesn't produce empty tokens.
 * This has the effect that empty lines don't result in empty tokens,
 * which is bad, because I can't use empty lines in the source-code.
 * So the source-code is somewhat dense without empty lines.
 */
const char source[] =
  "#include <assert.h>\n"
  "#include <stdio.h>\n"
  "#include <stdlib.h>\n"
  "#include <string.h>\n"
  "/* I use strtok() to split the source multiline-string in single lines\n"
  " * on escapeing. But strtok() doesn't produce empty tokens.\n"
  " * This has the effect that empty lines don't result in empty tokens,\n"
  " * which is bad, because I can't use empty lines in the source-code.\n"
  " * So the source-code is somewhat dense without empty lines.\n"
  " */\n"
  "const char source[] =\n"
  "%s\n"
  "char* escape_format(char* dest, const char* src, size_t src_size);\n"
  "int main(int argc, char** argv)\n"
  "{\n"
  "  // Allocated a buffer for the escaped source code that is big enough,\n"
  "  // 3x the size should be enough.\n"
  "  char* escaped_src = malloc(3*sizeof(source)*sizeof(char)+1);\n"
  "  escape_format(escaped_src, source, sizeof(source));\n"
  "  printf(source, escaped_src);\n"
  "  free(escaped_src);\n"
  "  return 0;\n"
  "}\n"
  "/**\n"
  " * Escapes and reformates a given string, so if given \"source\" above\n"
  " * it looks like \"source\" in the source code above.\n"
  " * dest is assumed to be big enough for it.\n"
  " */\n"
  "char* escape_format(char* dest, const char* const src, const size_t src_size)\n"
  "{\n"
  "  assert(dest != NULL);\n"
  "  assert(src != NULL);\n"
  "  assert(src_size > 0);\n"
  "  // strtok() changes its source-pointer so copy it before feeding it to strtok().\n"
  "  char* srctmp = malloc(src_size*sizeof(char));\n"
  "  strcpy(srctmp, src);\n"
  "  char* line = strtok(srctmp, \"\\n\");\n"
  "  while(line != NULL)\n"
  "  {\n"
  "    *dest = ' '; dest++;\n"
  "    *dest = ' '; dest++;\n"
  "    *dest = '\"'; dest++;\n"
  "    while(*line != '\\0')\n"
  "    {\n"
  "      // Escape only backslashes, newlines and \"\n"
  "      switch(*line)\n"
  "      {\n"
  "        case '\\\\':\n"
  "          *dest = '\\\\'; dest++;\n"
  "          *dest = '\\\\';\n"
  "          break;\n"
  "        case '\\n':\n"
  "          *dest = '\\\\'; dest++;\n"
  "          *dest = 'n';\n"
  "          break;\n"
  "        case '\"':\n"
  "          *dest = '\\\\'; dest++;\n"
  "          *dest = '\"';\n"
  "        default:\n"
  "          *dest = *line;\n"
  "          break;\n"
  "      }\n"
  "      dest++;\n"
  "      line++;\n"
  "    }\n"
  "    line = strtok(NULL, \"\\n\");\n"
  "    *dest = '\\\\'; dest++;\n"
  "    *dest = 'n'; dest++;\n"
  "    *dest = '\"'; dest++;\n"
  "    *dest = '\\n'; dest++;\n"
  "  }\n"
  "  // At the end change the newline with \";\".\n"
  "  dest--;\n"
  "  *dest = ';'; dest++;\n"
  "  *dest = '\\0';\n"
  "  free(srctmp);\n"
  "  return dest;\n"
  "}\n";
char* escape_format(char* dest, const char* src, size_t src_size);
int main(int argc, char** argv)
{
  // Allocated a buffer for the escaped source code that is big enough,
  // 3x the size should be enough.
  char* escaped_src = malloc(3*sizeof(source)*sizeof(char)+1);
  escape_format(escaped_src, source, sizeof(source));
  printf(source, escaped_src);
  free(escaped_src);
  return 0;
}
/**
 * Escapes and reformates a given string, so if given "source" above
 * it looks like "source" in the source code above.
 * dest is assumed to be big enough for it.
 */
char* escape_format(char* dest, const char* const src, const size_t src_size)
{
  assert(dest != NULL);
  assert(src != NULL);
  assert(src_size > 0);
  // strtok() changes its source-pointer so copy it before feeding it to strtok().
  char* srctmp = malloc(src_size*sizeof(char));
  strcpy(srctmp, src);
  char* line = strtok(srctmp, "\n");
  while(line != NULL)
  {
    *dest = ' '; dest++;
    *dest = ' '; dest++;
    *dest = '"'; dest++;
    while(*line != '\0')
    {
      // Escape only backslashes, newlines and "
      switch(*line)
      {
        case '\\':
          *dest = '\\'; dest++;
          *dest = '\\';
          break;
        case '\n':
          *dest = '\\'; dest++;
          *dest = 'n';
          break;
        case '"':
          *dest = '\\'; dest++;
          *dest = '"';
        default:
          *dest = *line;
          break;
      }
      dest++;
      line++;
    }
    line = strtok(NULL, "\n");
    *dest = '\\'; dest++;
    *dest = 'n'; dest++;
    *dest = '"'; dest++;
    *dest = '\n'; dest++;
  }
  // At the end change the newline with ";".
  dest--;
  *dest = ';'; dest++;
  *dest = '\0';
  free(srctmp);
  return dest;
}
