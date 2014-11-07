#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <errno.h>

// 1 GB = 1073741824 = 1024*1024*1024
#define MAX_BUFFER_SIZE 1073741824

// 100 MB = 104857600 = 100*1024*1024
//#define MAX_BUFFER_SIZE 104857600

// 500 MB = 524288000 = 500*1024*1024
//#define MAX_BUFFER_SIZE 524288000

#define assert_filebuffer(B) assert(NULL != (B).data); \
  assert((B).pos <= (B).length); \
  assert(NULL != (B).path); \
  assert((B).filepos <= (B).filesize)

// ---

typedef struct filebuffer_t
{
  int32_t* data;
  size_t length;
  size_t pos;

  const char* path;
  size_t filepos;
  size_t filesize;

} filebuffer_t;

// ---

filebuffer_t malloc_filebuffer(const char* const path, const size_t maxsize);
void free_filebuffer(const filebuffer_t b);

int write_filebuffer(const filebuffer_t b, const char* const path);

int read_next_chunk(filebuffer_t* b);
void advance_filebuffer(const int32_t last, filebuffer_t* b);

int32_t min(const filebuffer_t b1, const filebuffer_t b2);

// ---

int cmp(const void* e1, const void* e2)
{
  return *((int32_t*)e1) >= *((int32_t*)e2);
}

// ---

int main(int argc, char** argv)
{
  assert(0 == (MAX_BUFFER_SIZE%4));
  assert(0 == ((MAX_BUFFER_SIZE/2)%4));

  const char* outfile[] = {
    "ndata.tmp", "ndata.tmp1", "ndata.tmp2", "ndata.tmp3", "ndata.tmp4",
    "ndata.tmp5", "ndata.tmp6", "ndata.tmp7", "ndata.tmp8", "ndata.tmp9"};
  //int buffer_length[] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0};

  // ---

  int chunks = 0;

  {
    filebuffer_t b = malloc_filebuffer("ndata.dat", MAX_BUFFER_SIZE);

    // At the first run, validate integrity of the data.
    assert_filebuffer(b);
    assert(123456789 == b.data[0]);

    for(chunks=0; chunks<10; chunks++)
    {
      if(chunks != 0)
        read_next_chunk(&b);

      if(0 != b.length)
      {
        qsort(b.data, b.length, sizeof(int32_t), cmp);

        // Omit the negative numbers at the beginning.
        b.pos = 0;
        for(b.pos=0; b.pos<b.length; b.pos++)
          if(b.data[b.pos] >= 0)
            break;

        int success = write_filebuffer(b, outfile[chunks]);
        assert(success);

        // start = b.length*sizeof(int32_t);
        // buffer_length[chunks] =  b.length - b.pos;
      }
      else
        break;
    }

    free_filebuffer(b);
  }

  // ---

  if(1 == chunks)
  {
    filebuffer_t b = malloc_filebuffer(outfile[0], MAX_BUFFER_SIZE);

    assert_filebuffer(b);

    int32_t last = b.data[0];
    printf("%d\n", last);

    for(b.pos=0; b.pos<b.length; b.pos++)
      if(last != b.data[b.pos])
      {
        last = b.data[b.pos];
        printf("%d\n", last);
      }
  }
  else if(2 == chunks)
  {
    filebuffer_t b1 = malloc_filebuffer(outfile[0], MAX_BUFFER_SIZE/2);
    filebuffer_t b2 = malloc_filebuffer(outfile[1], MAX_BUFFER_SIZE/2);

    assert_filebuffer(b1);
    assert_filebuffer(b2);

    while((b1.pos < b1.length) || (b2.pos < b2.length))
    {
      const int32_t last = min(b1, b2);
      printf("%d\n", last);

      advance_filebuffer(last, &b1);
      advance_filebuffer(last, &b2);
    }

    free_filebuffer(b1);
    free_filebuffer(b2);
  }
  else
  { // Only up to 2 chunks are supported yet ->  0 < chunks <= 2;
    fprintf(stderr, "Error: your data is too large - only 2 data chunks are supported yet. Do nothing.\n");
    return -1;
  }

  return 0;
}


// ---

filebuffer_t malloc_filebuffer(const char* const path, const size_t maxsize)
{
  // PRE-CONDITION
  assert(NULL != path);
  assert(0 == (maxsize%sizeof(int32_t)));

  // Open file.
  FILE* const file = fopen(path, "rb");
  if(NULL == file)
  {
    const int err = errno;
    fprintf(stderr, "Error: Can't open file `%s': %s!\n", path, strerror(err));

    filebuffer_t ret = {NULL, 0, 0,  NULL, 0, 0};
    return ret;
  }

  // Determine file size.
  assert( 0 == fseek(file, 0L, SEEK_END) );
  const long size = ftell(file);
  assert( 0 == fseek(file, 0L, SEEK_SET) );

  //fprintf(stderr, "size of the file is: %ld bytes\n", size);

  assert(0 == (size%sizeof(int32_t)));

  const size_t length = (size < maxsize ? size : maxsize) / sizeof(int32_t);

  // filebuffer_t -> { data, length, pos,  path, filepos, filesize }
  filebuffer_t buffer = { malloc(length*sizeof(int32_t)), length, 0,  path, 0, size };
  assert(NULL != buffer.data);

  // Read file content.
  const size_t read_numbers = fread(buffer.data, sizeof(int32_t), buffer.length, file);
  buffer.filepos = read_numbers*sizeof(int32_t);

  // Good idea? Maybe better to assert() it.
  if(read_numbers != buffer.length)
  {
    fprintf(stderr, "Warning: Can't read %ld bytes but only %ld bytes of data from file `%s'!\n",
          buffer.length*sizeof(int32_t), read_numbers*sizeof(int32_t), path);
    buffer.length = read_numbers;
  }

  fclose(file);

  /* POST-CONDITION
   * Returned is an allocated buffer_list containing the (maybe partial) content of the file given
   * or NULL on error.
   */
  return buffer;
}

void free_filebuffer(const filebuffer_t b)
{
  if(NULL != b.data)
    free(b.data);
}

int write_filebuffer(const filebuffer_t b, const char* const path)
{
  // PRE-CONDITION
  assert(NULL != path);
  assert_filebuffer(b);

  // Open file.
  FILE* const file = fopen(path, "wb");
  if(NULL == file)
  {
    const int err = errno;
    fprintf(stderr, "Error: Can't open file for writing `%s': %s!\n", path, strerror(err));
    return 0;
  }

  // Write file content.
  const size_t written_numbers = fwrite(b.data+b.pos, sizeof(int32_t), b.length-b.pos, file);

  if(written_numbers != (b.length-b.pos))
  {
    fprintf(stderr, "Warning: Can't write %ld bytes but only %ld bytes of data to file `%s'!\n",
        (b.length-b.pos)*sizeof(int32_t), written_numbers*sizeof(int32_t), path);

    fclose(file);
    return 0;
  }

  fclose(file);
  return 1;
}

int read_next_chunk(filebuffer_t* const b)
{
  assert_filebuffer(*b);

  // Open file.
  FILE* const file = fopen(b->path, "rb");
  if(NULL == file)
  {
    const int err = errno;
    fprintf(stderr, "Error: Can't open file `%s': %s!\n", b->path, strerror(err));

    free(b->data);
    b->data = NULL; b->pos = 0; b->length = 0;
    b->filepos = 0; b->filesize = 0;
    return 0;
  }

  assert( 0 == fseek(file, b->filepos, SEEK_SET) );
  assert(0 == ((b->filesize-b->filepos)%sizeof(int32_t)));

  const size_t rest_length = (b->filesize - b->filepos) / sizeof(int32_t);
  const size_t length = rest_length < b->length ? rest_length : b->length;

  const size_t read_numbers = fread(b->data, sizeof(int32_t), length, file);
  b->pos = 0;
  b->length = length;
  b->filepos += read_numbers*sizeof(int32_t);

  // Good idea? Maybe better to assert() it.
  if(read_numbers != length)
  {
    fprintf(stderr, "Warning: Can't read %ld bytes but only %ld bytes of data from file `%s'!\n",
          length*sizeof(int32_t), read_numbers*sizeof(int32_t), b->path);
    b->length = read_numbers;

    fclose(file);
    return 0;
  }

  fclose(file);
  return 1;
}

void advance_filebuffer(const int32_t last, filebuffer_t* const b)
{
  while((b->pos < b->length) && (last == b->data[b->pos]))
    b->pos++;

  if(b->pos >= b->length)
  {
    read_next_chunk(b);

    while((b->pos < b->length) && (last == b->data[b->pos]))
      b->pos++;
  }
}

int32_t min(const filebuffer_t b1, const filebuffer_t b2)
{
  assert((b1.pos < b1.length) || (b2.pos < b2.length));

  if(b1.pos >= b1.length)
    return b2.data[b2.pos];

  if(b2.pos >= b2.length)
    return b1.data[b1.pos];

  return (b1.data[b1.pos] < b2.data[b2.pos]) ? b1.data[b1.pos] : b2.data[b2.pos];
}

