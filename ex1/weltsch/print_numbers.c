#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <endian.h>
#include <assert.h>

const int NUMBER_OF_INT_BYTES = sizeof(int32_t);

// comparing function for qsort
int compare_int(const void * a, const void * b) {
    if (*(int*)a > *(int*)b) return 1;
    if (*(int*)a < *(int*)b) return -1;
    return 0;
}

// calculate the size of the file in bytes
long get_file_size(FILE *file) {
    fseek(file, 0L, SEEK_END);
    long size = ftell(file);
    rewind(file);
    return size;
}

// used for assertions
// will check if an array is sorted
int assert_sorted(int32_t *numbers, int length) {
    int i, last, current;

    last = INT32_MIN;

    for (i = 0; i < length; i++) {
        current = numbers[i];
        if (last > current) {
            printf("last: %d, current: %d\n", last, current);
            return 0;
        }
        last = current;
    }

    return 1;
}

// if the array is sorted there is an easy way to leave out duplicates
void print_no_duplicates(int32_t *numbers, int length) {
    // printing without duplicates only works
    // if the array is correctly sorted
    assert(assert_sorted(numbers, length));

    int i;
    int32_t last_num = -1;
    for (i = 0; i < length; i++) {
        int32_t num = numbers[i];
        if (num >= 0 && num != last_num) {
            printf("%d\n", num);
        }
        last_num = num;
    }
}

int main(int argc, char* argv[]) {
    if (argc < 2) {
        printf("Please specify a filepath.\n");
        return 1;
    }

    FILE *file = fopen(argv[1], "r");

    if (NULL == file) {
        printf("An error occured while trying to open the specified file\n");
        return 1;
    }

    int32_t *buffer = malloc(NUMBER_OF_INT_BYTES);
    long file_size = get_file_size(file);

    int32_t *numbers = malloc(file_size);
    if (NULL == numbers) {
        printf("Could not allocate enough memory.\n");
    }

    int i = 0;
    while (fread(buffer, NUMBER_OF_INT_BYTES, 1, file) >= 1) {
        int32_t num = le32toh(*buffer);
        numbers[i] = num;
        i++;
    }

    free(buffer);
    fclose(file);

    int array_length = file_size / NUMBER_OF_INT_BYTES;
    qsort(numbers, array_length, NUMBER_OF_INT_BYTES, compare_int);

    print_no_duplicates(numbers, array_length);

    free(numbers);
    return 0;
}
