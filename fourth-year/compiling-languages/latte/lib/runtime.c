#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// TAKEN FROM ELSEWHERE. NOT WRITTEN BY ME
void error() {
  fprintf(stderr, "runtime error\n");
  exit(1);
}

void printString(char *str) {
  if (str == NULL) {
    printf("\n");
  } else {
    printf("%s\n", str);
  }
}

// Returns pointer to beginning of the string, which is packed into the box for
// metadata
char *__read_string() {
  const size_t metadata_padding = 8;
  const size_t d_word = 4;
  size_t buffer_size = 0;
  char *buffer = NULL;

  size_t len_read = getline(&buffer, &buffer_size, stdin);
  if (len_read <= 0) {
    return NULL;
  }

  buffer[len_read - 1] = '\0';
  char *result = calloc(metadata_padding + len_read, d_word);
  memcpy(result + metadata_padding, buffer, len_read);
  free(buffer);

  return result + metadata_padding;
}
char *readString() {
    // Get the string from __read_string
    char *padded_string = __read_string();
    if (padded_string == NULL) {
        return NULL; // Handle errors or EOF
    }

    // Calculate the length of the string
    size_t string_length = strlen(padded_string);

    // Allocate memory for the clean string (no metadata)
    char *clean_string = malloc(string_length + 1); // +1 for the null terminator
    if (clean_string == NULL) {
        free(padded_string - 8); // Free the original allocated memory
        return NULL;
    }

    // Copy the string to the new memory
    strcpy(clean_string, padded_string);

    // Free the original memory (adjust pointer to include metadata)
    free(padded_string - 8);

    return clean_string;
}

void printInt(int x) { printf("%d\n", x); }

char *__read_int_helper() {
  size_t buffer_size = 0;
  char *buffer = NULL;
  size_t len_read = getline(&buffer, &buffer_size, stdin);
  if (len_read <= 0) {
    return NULL;
  }
  buffer[len_read - 1] = '\0';
  return buffer;
}

int readInt() {
  char *input = __read_int_helper();
  int read_value = atoi(input);
  free(input);
  return read_value;
}

// string memory layout:
// [dtr, ref_cnt, c0, c1, ...]
//                ^ pointer to str
char *_concatStrings(void *str1, void *str2) {
  const size_t metadata_padding = 8;

  if (str1 == NULL && str2 == NULL) {
    return NULL;
  } else if (str1 == NULL) {
    size_t length = strlen(str2);
    char *result = malloc(length + 1 + metadata_padding);
    memcpy(result + metadata_padding, str2, length + 1);
    return result + metadata_padding;
  } else if (str2 == NULL) {
    size_t length = strlen(str1);
    char *result = malloc(length + 1 + metadata_padding);
    memcpy(result + metadata_padding, str1, length + 1);
    return result + metadata_padding;
  } else {
    size_t length1 = strlen(str1);
    size_t length2 = strlen(str2);

    char *result = malloc(length1 + length2 + 1 + metadata_padding);
    memcpy(result + metadata_padding, str1, length1);
    memcpy(result + metadata_padding + length1, str2,
           length2 + 1); // include end_of_string

    return result + metadata_padding;
  }
}

char *__rstrconcat(void *str1, void *str2) { return _concatStrings(str2, str1); }

// In Latte implementation NULL string == empty string
int _compareStrings(const char *str1, const char *str2) {
  const char *empty_str = "";
  if ((str1 == NULL || (strcmp(str1, empty_str) == 0)) &&
      (str2 == NULL || (strcmp(str2, empty_str) == 0))) {
    return 1;
  } else if (str1 == NULL || str2 == NULL) {
    return 0;
  }
  return strcmp(str1, str2) == 0;
}

void __incr_ref_counter(void *memory_loc) {
  if (memory_loc != NULL) {
    int *p = (int *)memory_loc;
    int *ref_counter = p - 1;
    (*ref_counter)++;
  }
}
// define i8* @_malloc(i32 %size) {
//   %res = call i8* @malloc(i32 %size)
//   ret i8* %res
// }
void *_malloc(int size) {
  int *result = malloc(size);
  // memset to 0 
  memset(result, 0, size);
  return result;
}

int _count_arr_length(void *arr, int elem_size) {
  if (arr == NULL) {
    return 0;
  }
  int *p = (int *)arr;
  int *length = p - 1;
  return *length / elem_size;
}