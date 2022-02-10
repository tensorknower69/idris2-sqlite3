#include <stdlib.h>

int sizeof_ptr() {
  return sizeof(void *);
}

void* join_ptr(void** ptr) {
  return *ptr;
}
