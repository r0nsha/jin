#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

typedef int8_t I8;
typedef int16_t I16;
typedef int32_t I32;
typedef int64_t I64;
typedef intptr_t Int;

typedef uint8_t U8;
typedef uint16_t U16;
typedef uint32_t U32;
typedef uint64_t U64;
typedef uintptr_t Uint;

typedef struct {
} Unit;
typedef Unit Never;
typedef bool Bool;

typedef struct {
  U8 *ptr;
  Uint len;
} Str;

void jin_panic_arithmetic_overflow(U8 *opname) {
  printf("panic: attempt to %s with overflow\n", opname);
  exit(1);
}
