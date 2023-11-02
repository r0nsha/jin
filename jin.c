#include <stdint.h>
#include <stdbool.h>

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

typedef struct {} Unit;
typedef Unit Never;

typedef struct {
  U8* data;
  Uint len;
} Str;
