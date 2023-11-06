#include <stdbool.h>
#include <stdint.h>

typedef int8_t i8_t;
typedef int16_t i16_t;
typedef int32_t i32_t;
typedef int64_t i64_t;
typedef intptr_t int_t;

typedef uint8_t u8_t;
typedef uint16_t u16_t;
typedef uint32_t u32_t;
typedef uint64_t u64_t;
typedef uintptr_t uint_t;

typedef struct {
} unit_t;
typedef unit_t never_t;
typedef bool bool_t;

typedef struct {
  u8_t *ptr;
  uint_t len;
} str_t;
