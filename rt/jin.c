#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

#define FORCE_INLINE __attribute__((always_inline)) inline

typedef int8_t i8;
typedef int16_t i16;
typedef int32_t i32;
typedef int64_t i64;
typedef intptr_t isize;

typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;
typedef uintptr_t usize;

typedef float f32;
typedef double f64;

typedef struct {
} unit;
typedef unit never;

typedef struct {
  u8 *ptr;
  usize len;
} str;

FORCE_INLINE void *__jin_alloc(size_t size) { return malloc(size); }

FORCE_INLINE void __jin_dealloc(void *ptr) { free(ptr); }
