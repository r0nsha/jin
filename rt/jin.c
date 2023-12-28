#include <math.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

// Macros
#define FORCE_INLINE __attribute__((always_inline)) inline

// Builtin types
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

typedef struct never {
} unit;

typedef unit never;

typedef struct str {
  u8 *ptr;
  usize len;
} str;

typedef struct jin_rt_location {
  const char *path;
  u32 line;
  u32 column;
} jin_rt_location;

// Built-in functions
void jin_rt_panic(u8 *msg);
void jin_rt_panic_at(jin_rt_location loc, u8 *msg);
void *jin_rt_alloc(size_t size);
void jin_rt_free(void *ptr);

void jin_rt_panic(u8 *msg) {
  printf("panic: %s\n", msg);
  exit(1);
}

void jin_rt_panic_at(jin_rt_location loc, u8 *msg) {
  printf("panic at %s:%d:%d: %s\n", loc.path, loc.line, loc.column, msg);
  exit(1);
}

FORCE_INLINE void *jin_rt_alloc(size_t size) {
  void *ptr = malloc(size);

  if (!ptr) {
    jin_rt_panic("out of memory");
  }

  return ptr;
}

FORCE_INLINE void jin_rt_free(void *ptr) { free(ptr); }
