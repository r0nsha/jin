#ifndef JINRT_H
#define JINRT_H
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

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

typedef struct jinrt_location {
  const char *path;
  u32 line;
  u32 column;
} jinrt_location;

typedef void *jinrt_backtrace;

typedef struct jinrt_stackframe {
  const char *file;
  u32 line;
  const char *in;
} jinrt_stackframe;

void jinrt_init();
void *jinrt_alloc(size_t size);
void jinrt_free(void *ptr, u8 *tyname, jinrt_location loc);
void jinrt_panic(u8 *msg);
void jinrt_panic_at(u8 *msg, jinrt_location loc);
bool jinrt_strcmp(str a, str b);
jinrt_backtrace *jinrt_backtrace_new();
void jinrt_backtrace_push(jinrt_backtrace *backtrace, const char *file,
                          u32 line, const char *in);
void jinrt_backtrace_pop(jinrt_backtrace *backtrace);
#endif
