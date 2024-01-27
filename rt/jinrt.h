#ifndef JINRT_H
#define JINRT_H
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

typedef struct array {
  void *data;
  usize refcnt;
} array;

typedef struct slice {
  array *array;
  void *start;
  usize len;
  usize cap;
} slice;

typedef void *jinrt_backtrace;

typedef struct jinrt_stackframe {
  const char *file;
  u32 line;
  const char *in;
} jinrt_stackframe;

// Initialization
void jinrt_init();

// Alloc
void *jinrt_alloc(size_t size);
void jinrt_free(jinrt_backtrace *backtrace, void *ptr, u8 *tyname,
                jinrt_stackframe frame);

// Slices
slice jinrt_slice_alloc(size_t elem_size, usize cap);
void jinrt_slice_free(jinrt_backtrace *backtrace, slice s, u8 *tyname,
                      jinrt_stackframe frame);
void jinrt_slice_incref(slice s);
void jinrt_slice_decref(slice s);
usize jinrt_slice_index_boundscheck(jinrt_backtrace *backtrace, slice s,
                                    usize index, jinrt_stackframe frame);

// Slice intrinsics
slice jinrt_slice_grow(jinrt_backtrace *backtrace, slice s, usize elem_size,
                       usize new_cap, jinrt_stackframe frame);
unit jinrt_slice_push_boundscheck(jinrt_backtrace *backtrace, slice s,
                                  jinrt_stackframe frame);

// Panic
void jinrt_panic_at(jinrt_backtrace *backtrace, u8 *msg,
                    jinrt_stackframe frame);

// Utils
bool jinrt_strcmp(str a, str b);

// Stack traces
jinrt_backtrace *jinrt_backtrace_new();
void jinrt_backtrace_free(jinrt_backtrace *backtrace);
void jinrt_backtrace_push(jinrt_backtrace *backtrace, jinrt_stackframe frame);
void jinrt_backtrace_pop(jinrt_backtrace *backtrace);
#endif
