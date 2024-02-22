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

typedef struct unit {
} unit;

typedef unit never;

typedef struct array {
  void *data;
  u32 refcnt;
} array;

typedef struct slice {
  array *array;
  void *start;
  usize len;
} slice;

typedef slice str;

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
slice jinrt_slice_slice(jinrt_backtrace *backtrace, slice s, size_t elem_size,
                        usize low, usize high, jinrt_stackframe frame);
slice jinrt_slice_grow(jinrt_backtrace *backtrace, slice s, size_t elem_size,
                       usize new_cap, jinrt_stackframe frame);
usize jinrt_slice_cap(slice *s);

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
