#include <stdio.h>
#include <string.h>
#include "jinrt.h"

FORCE_INLINE void *jinrt_alloc(size_t size) {
  void *ptr = malloc(size);

  if (!ptr) {
    jinrt_panic("out of memory");
  }

  return ptr;
}

FORCE_INLINE void jinrt_free(void *ptr) { free(ptr); }

FORCE_INLINE void jinrt_panic(u8 *msg) {
  dprintf(STDERR, "panic at '%s'\n", msg);
  exit(1);
}

FORCE_INLINE void jinrt_panic_at(u8 *msg, jinrt_location loc) {
  dprintf(STDERR, "panic at '%s', %s:%d:%d\n", msg, loc.path, loc.line,
          loc.column);
  exit(1);
}

void jinrt_refcheck(usize refcnt, u8 *fmt, jinrt_location loc) {
  if (refcnt != 0) {
    size_t needed = snprintf(NULL, 0, fmt, refcnt);
    char *msg = malloc(needed);
    sprintf(msg, fmt, refcnt);
    jinrt_panic_at(msg, loc);
  }
}

FORCE_INLINE bool jinrt_strcmp(str a, str b) {
  return strcmp(a.ptr, b.ptr) == 0;
}
