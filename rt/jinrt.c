#include "jinrt.h"

FORCE_INLINE void *jin_rt_alloc(size_t size) {
  void *ptr = malloc(size);

  if (!ptr) {
    jin_rt_panic("out of memory");
  }

  return ptr;
}

FORCE_INLINE void jin_rt_free(void *ptr) { free(ptr); }

FORCE_INLINE void jin_rt_panic(u8 *msg) {
  dprintf(STDERR, "panic at '%s'\n", msg);
  exit(1);
}

FORCE_INLINE void jin_rt_panic_at(u8 *msg, jin_rt_location loc) {
  dprintf(STDERR, "panic at '%s', %s:%d:%d\n", msg, loc.path, loc.line,
          loc.column);
  exit(1);
}

void jin_rt_refcheck(usize refcnt, u8 *fmt, jin_rt_location loc) {
  if (refcnt != 0) {
    size_t needed = snprintf(NULL, 0, fmt, refcnt);
    char *msg = malloc(needed);
    sprintf(msg, fmt, refcnt);
    jin_rt_panic_at(msg, loc);
  }
}

FORCE_INLINE bool jin_rt_strcmp(str a, str b) {
  return strcmp(a.ptr, b.ptr) == 0;
}
