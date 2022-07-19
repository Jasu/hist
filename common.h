#pragma once

#define _GNU_SOURCE 1
#define _POSIX_C_SOURCE 200809L

#include <stdint.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdnoreturn.h>
#include <tdb.h>

#define HIST_UNLIKELY(COND) __builtin_expect((COND), 0)
#define HIST_LIKELY(COND) __builtin_expect((COND), 1)
#define HIST_FAIL(...) fail(__FUNCTION__, __LINE__ __VA_OPT__(,) __VA_ARGS__)
#define HIST_CHECK(COND, ...) if (HIST_UNLIKELY(!(COND))) { HIST_FAIL(__VA_ARGS__); }
#define HIST_INLINE __attribute__((always_inline)) inline static
#define HIST_NOINLINE __attribute__((noinline))
#define HIST_NORETURN _Noreturn
#define HIST_NODISC __attribute__((warn_unused_result))
#define HIST_NODISC_PTR __attribute__((warn_unused_result,returns_nonnull))
#define HIST_NONNULL __attribute__((nonnull))
#define HIST_UNUSED __attribute__((unused))

#define HIST_NUM_BUCKETS 4

#ifndef NDEBUG
#if __has_builtin(__builtin_debugtrap)
#define HIST_ENTER_DEBUGGER() __builtin_debugtrap()
#else
#define HIST_ENTER_DEBUGGER() __asm__ volatile("int $0x03")
#endif
#else
#define HIST_ENTER_DEBUGGER() do {} while(false)
#endif

typedef enum hist_ft {
  hist_ft_missing,
  hist_ft_dir,
  hist_ft_file,
  hist_ft_other,
} hist_ft_t;

typedef uint32_t hist_id_t;

#define HIST_ID_TYPE_NONE 0
#define HIST_ID_TYPE_BUCKET 1
#define HIST_ID_TYPE_DIR 2
#define HIST_ID_TYPE_MASK 15
#define HIST_ID_VALUE_SHIFT 4

#define HIST_ID_TYPE(ID) (ID & HIST_ID_TYPE_MASK)
#define HIST_ID(TYPE, VALUE) (hist_id_t)(((VALUE) << HIST_ID_VALUE_SHIFT) | HIST_ID_TYPE_##TYPE)
#define HIST_BUCKET_ID(INDEX) HIST_ID(BUCKET, INDEX)
#define HIST_DIR_ID(HASH) HIST_ID(DIR, HASH)
#define HIST_ID_VALUE(ID) (ID >> HIST_ID_VALUE_SHIFT)
#define HIST_ID_NONE (hist_id_t)0

#define HIST_ID_P(ID) (bool)HIST_ID_TYPE(ID)
#define HIST_BUCKET_ID_P(ID) (HIST_ID_TYPE(ID) == HIST_ID_TYPE_BUCKET)
#define HIST_DIR_ID_P(ID) (HIST_ID_TYPE(ID) == HIST_ID_TYPE_DIR)
#define HIST_DIR_ID_ROOT (hist_id_t)0x285562e2U
#define HIST_TDB_KEY(N) (TDB_DATA){(unsigned char*)&N, 4}
#define HIST_TDB_VAL(PTR, SZ) (TDB_DATA){(unsigned char*)PTR, SZ}
