#include "common.h"

#include <sys/stat.h>
#include <stdarg.h>
#include <limits.h>
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <math.h>

#define XXH_INLINE_ALL
#include <xxhash.h>

typedef struct hist_dir {
  uint8_t buckets;
  uint8_t depth;
  uint32_t last_timestamp;
  float frecencies[HIST_NUM_BUCKETS];
  hist_id_t parent_id;
  hist_id_t grandparent_id;
  char path[0];
} hist_dir_t;

#define HIST_DIR_IN_BUCKET_P(D, N) ((D)->buckets & (1 << N))
#define HIST_DIR_SET_BUCKET(D, N) (D)->buckets |= 1 << N
#define HIST_DIR_UNSET_BUCKET(D, N) (D)->buckets &= ~(1 << N)

#define FOR_BUCKETS(IDX) for (uint32_t IDX = 0; IDX < HIST_NUM_BUCKETS; ++IDX)
#define FOR_BUCKET_ENTRIES(IT, IDX) \
  for (hist_bucket_entry_t *IT = hist_buckets[IDX]->entries, \
       *_##IT##_end = IT + hist_buckets[IDX]->size;        \
       IT != _##IT##_end; ++IT)
#define FOR_BUCKET_ENTRIES_CONTINUE_AFTER_DEL(IT) --IT; --_##IT##_end; continue

HIST_INLINE float hist_exp2f(float x) {
    if (x <= -126) return 0;
    float y;
    int exponent = (int)(x + 127);
    x += 127 - exponent;
    exponent <<= 23;
    for (int j = 0; j < 4; j++) {
      ((char *)&y)[j] = ((char *)&exponent)[j]; 
    }
    x *= x * 0.339766027f + 0.660233972f;
    return (x + 1) * y;
}


typedef struct hist_bucket_entry {
  hist_id_t dir_id;
  uint32_t last_timestamp;
  float frecency;
} hist_bucket_entry_t;

typedef struct hist_bucket {
  uint32_t size;
  hist_bucket_entry_t entries[0];
} hist_bucket_t;

typedef int (*hist_tx_cb_t)(struct tdb_context *);

typedef struct hist_tx_fn {
  const hist_tx_cb_t fn;
  const char err[20];
} hist_tx_fn_t;

typedef struct hist_tx_type {
  const hist_tx_fn_t begin;
  const hist_tx_fn_t commit;
  const hist_tx_fn_t cancel;
} hist_tx_type_t;

static const hist_tx_type_t tx_read = {
  { tdb_lockall_read, "Read lock failed" },
  { tdb_unlockall_read, "Read unlock failed" },
  { tdb_unlockall_read, "Read cancel failed" }
};

static const hist_tx_type_t tx_write = {
  { tdb_transaction_start, "TX start failed" },
  { tdb_transaction_commit, "TX commit failed" },
  { tdb_transaction_cancel, "TX cancel failed" }
};


static uint32_t cur_timestamp = 0;
static hist_bucket_t *hist_buckets[HIST_NUM_BUCKETS] = {NULL, NULL, NULL, NULL};
static const hist_tx_type_t *cur_tx = NULL;

#define HIST_BUCKET_CAPACITY(N) (16U << (N))
#define HIST_BUCKET_BYTES(N) (sizeof(hist_bucket_t) + ((sizeof(hist_bucket_entry_t) * 16) << (N)))

#define HIST_EXPONENT(HALF_LIFE_MINUTES) -0.01666666f / HALF_LIFE_MINUTES
static const float frecency_decay_exponents[HIST_NUM_BUCKETS] = { HIST_EXPONENT(10.0f), HIST_EXPONENT(240.0f), HIST_EXPONENT(3000.0f), HIST_EXPONENT(10000.f) };

static struct tdb_context* tdb_ctx = NULL;

HIST_NORETURN HIST_NOINLINE static void fail(const char * HIST_NONNULL fn, int32_t line, const char * HIST_NONNULL err, ...) {
  va_list args;
  va_start(args, err);
  vdprintf(STDERR_FILENO, err, args);
  va_end(args);
  dprintf(STDERR_FILENO, " at %s:%d\n", fn, line);
  if (tdb_ctx) {
    if (tdb_error(tdb_ctx)) {
      dprintf(STDERR_FILENO, "TDB error: %s\n", tdb_errorstr(tdb_ctx));
    }
    if (cur_tx) {
      const hist_tx_fn_t fn = cur_tx->cancel;
      cur_tx = NULL;
      HIST_CHECK(!fn.fn(tdb_ctx), fn.err);
    }
    tdb_close(tdb_ctx);
  }
  HIST_ENTER_DEBUGGER();
  exit(1);
}

static unsigned int hist_tdb_hash(TDB_DATA *key) {
  return XXH3_64bits(key->dptr, key->dsize);
}

HIST_NODISC inline hist_id_t dir_id(const char * HIST_NONNULL path, uint32_t len) {
  return HIST_DIR_ID(XXH3_64bits(path, len));
}

HIST_NODISC HIST_NOINLINE static float compute_real_frecency(float frecency, uint32_t old_ts, hist_id_t bucket_index) {
  return hist_exp2f((cur_timestamp - old_ts) * frecency_decay_exponents[bucket_index]) * frecency;
}

HIST_NODISC_PTR static hist_dir_t * create_dir(const char * HIST_NONNULL path, uint32_t path_len) {
  hist_dir_t *result = (hist_dir_t*)calloc(1, sizeof(hist_dir_t) + path_len + 1);
  FOR_BUCKETS(i) {
    result->frecencies[i] = 1.0f;
  }
  memcpy(result->path, path, path_len);
  result->depth = path_len != 1;
  const char *slash1 = path + 1, *slash2 = path + 1, *cur = path;
  while ((cur = strchr(cur + 1, '/'))) {
    result->depth++;
    slash1 = slash2;
    slash2 = cur;
  }
  result->parent_id = dir_id(path, slash2 - path);
  result->grandparent_id = dir_id(path, slash1 - path);
  return result;
}

HIST_NODISC inline static void* maybe_fetch(hist_id_t key) {
  return tdb_fetch(tdb_ctx, HIST_TDB_KEY(key)).dptr;
}

HIST_NOINLINE static void store_bucket(uint32_t index, hist_bucket_t * HIST_NONNULL bucket, bool is_create) {
  assert(index < HIST_NUM_BUCKETS);
  hist_id_t id = HIST_BUCKET_ID(index);
  HIST_CHECK(!tdb_store(tdb_ctx, HIST_TDB_KEY(id), HIST_TDB_VAL(bucket, HIST_BUCKET_BYTES(index)), is_create ? TDB_INSERT : TDB_MODIFY),
             "Storing bucket %d failed.", id);
}

HIST_NODISC_PTR HIST_NOINLINE static hist_dir_t * fetch_dir(hist_id_t id) {
  hist_dir_t * result = maybe_fetch(id);
  HIST_CHECK(result && HIST_DIR_ID_P(id), "Dir does not exist (id=%08x)", id);
  return result;
}

HIST_NOINLINE static void store_dir(hist_id_t hash, hist_dir_t * HIST_NONNULL dir, bool enforce_update) {
  HIST_CHECK(!tdb_store(tdb_ctx, HIST_TDB_KEY(hash), HIST_TDB_VAL(dir, sizeof(hist_dir_t) + strlen(dir->path) + 1), enforce_update ? TDB_MODIFY : 0),
             "Error when storing directory");
}

HIST_NODISC_PTR static hist_bucket_entry_t * find_insert_pos(hist_bucket_entry_t * HIST_NONNULL begin, hist_bucket_entry_t * HIST_NONNULL end, hist_id_t bucket_index, float real_frecency) {
  assert(begin <= end);
  while (begin != end && real_frecency < compute_real_frecency(begin->frecency, begin->last_timestamp, bucket_index)) {
    ++begin;
  }
  return begin;
}

HIST_NODISC_PTR HIST_NOINLINE static hist_bucket_entry_t * find_bucket_entry_by_id(hist_bucket_entry_t * HIST_NONNULL begin, hist_bucket_entry_t * HIST_NONNULL end, hist_id_t id) {
  assert(begin <= end && HIST_DIR_ID_P(id));
  for (; begin->dir_id != id; ++begin) {
    assert(begin < end);
  }
  return begin;
}

static void remove_bucket_entry(hist_id_t bucket_key, hist_id_t id) {
  hist_bucket_t *bucket = hist_buckets[bucket_key];
  bucket->size--;
  hist_bucket_entry_t *entry = find_bucket_entry_by_id(bucket->entries, bucket->entries + bucket->size, id);
  uint32_t len = bucket->entries + bucket->size - entry;
  HIST_CHECK(len <= bucket->size, "Entry not in bucket.");
  memmove(entry, entry + 1, len * sizeof(hist_bucket_entry_t));
}

static void insert_bucket_entry(hist_bucket_entry_t * HIST_NONNULL pos, hist_bucket_entry_t * HIST_NONNULL end, hist_id_t bucket_index, hist_id_t dir_id, hist_dir_t *dir) {
  assert(bucket_index < HIST_NUM_BUCKETS && HIST_DIR_ID_P(dir_id) && HIST_DIR_IN_BUCKET_P(dir, bucket_index));
  if (end != pos) {
    assert(pos < end);
    memmove(pos + 1, pos, (end - pos) * sizeof(hist_bucket_entry_t));
  }
  *pos = (hist_bucket_entry_t){
    dir_id,
    dir->last_timestamp,
    dir->frecencies[bucket_index],
  };
}

static void maybe_bucketize_dir(hist_id_t dir_id, hist_dir_t * HIST_NONNULL dir) {
  FOR_BUCKETS(bucket_index) {
    hist_bucket_t *bucket = hist_buckets[bucket_index];
    uint32_t sz = bucket->size;
    hist_bucket_entry_t *end = bucket->entries + sz;
    bool is_in_bucket = HIST_DIR_IN_BUCKET_P(dir, bucket_index);
    if (!is_in_bucket) {
      if (HIST_LIKELY(sz == HIST_BUCKET_CAPACITY(bucket_index))) {
        --end;
        float bucket_frecency = compute_real_frecency(end->frecency, end->last_timestamp, bucket_index);
        if (dir->frecencies[bucket_index] < bucket_frecency) {
          continue;
        }
        assert(bucket_frecency >= 0);
        hist_dir_t *dropped_dir = fetch_dir(end->dir_id);
        assert(dropped_dir->frecencies[bucket_index] == end->frecency && dropped_dir->last_timestamp == end->last_timestamp);
        assert(HIST_DIR_IN_BUCKET_P(dropped_dir, bucket_index));
        dropped_dir->buckets -= 1 << bucket_index;
        store_dir(end->dir_id, dropped_dir, true);
      } else {
        ++sz;
      }
      HIST_DIR_SET_BUCKET(dir, bucket_index);
    }
    hist_bucket_entry_t *insert_begin = find_insert_pos(bucket->entries, end, bucket_index, dir->frecencies[bucket_index]);

    if (is_in_bucket) {
      end = find_bucket_entry_by_id(insert_begin, end, dir_id);
    }
    insert_bucket_entry(insert_begin, end, bucket_index, dir_id, dir);
    assert(sz > 0 && sz <= HIST_BUCKET_CAPACITY(bucket_index));
    assert(sz == bucket->size || sz == bucket->size + 1);
    bucket->size = sz;

    store_bucket(bucket_index, bucket, false);
  }
}

#define HIST_CHECK_JMP(COND, ERR) if (HIST_UNLIKELY(!(COND))) { err = ERR; goto fail; }

static void bump_path(char * HIST_NONNULL path) {
  HIST_CHECK(*path == '/', "Expected an absolute path.");
  uint32_t path_len = strlen(path);
  while (path_len > 1 && path[path_len - 1] == '/') {
    --path_len;
  }
  path[path_len] = '\0';
  hist_id_t id = dir_id(path, path_len);
  hist_dir_t *dir = maybe_fetch(id);
  if (!dir) {
    dir = create_dir(path, path_len);
  } else {
    for (int32_t i = 0; i < HIST_NUM_BUCKETS; ++i) {
      dir->frecencies[i] = compute_real_frecency(dir->frecencies[i], dir->last_timestamp, i) + 1.0f;
    }
  }
  dir->last_timestamp = cur_timestamp;
  maybe_bucketize_dir(id, dir);
  store_dir(id, dir, false);
}

typedef void (*hist_cmd_fn_t)(char *);

typedef struct hist_cmd {
  const char name[16];
  int32_t args;
  const hist_tx_type_t *tx;
  hist_cmd_fn_t fn;
} hist_cmd_t;

static void open_db(const hist_tx_type_t * tx) {
  const char *homedir = getenv("HOME");
  uint32_t len = homedir ? strlen(homedir) : 0;
  HIST_CHECK(len && len < PATH_MAX - 26, "$HOME is not set");
  char * hist_db_path = (char*)__builtin_alloca_uninitialized(len + 26);
  memcpy(hist_db_path, homedir, len);
  memcpy(hist_db_path + len, "/.local/share/hist/db.tdb", 26);
  tdb_ctx = tdb_open_ex(hist_db_path, 0, TDB_NOSYNC, tx ? O_CREAT | O_RDWR : O_RDONLY, S_IWUSR | S_IRUSR,
                        NULL, &hist_tdb_hash);
  HIST_CHECK(tdb_ctx, "Opening db failed");

  if (tx) {
    HIST_CHECK(!tx->begin.fn(tdb_ctx), tx->begin.err);
    cur_tx = tx;
  }
  FOR_BUCKETS(i) {
    hist_buckets[i] = maybe_fetch(HIST_BUCKET_ID(i));
    if (HIST_UNLIKELY(!hist_buckets[i])) {
      hist_buckets[i] = calloc(1, HIST_BUCKET_BYTES(i));
      store_bucket(i, hist_buckets[i], true);
    }
  }
  cur_timestamp = (uint32_t)time(NULL);
}


HIST_NOINLINE static void dump_dir_title(int indent, hist_dir_t * dir) {
  dprintf(STDERR_FILENO, ("    %s (depth %d)\n") + 4 - indent, dir ? dir->path : "NULL", dir ? dir->depth : 0);
}

HIST_NOINLINE static void dump_frecency(const char *HIST_NONNULL prefix, float frecency, uint32_t last_timestamp, uint32_t bucket_index) {
  dprintf(STDERR_FILENO, "%s%6f(%6f), At %u (%d ago)\n", prefix, frecency,
          compute_real_frecency(frecency, last_timestamp, bucket_index),
          last_timestamp, cur_timestamp - last_timestamp);
}

static int dump_dirs_cb(struct tdb_context *, TDB_DATA key, TDB_DATA value, void *) {
  if (!HIST_DIR_ID_P(*(hist_id_t*)key.dptr)) {
    return 0;
  }
  hist_dir_t *dir = (hist_dir_t*)value.dptr;
  dump_dir_title(0, dir);
  dprintf(STDERR_FILENO, "  Parent: %08x Grandparent: %08x\n  Frecencies:\n", dir->parent_id, dir->grandparent_id);
  for (int32_t i = 0; i < HIST_NUM_BUCKETS; ++i) {
    dprintf(STDERR_FILENO, "    #%d", i);
    dump_frecency(HIST_DIR_IN_BUCKET_P(dir, i) ? " [B]: " : ":     ", dir->frecencies[i], dir->last_timestamp, i);
  }
  return 0;
}

static void dump_buckets() {
  for (hist_id_t i = 0; i < HIST_NUM_BUCKETS; ++i) {
    dprintf(STDERR_FILENO, "Bucket #%d: (%d entries of max %d)\n", i, hist_buckets[i]->size, HIST_BUCKET_CAPACITY(i));
    for (hist_bucket_entry_t *entry = hist_buckets[i]->entries, *end = entry + hist_buckets[i]->size; entry != end; ++entry) {
      hist_dir_t * dir = maybe_fetch(entry->dir_id);
      dump_dir_title(4, dir);
      dump_frecency("      Entry: ", entry->frecency, entry->last_timestamp, i);
      if (dir) {
        dump_frecency("      Dir:   ", dir->frecencies[i], dir->last_timestamp, i);
      } 
    }
  }
}

HIST_INLINE uint32_t hist_top_print(hist_id_t id, hist_id_t bucket) {
  hist_dir_t *dir = fetch_dir(id);
  // Check that it is not already displayed
  int32_t should_show = !(dir->buckets & ((1 << bucket) - 1));
  if (should_show) {
    dprintf(STDOUT_FILENO, "%s\n", dir->path);
  }
  free(dir);
  return should_show;
}

static void hist_top(char *arg) {
  int32_t num = atoi(arg);
  HIST_CHECK(num > 0, "Expected a positive argument for top.");
  for (hist_id_t i = 0; i < HIST_NUM_BUCKETS; ++i) {
    for (hist_bucket_entry_t *it = hist_buckets[i]->entries, *end = it + hist_buckets[i]->size;
         it != end; ++it) {
      num -= hist_top_print(it->dir_id, i);
      if (!num) {
        return;
      }
    }
  }
}

static hist_ft_t hist_get_filetype(const char * HIST_NONNULL path) {
  struct stat path_stat;
  if (stat(path, &path_stat)) {
    return hist_ft_missing;
  }
  switch (path_stat.st_mode & S_IFMT) {
  case S_IFDIR: return hist_ft_dir;
  case S_IFREG: return hist_ft_file;
  default: return hist_ft_other;
  }
}

static void hist_bucket_mop(char*) {
  FOR_BUCKETS(bucket_key) {
    FOR_BUCKET_ENTRIES(it, bucket_key) {
      hist_id_t dir_id = it->dir_id;
      hist_dir_t *dir = fetch_dir(dir_id);
      if (dir->last_timestamp != cur_timestamp) {
        bool is_dir = hist_get_filetype(dir->path) == hist_ft_dir;
        FOR_BUCKETS(j) {
          if (!is_dir) {
            if (HIST_DIR_IN_BUCKET_P(dir, j)) {
              remove_bucket_entry(j, dir_id);
            }
          } else {
            dir->frecencies[j] = compute_real_frecency(dir->frecencies[j], dir->last_timestamp, j);
          }
        }
        if (!is_dir) {
          HIST_CHECK(!tdb_delete(tdb_ctx, HIST_TDB_KEY(dir_id)), "Deleting dir failed");
          FOR_BUCKET_ENTRIES_CONTINUE_AFTER_DEL(it);
        } else {
          dir->last_timestamp = cur_timestamp;
          store_dir(it->dir_id, dir, true);
        }
      }
      it->frecency = dir->frecencies[bucket_key];
      it->last_timestamp = cur_timestamp;
      free(dir);
    }
    store_bucket(bucket_key, hist_buckets[bucket_key], false);
  }
}

#define HIST_VALIDATE(COND, MSG, ...) if (HIST_UNLIKELY(!(COND))) { hist_validation_error(bucket_key, entry, MSG "\n" __VA_OPT__(,) __VA_ARGS__); }
#define HIST_VALIDATE_SKIP(COND, MSG, ...) if (HIST_UNLIKELY(!(COND))) { hist_validation_error(bucket_key, entry, MSG "\n" __VA_OPT__(,) __VA_ARGS__); continue; }

HIST_NOINLINE static void hist_validation_error(hist_id_t bucket, hist_bucket_entry_t* entry, const char * HIST_NONNULL msg, ...) {
  const char *name = "";
  if (entry) {
    bool is_valid = HIST_DIR_ID_P(entry->dir_id);
    hist_dir_t *dir = is_valid ? maybe_fetch(entry->dir_id) : NULL;
    name = dir ? dir->path : is_valid ? "[MISSING DIR]" : "[INVALID ID]";
  } 
  dprintf(STDERR_FILENO, "At bucket %d, dir %08x%s: ", bucket, entry ? entry->dir_id : 0, name);
  va_list args;
  va_start(args, msg);
  vdprintf(STDERR_FILENO, msg, args);
  va_end(args);
}

static void hist_validate(char*) {
  FOR_BUCKETS(bucket_key) {
    hist_bucket_entry_t *entry = NULL;
    hist_bucket_t *bucket = hist_buckets[bucket_key];
    HIST_VALIDATE_SKIP(bucket, "Bucket is null");
    HIST_VALIDATE_SKIP(bucket->size <= HIST_BUCKET_CAPACITY(bucket_key), "Bucket size is invalid: %d", bucket->size);

    float prev_frecency = 0.f;
    FOR_BUCKET_ENTRIES(entry, bucket_key) {
      float real_frecency = compute_real_frecency(entry->frecency, entry->last_timestamp, bucket_key);
      HIST_VALIDATE(!prev_frecency || prev_frecency >= real_frecency, "Entries should be in descending order, but Prev(%f) < Cur(%f)", prev_frecency, real_frecency);
      prev_frecency = real_frecency;
      HIST_VALIDATE(entry->frecency >= 0, "Frecency is negative: %f", entry->frecency);
      FOR_BUCKET_ENTRIES(entry2, bucket_key) {
        HIST_VALIDATE(entry2 == entry || entry->dir_id != entry2->dir_id, "Duplicate entry in bucket");
      }
      hist_dir_t *dir = maybe_fetch(entry->dir_id);
      HIST_VALIDATE_SKIP(dir, HIST_DIR_ID_P(entry->dir_id) ? "Dir id invalid" : "Dir not found");
      HIST_VALIDATE(HIST_DIR_IN_BUCKET_P(dir, bucket_key), "Bucket not marked in dir");
      HIST_VALIDATE(dir->frecencies[bucket_key] == entry->frecency, "Dir and entry had differing frecencies: %f != %f", 
                    dir->frecencies[bucket_key], entry->frecency);
      HIST_VALIDATE(dir->last_timestamp == entry->last_timestamp, "Dir and entry had differing timestamps: %u != %u", 
                    dir->last_timestamp, entry->last_timestamp);
      uint32_t path_len = strlen(dir->path);
      
      HIST_VALIDATE((path_len == 1) == (entry->dir_id == HIST_DIR_ID_ROOT), "Id should be 4 exactly for the root dir");
      HIST_VALIDATE((path_len == 1) == !dir->depth, "Exactly the root directory should be at depth 0, path_len=%u, depth=%d", path_len, dir->depth);
      HIST_VALIDATE((dir->depth <= 1) == (dir->parent_id == HIST_DIR_ID_ROOT), "Root and first-level dirs should have root as parent, parent %u depth %u", dir->parent_id, dir->depth);
      HIST_VALIDATE((dir->depth <= 2) == (dir->grandparent_id == HIST_DIR_ID_ROOT), "Root, first-, and second-level dirs should have root as grandparent, grandparent %u depth %u", dir->grandparent_id, dir->depth);
      HIST_VALIDATE(*dir->path == '/' && (path_len == 1 || dir->path[path_len - 1] != '/'),
                    "Path \"%s\" should start with a '/' and not end with one.", dir->path);
      hist_id_t hashed = dir_id(dir->path, path_len);
      HIST_VALIDATE(hashed == entry->dir_id, "Path \"%s\" hashed to %u, not id", dir->path, hashed);
      free(dir);
    }
  }
}

static void hist_dump(char * arg) {
  if (!strcmp(arg, "dirs")) {
    HIST_CHECK(tdb_traverse(tdb_ctx, &dump_dirs_cb, NULL) >= 0, "Traversing failed");
  } else {
    HIST_CHECK(!strcmp(arg, "buckets"), "Expected \"dirs\" or \"buckets\" for \"dump\"");
    dump_buckets();
  }
}

static const hist_cmd_t hist_commands[] = {
  { "put", 1, &tx_write, &bump_path },
  { "top", 1, &tx_read, &hist_top },
  { "mop", 0, &tx_write, &hist_bucket_mop },
  { "validate", 0, &tx_read, &hist_validate },
  { "dump", 1, NULL, &hist_dump },
  { "", 0, NULL, NULL },
};


int main(int argc, char **argv) {
  for (const hist_cmd_t *cmd = hist_commands; *cmd->name; ++cmd) {
    if (cmd->args + 2 == argc && !strcmp(cmd->name, argv[1])) {
      open_db(cmd->tx);
      cmd->fn(argv[2]);
      if (cmd->tx) {
        cur_tx = NULL;
        HIST_CHECK(!cmd->tx->commit.fn(tdb_ctx), cmd->tx->commit.err);
      }
      tdb_close(tdb_ctx);
      return 0;
    }
  }
  HIST_CHECK(false,
          "Usage: hist put <dir-path>\n"
          "            top <num>\n"
          "            dump (dirs|buckets)\n"
          "            validate\n"
          "            mop\n");
}

