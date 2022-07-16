#define _GNU_SOURCE 1
#define _POSIX_C_SOURCE 200809L

#include <stdarg.h>
#include <limits.h>
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <stdint.h>
#include <time.h>
#include <math.h>
#include <tdb.h>
#include <xxhash.h>

#define HIST_CHECK(COND, ...) if (!(COND)) { fail(__PRETTY_FUNCTION__, __LINE__  __VA_OPT__(,) __VA_ARGS__); }
#define HIST_NUM_BUCKETS 4
#define HIST_DECAY_THRESHOLD 120.0f
#define HIST_DB_PATH "/.local/share/hist/db.tdb"

/**
 * Offset to substract from timestamps to make them work as 32-bit floats without precision loss.
 *
 * Since the offset defines a new epoch, it should be some transformative moment in the history
 * of computing, perhaps even more important than creation of UNIX. This timestamp points to
 * Oct 25, 2021, 18:55:05 UTC. This is when Ethereum block 13488323 was mined, ending an NFT
 * auction where someone bought a rather ugly clip art of a blue rock for Ether then worth almost
 * $4M. This is the rock in question: https://etherrock.com/55.png
 * 
 * Of course, a virtual rock is almost as good as a real one, and $4M is a bargain compared to
 * the $12M paid a bit earlier that year for a 100 carat diamond. Perhaps someone got bored of
 * his virtual rocks and traded them for a real one.
 */
#define HIST_TIMESTAMP_BASE 1635188105U

#define HIST_EXPONENT(HALF_LIFE_MINUTES) -0.01666666f / HALF_LIFE_MINUTES

static const float frecency_decay_exponents[HIST_NUM_BUCKETS] = { HIST_EXPONENT(120.0f), HIST_EXPONENT(1440.0f), HIST_EXPONENT(7200.0f), HIST_EXPONENT(30240.f) };
static const float frecency_increments[HIST_NUM_BUCKETS] = { 8.0f, 4.0f, 2.0f, 1.0f, };

static const TDB_DATA hist_state_key = { (unsigned char*)"s", 1 };

static const int32_t hist_bucket_sizes[HIST_NUM_BUCKETS] = { 16, 24, 64, 128 };

static const TDB_DATA hist_bucket_keys[HIST_NUM_BUCKETS] = {
  { (unsigned char*)"b0", 2 },
  { (unsigned char*)"b1", 2 },
  { (unsigned char*)"b2", 2 },
  { (unsigned char*)"b3", 2 },
};
static TDB_DATA hist_buckets[HIST_NUM_BUCKETS] = {{NULL, 0}, {NULL, 0}, {NULL, 0}, {NULL, 0}};

static float cur_timestamp;

static struct tdb_context* tdb_ctx = NULL;

static char hist_db_path[PATH_MAX];

typedef struct hist_dir {
  float frecency[4];
  float last_timestamp;
  int32_t cur_bucket;
  uint32_t depth;
  uint64_t parent_hash;
  uint64_t grandparent_hash;
  char path[0];
} hist_dir_t;

typedef struct hist_bucket_state {
  int32_t num_paths;
  float min_frecency;
  float min_last_timestamp;
} hist_bucket_state_t;

typedef struct hist_state {
  hist_bucket_state_t bucket_state[HIST_NUM_BUCKETS];
} hist_state_t;

typedef struct hist_path {
  uint64_t path_hash;
  char * path;
  int32_t path_len;
} hist_path_t;

static hist_state_t *hist_state = NULL;

static void fail(const char * fn, int32_t line, char *err, ...) {
  va_list args;
  va_start(args, err);
  fprintf(stderr, "Error at %s:%d:\n  ", fn, line);
  vfprintf(stderr, err, args);
  if (tdb_ctx && tdb_error(tdb_ctx)) {
    fprintf(stderr, "\nTDB error: %s\n", tdb_errorstr(tdb_ctx));
  } else {
    fprintf(stderr,
            "\nUsage: hist put <dir-path>\n"
            "            top <num>\n"
            "            dump-dirs\n"
            "            dump-buckets\n"
      );
  }
  if (tdb_ctx) {
    tdb_close(tdb_ctx);
  }
  va_end(args);
#ifndef NDEBUG
  __asm__ volatile("int $0x03");
#endif
  exit(1);
}

static void get_path(char *path, hist_path_t *target) {
  HIST_CHECK(*path == '/', "Expected an absolute path.");
  target->path = path + 1;
  target->path_len = strlen(target->path);
  while (target->path_len && target->path[target->path_len - 1] == '/') {
    target->path[--target->path_len] = '\0';
  }
  target->path_hash = XXH3_64bits(target->path, target->path_len);
}

static inline float compute_real_frecency(float frecency, float old_ts, int32_t bucket_index) {
  assert(cur_timestamp >= old_ts);
  assert(frecency >= 0);
  assert(bucket_index >= 0 && bucket_index < HIST_NUM_BUCKETS);
  float exp = exp2f((cur_timestamp - old_ts) * frecency_decay_exponents[bucket_index]);
  assert(exp <= 1.0f);
  return frecency * exp;
}

static hist_dir_t * create_dir(hist_path_t *path) {
  hist_dir_t *result = (hist_dir_t*)malloc(sizeof(hist_dir_t) + path->path_len + 1);
  for (int32_t i = 0; i < HIST_NUM_BUCKETS; ++i) {
    result->frecency[i] = frecency_increments[i];
  }
  result->last_timestamp = cur_timestamp;
  result->cur_bucket = -1;
  result->depth = path->path_len != 0;
  result->parent_hash = 0;
  result->grandparent_hash = 0;
  printf("Creating with path %s\n", path->path);
  memcpy(result->path, path->path, path->path_len + 1);
  char * separator = strrchr(path->path, '/');
  if (!separator) {
    goto out;
  }
  *separator = '\0';
  result->parent_hash = XXH3_64bits(path->path, separator - path->path);
  ++result->depth;

  separator = strrchr(path->path, '/');
  if (!separator) {
    goto out;
  }
  *separator = '\0';
  result->grandparent_hash = XXH3_64bits(path->path, separator - path->path);
  ++result->depth;

  while ((separator = strchr(path->path, '/'))) {
    *separator = 0;
    path->path = separator + 1;
    ++result->depth;
  }

out:
  return result;
}

static uint64_t* get_bucket(int32_t index) {
  assert(index >= 0 && index < HIST_NUM_BUCKETS);
  if (!hist_buckets[index].dptr) {
    hist_buckets[index] = tdb_fetch(tdb_ctx, hist_bucket_keys[index]);
    HIST_CHECK(hist_buckets[index].dptr, "Corrupted database: bucket %d does not exist.", index);
    HIST_CHECK(hist_buckets[index].dsize == hist_bucket_sizes[index] * sizeof(uint64_t),
               "Corrupted database: bucket %d has invalid size %d (expected %d).",
               index, hist_buckets[index].dsize, hist_bucket_sizes[index] * sizeof(uint64_t));
  }
  return (uint64_t*)hist_buckets[index].dptr;
}

static hist_dir_t * fetch_dir(uint64_t hash, bool required) {
  TDB_DATA dir_data = tdb_fetch(tdb_ctx, (TDB_DATA){(unsigned char*)&hash, sizeof(uint64_t)});
  HIST_CHECK(!required || dir_data.dptr, "Corrupted database: no path by index %08lx", hash);
  HIST_CHECK(!dir_data.dptr || dir_data.dsize > sizeof(hist_dir_t), "Corrupted database: no path by index %08lx", hash);
  return (hist_dir_t*)dir_data.dptr;
}

static void store_buckets() {
  for (int32_t i = 0; i < HIST_NUM_BUCKETS; ++i) {
    if (hist_buckets[i].dptr) {
      assert(hist_buckets[i].dsize == sizeof(uint64_t) * hist_bucket_sizes[i]);
      HIST_CHECK(!tdb_store(tdb_ctx, hist_bucket_keys[i], hist_buckets[i], 0),
                 "Storing bucket %d failed.", i);
    }
  }
}

static void store_dir(uint64_t hash, hist_dir_t * dir) {
  assert(dir);
  HIST_CHECK(!tdb_store(tdb_ctx,
                        (TDB_DATA){(unsigned char*)&hash, sizeof(uint64_t)},
                        (TDB_DATA){(unsigned char*)dir, sizeof(hist_dir_t) + strlen(dir->path) + 1},
                        0),
             "Error when storing directory");
}

static void remove_dir_from_bucket(int32_t bucket_index, uint64_t hash) {
  uint64_t *bucket = get_bucket(bucket_index);
  hist_bucket_state_t *bucket_state = hist_state->bucket_state + bucket_index;
  int32_t cur_idx = --(bucket_state->num_paths);
  assert(cur_idx >= 0);
  uint64_t prev = bucket[cur_idx];
  while (prev != hash) {
    assert(cur_idx >= 0);
    uint64_t cur = bucket[--cur_idx];
    assert(cur != prev);
    bucket[cur_idx] = prev;
    prev = cur;
  }

  // If last element is removed, the lowest value has to be updated.
  if (cur_idx == hist_state->bucket_state[bucket_index].num_paths) {
    if (!cur_idx) {
      bucket_state->min_frecency = 0.0f;
      bucket_state->min_last_timestamp = 0.0f;
    } else {
      --cur_idx;
      hist_dir_t *last = fetch_dir(bucket[cur_idx], true);
      assert(last->cur_bucket == bucket_index);
      bucket_state->min_frecency = last->frecency[bucket_index];
      bucket_state->min_last_timestamp = last->last_timestamp;
    }
  }
}

static void insert_dir_to_bucket(uint64_t hash, hist_dir_t * dir) {
  assert(hist_state);
  float real_frecency;
  int32_t bucket_idx = 0;
  hist_bucket_state_t *bs = hist_state->bucket_state;
  for (; bucket_idx < HIST_NUM_BUCKETS; ++bucket_idx, ++bs) {
    real_frecency = compute_real_frecency(dir->frecency[bucket_idx], dir->last_timestamp, bucket_idx);
    assert(bs->num_paths >= 0 && bs->num_paths <= hist_bucket_sizes[bucket_idx]);
    float bucket_frecency = compute_real_frecency(bs->min_frecency, bs->min_last_timestamp, bucket_idx);
    if (bucket_frecency < real_frecency || (bucket_frecency == real_frecency && dir->cur_bucket == bucket_idx)
        || bs->num_paths < hist_bucket_sizes[bucket_idx]) {
      break;
    }
  }

  if (dir->cur_bucket >= 0) {
    remove_dir_from_bucket(dir->cur_bucket, hash);
  }
  dir->cur_bucket = bucket_idx < HIST_NUM_BUCKETS ? bucket_idx : -1;
  store_dir(hash, dir);
  if (bucket_idx == HIST_NUM_BUCKETS) {
    return;
  }

  uint64_t* bucket = get_bucket(bucket_idx);
  int32_t idx = 0, max = bs->num_paths; 
  for (; idx != max; ++idx) {
    hist_dir_t *last = fetch_dir(bucket[idx], true);
    if (compute_real_frecency(last->frecency[bucket_idx], last->last_timestamp, bucket_idx) < real_frecency) {
      break;
    }
  }


  uint64_t dropped_hash = 0U;
  hist_dir_t * dropped = NULL;
  if (idx == max) {
    HIST_CHECK(max < hist_bucket_sizes[bucket_idx], "No space in bucket %d after all", bucket_idx);
    bs->num_paths++;
  } else {
    dropped_hash = bucket[max - 1];
    dropped = fetch_dir(dropped_hash, true);
    HIST_CHECK(dropped->cur_bucket == bucket_idx, "Bucket mismatch, stored as %d but was in bucket %d.", dropped->cur_bucket, bucket_idx);
    dropped->cur_bucket = -1;
    memmove(bucket + idx + 1, bucket + idx, sizeof(uint64_t) * (max - idx - 1));
  }

  bucket[idx] = hash;
  hist_dir_t * new_last = fetch_dir(bucket[bs->num_paths - 1], true);
  assert(new_last->cur_bucket == bucket_idx);
  bs->min_frecency = new_last->frecency[bucket_idx];
  bs->min_last_timestamp = new_last->last_timestamp;

  if (dropped) {
    insert_dir_to_bucket(dropped_hash, dropped);
  }
}

static void decay_dir(hist_dir_t *dir) {
  if (cur_timestamp - dir->last_timestamp <= HIST_DECAY_THRESHOLD) {
    return;
  }
  for (int32_t i = 0; i < HIST_NUM_BUCKETS; ++i) {
    dir->frecency[i] = compute_real_frecency(dir->frecency[i], dir->last_timestamp, i);
  }
  dir->last_timestamp = cur_timestamp;
}

static void bump_path(hist_path_t *path) {
  hist_dir_t *dir = fetch_dir(path->path_hash, false);
  if (!dir) {
    dir = create_dir(path);
  } else {
    decay_dir(dir);
    for (int32_t i = 0; i < HIST_NUM_BUCKETS; ++i) {
      dir->frecency[i] += frecency_increments[i];
    }
  }

  insert_dir_to_bucket(path->path_hash, dir);
  store_buckets();
  HIST_CHECK(!tdb_store(tdb_ctx, hist_state_key, (TDB_DATA){(unsigned char*)hist_state, sizeof(hist_state_t)}, 0), "Error when storing state");
}

enum hist_cmd {
  hist_cmd_put,
  hist_cmd_top,
  hist_cmd_dump_dirs,
  hist_cmd_dump_buckets,
};

static int32_t get_cmd(char *name) {
  if (!strcmp(name, "put")) {
    return hist_cmd_put;
  } else if (!strcmp(name, "top")) {
    return hist_cmd_top;
  } else if (!strcmp(name, "dump-buckets")) {
    return hist_cmd_dump_buckets;
  }
  HIST_CHECK(!strcmp(name, "dump-dirs"), "Expected command to be \"put\", \"top\", or \"dump-dirs\".");
  return hist_cmd_dump_dirs;
}

static void open_db(int32_t is_write) {
  char *homedir = getenv("HOME");
  HIST_CHECK(homedir, "$HOME is not set.");
  int32_t len = strlen(homedir);
  HIST_CHECK(len < PATH_MAX - (int)strlen(HIST_DB_PATH), "$HOME is too long.");
  memcpy(hist_db_path, homedir, len);
  memcpy(hist_db_path + len, HIST_DB_PATH, strlen(HIST_DB_PATH) + 1);
  printf("%s\n", hist_db_path);
  tdb_ctx = tdb_open(hist_db_path, 0, 0, O_CLOEXEC | (is_write ? O_CREAT | O_RDWR : O_RDONLY),
                     S_IWUSR | S_IRUSR);
  HIST_CHECK(tdb_ctx, "Opening database failed.");

  TDB_DATA state = tdb_fetch(tdb_ctx, hist_state_key);
  if (state.dptr) {
    assert(state.dsize == sizeof(hist_state_t));
    hist_state = (hist_state_t*)state.dptr;
  } else {
    hist_state = (hist_state_t*)calloc(sizeof(hist_state_t), 1);
    for (int32_t i = 0; i < HIST_NUM_BUCKETS; ++i) {
      hist_buckets[i] = (TDB_DATA){(unsigned char*)calloc(sizeof(uint64_t), hist_bucket_sizes[i]), sizeof(uint64_t) * hist_bucket_sizes[i] };
    }
  }
}

static int dump_dirs_cb(struct tdb_context *, TDB_DATA key, TDB_DATA value, void *) {
  assert(key.dptr && value.dptr);
  if (key.dsize != sizeof(uint64_t)) {
    return 0;
  }
  HIST_CHECK(value.dsize > sizeof(hist_dir_t), "Corrupted database, dir size mismatch.");
  hist_dir_t *dir = (hist_dir_t*)value.dptr;
  fprintf(stderr, "%08lx: /%s (depth %d)\n", *(uint64_t*)key.dptr, dir->path, dir->depth);
  fprintf(stderr, "  Parent: %08lx Grandparent: %08lx\n", dir->parent_hash, dir->grandparent_hash);
  fprintf(stderr, "  Cur bucket: %d\n", dir->cur_bucket);
  fprintf(stderr, "  Frecencies: (last timestamp %f, %f sec ago)\n", dir->last_timestamp, cur_timestamp - dir->last_timestamp);
  for (int32_t i = 0; i < HIST_NUM_BUCKETS; ++i) {
    fprintf(stderr, "    Stored: %8f Real: %f\n", dir->frecency[i], compute_real_frecency(dir->frecency[i], dir->last_timestamp, i));
  }
  return 0;
}

static void dump_buckets() {
  assert(hist_state);
  hist_bucket_state_t *state = hist_state->bucket_state;
  for (int32_t i = 0; i < HIST_NUM_BUCKETS; ++i, ++state) {
    uint64_t *bucket = get_bucket(i);
    fprintf(stderr, "Bucket #%d: (%d entries of max %d)\n", i, state->num_paths, hist_bucket_sizes[i]);
    fprintf(stderr, "  Min frecency: %f (real %f)\n", state->min_frecency, compute_real_frecency(state->min_frecency, state->min_last_timestamp, i));
    fprintf(stderr, "  Last timestamp: %f (%f sec ago)\n", state->min_last_timestamp, cur_timestamp - state->min_last_timestamp);
    if (!state->num_paths) {
      fprintf(stderr, "\n");
      continue;
    }
    fprintf(stderr, "  Entries:\n");
    for (int32_t j = 0; j < state->num_paths; ++j, ++bucket) {
      fprintf(stderr, "    %08lx: ", *bucket);
      hist_dir_t * dir = fetch_dir(*bucket, false);
      if (!dir) {
        fprintf(stderr, "MISSING\n");
        continue;
      }
      fprintf(stderr, "/%s\n", dir->path);
      fprintf(stderr, "              Frecency: %f (real %f)\n", dir->frecency[i], compute_real_frecency(dir->frecency[i], dir->last_timestamp, i));
      fprintf(stderr, "              Last timestamp %f (%f sec ago)\n", dir->last_timestamp, cur_timestamp - dir->last_timestamp);
      fprintf(stderr, "\n");
    }
  }
}

int main(int argc, char **argv) {
  cur_timestamp = (uint32_t)time(NULL) - HIST_TIMESTAMP_BASE;
  int32_t cmd = get_cmd(argv[1]);
  open_db(cmd == hist_cmd_put);

  switch (cmd) {
  case hist_cmd_put: {
    HIST_CHECK(argc == 3, "Expected a single path for \"put\".");
    hist_path_t path;
    get_path(argv[2], &path);
    bump_path(&path);
    break;
  }
  case hist_cmd_top: {
    HIST_CHECK(argc == 3, "Expected a number \"top\".");
    int32_t len = atoi(argv[2]);
    HIST_CHECK(len > 0, "Expected a positive number for \"top\".");
    break;
  }
  case hist_cmd_dump_dirs: {
    HIST_CHECK(argc == 2, "\"dump-dirs\" takes no arguments.");
    HIST_CHECK(tdb_traverse(tdb_ctx, &dump_dirs_cb, NULL) >= 0, "Traversing failed.");
    break;
  }
  case hist_cmd_dump_buckets: {
    HIST_CHECK(argc == 2, "\"dump-buckets\" takes no arguments.");
    dump_buckets();
    break;
  }
  }
  tdb_close(tdb_ctx);
  return 0;
}

