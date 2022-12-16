project('hist', 'c', default_options: ['c_std=c2x'])

add_project_link_arguments(
  '-fno-pie',
  '-fno-PIE',
  '-Wl,-no-pie',
  '-Wl,--no-pie',
  '-fno-plt',

  '-fuse-ld=lld',

  '-rtlib=compiler-rt',
  '-Wl,--pack-dyn-relocs=relr',
  '-Wl,--strip-all',
  '-Wl,--gc-sections',
  '-unwindlib=none',
  language: 'c')

add_project_arguments(

  # Linker-related, specifically symbol and section visibility.
  '-fvisibility=hidden',
  '-fno-direct-access-external-data',
  '-fno-semantic-interposition',
  '-fno-common',

  # Huge dump of flags, to work around an ongoing compiler flag change in
  # Clang/LLVM and various OS vendor patches undoing the changes.
  '-fno-pic',
  '-fno-pie',
  '-fno-PIE',
  '-fno-plt',
  '-ftls-model=local-exec',
  # Note: this should be changed if TLS is not local exec
  '-fno-threadsafe-statics',

  # Keep in sync with linker arguments.
  # Also, these are dependent on linking with LLD, at least for now
  '-fno-unwind-tables',
  '-fno-asynchronous-unwind-tables',

  '-fmerge-all-constants',

  # Unrolling made at least XXHash slower on Clang 13 and TigerLake
  '-fno-unroll-loops',

  '-fno-stack-protector',
  '-ffinite-loops',

  '-fstrict-enums',
  '-fstrict-overflow',
  '-fstrict-return',

  '-fapprox-func',
  '-fassociative-math',
  '-ffast-math',
  '-ffp-contract=fast',
  '-fno-signed-zeros',
  '-fno-math-errno',
  '-ffp-exception-behavior=ignore',
  '-funsafe-math-optimizations',
  '-freciprocal-math',
  '-ffinite-math-only',
  '-fno-honor-nans',
  '-fno-honor-infinities',

  language: 'c')

cc = meson.get_compiler('c')
tdb_dep = dependency('tdb')
executable('hist', 'main.c', dependencies: [tdb_dep])


