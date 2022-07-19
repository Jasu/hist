project('hist', 'c')

add_project_link_arguments(
  '-fno-pie',
  '-fno-PIE',
  '-Wl,-no-pie',
  '-Wl,--no-pie',
  '-fno-plt',

  '-march=tigerlake',
  '-Wl,--discard-all',

  '-fuse-ld=lld',
  # '-Wl,-Bsymbolic',

  # '-Xlinker,--warn-undefined-symbols',
  # '-Wl,-z,notext,-z,ifunc-noplt',

  '-rtlib=compiler-rt',
  '-Wl,--pack-dyn-relocs=relr',
  # '-Wl,-z,relro,-z,now',
  '-Wl,--strip-all',
  '-Wl,--gc-sections',
  '-fno-common',


  '-unwindlib=libunwind',
  language: 'c')

add_project_arguments(
  '-fno-stack-protector',

  # '-rtlib=compiler-rt',
  # '-print-libgcc-file-name',
  # 'static-pie",

  '-fno-pic',
  '-no-pie',
  '-fno-pie',
  '-fno-PIE',
  '-fno-plt',

  '-unwindlib=libunwind',

  '-fapprox-func',
  '-fassociative-math',
  '-ffast-math',
  '-ffp-contract=fast',
  '-fno-signed-zeros',
  '-fno-trapping-math',
  '-fno-math-errno',
  '-ffp-exception-behavior=ignore',
  '-funsafe-math-optimizations',
  '-freciprocal-math',
  '-fno-honor-nans',
  '-fno-honor-infinities',
  '-ffinite-math-only',

  '-ffinite-loops',

  '-fno-threadsafe-statics',

  '-fdirect-access-external-data',
  '-fno-semantic-interposition',
  '-fno-common',
  '-march=tigerlake',
  '-msse4.2',
  language: 'c')

cc = meson.get_compiler('c')
tdb_dep = dependency('tdb')
executable('hist', 'main.c', dependencies: [tdb_dep])


