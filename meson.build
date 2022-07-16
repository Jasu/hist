project('hist', 'c')

cc = meson.get_compiler('c')
m_dep = cc.find_library('m', required : false)
tdb_dep = dependency('tdb')
xxhash_dep = dependency('libxxhash')

executable('hist', 'main.c', dependencies: [m_dep, tdb_dep, xxhash_dep])


