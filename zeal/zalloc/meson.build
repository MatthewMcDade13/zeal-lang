zalloc_includes = include_directories('incl')

zalloc_sources = files('arena.cpp', 'mem.cpp', 'spool.cpp')


libzalloc = library('zalloc',
  zalloc_sources,
  include_directories: [zalloc_includes, zlibs_includes],
  install: true
)

depzalloc = declare_dependency(
  include_directories: zalloc_includes,
  link_with: [libzalloc, libzplog]
)

