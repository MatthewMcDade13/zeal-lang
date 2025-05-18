core_includes = include_directories('incl')

core_sources = files('lib.cpp', 'common.cpp', 'stack.cpp', 'value.cpp')


libzcore = library('zcore',
  core_sources,
  include_directories: [core_includes, zlibs_includes],
  install: true
)

depzcore = declare_dependency(
  include_directories: core_includes,
  link_with: [libzcore, libzplog]
)

