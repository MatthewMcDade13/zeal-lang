
core_includes = include_directories('inc')
core_sources = ['core.c']

corelib = library('zeal-core',
  core_sources,
  include_directories : core_includes,
  install : true
)

core_dep = declare_dependency(link_with: corelib)
