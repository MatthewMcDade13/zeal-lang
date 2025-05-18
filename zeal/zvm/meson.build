zvm_includes = include_directories('incl')

zvm_sources = files('zvm.cpp')


libzvm = library('zvm',
  zvm_sources,
  include_directories: [zvm_includes, zlibs_includes],
  dependencies: [depzcore, depzbytecode, zplog_dep],
  install: true
)

depzvm = declare_dependency(
  include_directories: [zvm_includes, zlibs_includes],
  link_with: [libzvm, libzplog]
)
