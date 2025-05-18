bytecode_includes = include_directories('incl')

bytecode_sources = files('lib.cpp', 'bc.cpp')


libzbytecode = library('zbytecode',
  bytecode_sources,
  include_directories: [bytecode_includes, zlibs_includes],
  dependencies: [depzcore, zplog_dep],
  install: true
)

depzbytecode = declare_dependency(
  include_directories: bytecode_includes,
  link_with: [libzbytecode, libzplog]
)
