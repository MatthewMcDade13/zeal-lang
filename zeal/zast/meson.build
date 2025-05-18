zast_includes = include_directories('incl')

zast_sources = files('ast.cpp', 'lex.cpp')


libzast = library('zast',
  zast_sources,
  include_directories: [zast_includes, zlibs_includes],
  dependencies: [depzcore, zplog_dep],
  install: true
)

depzast = declare_dependency(
  include_directories: [zast_includes, zlibs_includes],
  link_with: [libzast, libzplog]
)
