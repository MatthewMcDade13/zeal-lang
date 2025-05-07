zast_includes = include_directories('incl')

zast_sources = files('ast.cpp')


libzast = library('zast',
  zast_sources,
  include_directories: zast_includes,
  dependencies: [depzcore ],
  install: true
)

depzast = declare_dependency(
  include_directories: zast_includes,
  link_with: libzast
)
