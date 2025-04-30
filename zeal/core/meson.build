
core_incs = include_directories('inc')
core_sources = ['core.cpp', 'value.cpp']


llvm_dep = dependency('llvm', required : true)

libcore = library('zeal-core',
  core_sources,
  dependencies : llvm_dep,
  include_directories : core_incs,
  install : true
)

core_dep = declare_dependency(link_with : libcore)
