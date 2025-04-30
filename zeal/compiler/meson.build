

compiler_incs = include_directories(['inc', '../'])
compiler_sources = ['compiler.cpp']

llvm_dep = dependency('llvm', required : true)

libcompiler = library('zeal-compiler',
  compiler_sources,
  dependencies : llvm_dep,
  include_directories : compiler_incs,
  install : true
)

compiler_dep = declare_dependency(link_with : libcompiler)

