

repl_incs = include_directories('inc')
repl_sources = ['repl.cpp']

llvm_dep = dependency('llvm', required : true)

librepl = library('zeal-repl',
  repl_sources,
  dependencies : llvm_dep,
  include_directories : repl_incs,
  install : true
)

repl_dep = declare_dependency(link_with : librepl)

