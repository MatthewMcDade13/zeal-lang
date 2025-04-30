

parser_incs = include_directories('inc')
parser_sources = ['parser.cpp', 'token.cpp']

llvm_dep = dependency('llvm', required : true)

libparser = library('zeal-parse',
  parser_sources,
  dependencies : llvm_dep,
  include_directories : parser_incs,
  install : true
)

parser_dep = declare_dependency(link_with : libparser)
