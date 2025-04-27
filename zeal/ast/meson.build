
ast_includes = include_directories('inc')
ast_sources = ['ast.c', 'token.c', 'parse.c']

astlib = library('zeal-ast',
  ast_sources,
  include_directories : ast_includes,
  install : true
)

ast_dep = declare_dependency(link_with: astlib)
