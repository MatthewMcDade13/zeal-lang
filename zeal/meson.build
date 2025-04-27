project('zeal', 'c',
  version : '0.1',
  default_options : ['c_std=c17', 'warning_level=3'])


subdir('ast')
subdir('core')

root_includes = include_directories('.')
root_sources = ['main.c']

exe = executable('zeal', 
  root_sources,
  include_directories : root_includes,
  install : true,
  dependencies : [ast_dep, core_dep]
)

test('basic', exe)
