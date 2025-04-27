project('zeal', 'c',
  version : '0.1',
  default_options : ['warning_level=3'])


subdir('ast')
subdir('core')


exe = executable('zeal', 'main.c',
  install : true,
  dependencies : [ast_dep, core_dep]
)

test('basic', exe)
