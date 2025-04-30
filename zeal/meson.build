project('zeal', 'cpp',
  version : '0.1',
  default_options : ['warning_level=3',
                     'cpp_std=c++20'])

llvm_dep = dependency('llvm', required : true)

subdir('core')
subdir('parser')
subdir('repl')
subdir('compiler')


exe = executable('zeal', 'main.cpp',
  install : true,
  dependencies : [llvm_dep, core_dep, parser_dep, repl_dep, compiler_dep]
)

test('basic', exe)
