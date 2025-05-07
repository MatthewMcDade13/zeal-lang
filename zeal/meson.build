project('zeal-lang', 'cpp',
  version : '0.1',
  default_options : [
    'warning_level=3',
    'cpp_std=c++23'
  ])

cpp = meson.get_compiler('cpp')
project_args = []

if cpp.get_id() == 'gcc' or cpp.get_id() == 'clang'
  project_args += ['-Wextra']
endif

add_project_arguments(project_args, language: 'cpp')

main_sources = files('main.cpp')

# test('basic', exe)


subdir('zcore')
subdir('zbytecode')
subdir('zvm')


exe = executable('zeal',
  main_sources,
  link_with: [libzcore, libzvm, libzbytecode],
  install : true
)

