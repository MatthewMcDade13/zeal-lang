project('zeal-lang', 'cpp',
  version : '0.1',
  default_options : [
    'cpp_std=c++23',
    'werror=true'
  ])

# cpp = meson.get_compiler('cpp')
# project_args = []

# if cpp.get_id() == 'gcc' or cpp.get_id() == 'clang'
#   project_args += ['-Wextra']
# endif

# add_project_arguments(project_args, language: 'cpp')

main_sources = files('main.cpp')

# test('basic', exe)


subdir('libs')



subdir('zcore')
subdir('zbytecode')
subdir('zvm')
subdir('zast')



exe = executable('zeal',
  main_sources,
  cpp_pch: 'pch/zeal_pch.h',
  dependencies: [depzcore, depzvm, depzbytecode, depzast, zplog_dep, zbench_dep],
  link_with: [libzcore, libzvm, libzbytecode, libzast, libzplog, libzbench],
  install : true
)

dep_includes = include_directories('zcore/incl', 'zbytecode/incl', 'zvm/incl', 'zast/incl', 'libs/incl')

dep = declare_dependency(
  include_directories: dep_includes, 
  link_with: [libzcore, libzvm, libzbytecode, libzast, libzplog, libzbench]
)

zbench_sources = files('zbench.cpp')

bench_exe = executable('zbench',
  zbench_sources,
  dependencies: [zbench_dep, dep],
  link_with: [libzbench],
  install: true
)

test_sources = files('tests/all.cpp')
alltests_exe = executable('ztests',
  test_sources,
  include_directories: zlibs_includes,
  dependencies: dep,
  install: true
)

test('zeal_tests', alltests_exe)

