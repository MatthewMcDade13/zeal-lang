project(
  'zeal-lang',
  'cpp',
  'c',
  version: '0.1',
  default_options: [
    'cpp_std=c++17,c++20,c++23',
    'c_std=c11,c17,c18,c23',
    # 'cpp_eh=none',
    # 'cpp_rtti=false',
    'werror=true',
  ],
)

project_args = []

buildtype = get_option('buildtype')

if buildtype == 'debug' or buildtype == 'debugoptimized'
  project_args += ['-DZEAL_DEBUG=1']
else
  project_args += ['-DZEAL_DEBUG=0']
endif

main_sources = files('main.cpp')

subdir('libs')

subdir('zalloc')
subdir('zcore')
subdir('zbytecode')
subdir('zvm')
subdir('zast')

exe = executable(
  'zeal',
  main_sources,
  cpp_args: project_args,
  c_args: project_args,
  cpp_pch: 'pch/zeal_pch.h',
  dependencies: [depzcore, depzvm, depzbytecode, depzast, zplog_dep, zbench_dep],
  link_with: [libzcore, libzvm, libzbytecode, libzast, libzplog, libzbench],
  install: true,
)

# TODO: create a library config so zeal can be easily embedded

dep_includes = include_directories(
  'zalloc/incl',
  'zcore/incl',
  'zbytecode/incl',
  'zvm/incl',
  'zast/incl',
  'libs/incl',
)

dep = declare_dependency(
  include_directories: dep_includes,
  link_with: [libzcore, libzvm, libzbytecode, libzast, libzplog, libzbench],
)

zbench_sources = files('zbench.cpp')

bench_exe = executable(
  'zbench',
  zbench_sources,
  dependencies: [zbench_dep, dep],
  link_with: [libzbench],
  install: true,
)

test_sources = files('tests/all.cpp')
alltests_exe = executable(
  'ztests',
  test_sources,
  include_directories: zlibs_includes,
  dependencies: dep,
  install: true,
)

test('zeal_tests', alltests_exe)
