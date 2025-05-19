zlibs_includes = include_directories('incl')

# zlibs_sources = files('nanobench.cpp', 'plog.cpp')

# Get the build type
buildtype = get_option('buildtype')

# Conditionally define the library as static for release builds
if buildtype == 'release'

  message('Buidling static external depenedencies in release mode...')

  zbench_sources = files('nanobench.cpp')
  libzbench = static_library('libzbench',
    zbench_sources,
    include_directories: zlibs_includes,
    pic: true, 
    install: true
  )

  zplog_sources = files('plog.cpp')
  libzplog = static_library('libzplog',
    zplog_sources,
    include_directories: zlibs_includes,
    pic: true,
    install: true,
  )

else

  message('Letting meson decide how to build non-release mode build...')

  zbench_sources = files('nanobench.cpp')

  libzbench = library('libzbench', 
    zbench_sources,
    include_directories: zlibs_includes,
    pic: true,
    install: true,
  )

  zplog_sources = files('plog.cpp')
  libzplog = library('libzplog',
    zplog_sources,
    include_directories: zlibs_includes,
    pic: true,
    install: true
  )
endif

zbench_dep = declare_dependency(
  include_directories: zlibs_includes,
  link_with: libzbench
)

zplog_dep = declare_dependency(
  include_directories: zlibs_includes,
  link_with: libzplog
)

