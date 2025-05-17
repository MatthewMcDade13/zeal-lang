llvmc_includes = include_directories('incl')

llvmc_sources = files('llvmc.cpp')


llvm_modules = ['core', 'support', 'analysis', 'transformutils', 'scalaropts', 'ipo', 'instcombine', 'target']
llvm_dep = dependency('llvm', modules : llvm_modules, required : true, version : '>=15')

libllvmc = library('llvmc',
  llvmc_sources,
  include_directories: llvmc_includes,
  dependencies: [depzcore, depzbytecode, llvm_dep],
  install: true
)

depllvmc = declare_dependency(
  include_directories: llvmc_includes,
  link_with: libllvmc
)
