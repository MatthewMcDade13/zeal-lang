#ifndef ZEAL_CORE_INCL_C_COMMON_H_
#define ZEAL_CORE_INCL_C_COMMON_H_

#ifdef __cplusplus
#include "common_cpp.h"

#ifndef ZEAL_NAMESPACE_CAPI_BEGIN
#define ZEAL_NAMESPACE_CAPI_BEGIN(NAME) \
  namespace zeal::NAME { \
  extern "C" { 
#endif

#ifndef ZEAL_NAMESPACE_CAPI_END
#define ZEAL_NAMESPACE_CAPI_END \
} \
} \

#endif

#else
#include "common_c.h"

#ifndef ZEAL_NAMESPACE_CAPI_BEGIN
#define ZEAL_NAMESPACE_CAPI_BEGIN(NAME)
#endif

#ifndef ZEAL_NAMESPACE_CAPI_END
#define ZEAL_NAMESPACE_CAPI_END
#endif

#endif
#endif
