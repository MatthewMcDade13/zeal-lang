#ifndef _ZEAL_LIBS_INCL_DEBUG_H_
#define _ZEAL_LIBS_INCL_DEBUG_H_

#include <stdio.h>
#include <stdlib.h>

// Define NDEBUG to disable assertions in release builds
#if defined(NDEBUG)
#define ASSERT(Condition, ...) ((void) 0)
#define ASSERT_MSG(Condition, Message, ...) ((void) 0)
#define ASSERT_FAIL(Message, ...) ((void) 0)
#else
// Internal implementation - don't call directly
static inline void _assert_handler(const char* File, int Line, const char* Func,
                                   const char* Condition, const char* Message) {
    // Print to stderr first in case the abort handler doesn't flush
    if (Message) {
        fprintf(stderr,
                "\n"
                "### ASSERTION FAILED ###\n"
                "File: %s\n"
                "Line: %d\n"
                "Function: %s\n"
                "Condition: %s\n"
                "Message: %s\n"
                "\n",
                File, Line, Func, Condition, Message);
    } else {
        fprintf(stderr,
                "\n"
                "### ASSERTION FAILED ###\n"
                "File: %s\n"
                "Line: %d\n"
                "Function: %s\n"
                "Condition: %s\n"
                "\n",
                File, Line, Func, Condition);
    }

    // Force a core dump/crash
    abort();
}

#define ASSERT(Condition, ...)                                                      \
    do {                                                                            \
        if (!(Condition)) {                                                         \
            _assert_handler(__FILE__, __LINE__, __func__, #Condition, __VA_ARGS__); \
        }                                                                           \
    } while (0)

#define ASSERT_MSG(Condition, Message, ...)                                    \
    do {                                                                       \
        if (!(Condition)) {                                                    \
            _assert_handler(__FILE__, __LINE__, __func__, #Condition, Message, \
                            ##__VA_ARGS__);                                    \
        }                                                                      \
    } while (0)

#define ASSERT_FAIL(Message, ...)                                                   \
    do {                                                                            \
        _assert_handler(__FILE__, __LINE__, __func__, "Explicit assertion failure", \
                        Message, ##__VA_ARGS__);                                    \
    } while (0)
#endif

#endif
