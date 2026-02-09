/**
 * Simple standard-output logging library with color support.
 *
 * Log messages of various levels can be added with the log_<level> macros that take a printf-
 * style format string and zero or more format parameters.
 *
 * All levels of log macros can be disabled at compile time by unsetting the LOG_ENABLE macro.
 * At runtime, the minimum log level to be displayed can be controlled with the log_set_level
 * function.
 *
 * @author Jonathan Uhler
 */


#ifndef _LOGGER_H_
#define _LOGGER_H_


#include <stdio.h>


#define LOG_COLOR_RESET "\033[0m"
#define LOG_COLOR_TRACE "\033[37m"
#define LOG_COLOR_DEBUG "\033[30m"
#define LOG_COLOR_INFO  "\033[32m"
#define LOG_COLOR_WARN  "\033[33m"
#define LOG_COLOR_ERROR "\033[91m\033[1m"
#define LOG_COLOR_FATAL "\033[38;5;124m\033[1m"


#ifndef LOG_DISABLE
#define LOG_PRINT(severity, level, ...)                                                            \
    if (log_level >= level) {                                                                      \
        fprintf(stderr, "[%s:%s:%d] [" severity "] ", __FILE__, __func__, __LINE__);               \
        fprintf(stderr, __VA_ARGS__);                                                              \
        fprintf(stderr, "\n");                                                                     \
    }
#define log_fatal(...)                                                                             \
    LOG_PRINT(LOG_COLOR_FATAL "fatal" LOG_COLOR_RESET, LOG_LEVEL_FATAL, __VA_ARGS__)
#define log_error(...)                                                                             \
    LOG_PRINT(LOG_COLOR_ERROR "error" LOG_COLOR_RESET, LOG_LEVEL_ERROR, __VA_ARGS__)
#define log_warn(...) LOG_PRINT(LOG_COLOR_WARN "warn" LOG_COLOR_RESET, LOG_LEVEL_WARN, __VA_ARGS__)
#define log_info(...) LOG_PRINT(LOG_COLOR_INFO "info" LOG_COLOR_RESET, LOG_LEVEL_INFO, __VA_ARGS__)
#define log_debug(...)                                                                             \
    LOG_PRINT(LOG_COLOR_DEBUG "debug" LOG_COLOR_RESET, LOG_LEVEL_DEBUG, __VA_ARGS__)
#define log_trace(...)                                                                             \
    LOG_PRINT(LOG_COLOR_TRACE "trace" LOG_COLOR_RESET, LOG_LEVEL_TRACE, __VA_ARGS__)
#else // LOG_DISABLE
#define log_fatal(...)
#define log_error(...)
#define log_warn(...)
#define log_info(...)
#define log_debug(...)
#define log_trace(...)
#endif // LOG_DISABLE


/**
 * Enumeration of all supported log levels.
 */
enum log_level {
    LOG_LEVEL_FATAL = 0,
    LOG_LEVEL_ERROR = 10,
    LOG_LEVEL_WARN = 20,
    LOG_LEVEL_INFO = 30,
    LOG_LEVEL_DEBUG = 40,
    LOG_LEVEL_TRACE = 50
};


/** The current minimum log level to send to standard output. Do not modify this directly. */
extern enum log_level log_level;


/**
 * Set a new minimum log level.
 */
void log_set_level(enum log_level new_level);


#endif // _LOGGER_H_
