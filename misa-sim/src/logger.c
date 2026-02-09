#include "logger.h"

enum log_level log_level;

void log_set_level(enum log_level new_level) {
    log_level = new_level;
}
