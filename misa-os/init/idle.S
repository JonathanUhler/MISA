#include "api/task.h"


        .section text


mos_idle_task:
        task_yield
        goto mos_idle_task
