#include <stdio.h>
#include <stdlib.h>
#include <syslog.h>

void
cobalert (char *message)
{
	fprintf(stderr, "ERROR: %s\n", message);
	syslog(LOG_ERR, "ERROR: %s", message);
}
