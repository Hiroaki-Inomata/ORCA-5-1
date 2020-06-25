#include <stdio.h>
#include <stdlib.h>
#include <syslog.h>

void
cobabort (char *message)
{
	fprintf(stderr, "ERROR: %s\n", message);
	syslog(LOG_ERR, "ERROR: %s", message);
	exit(2);
}
