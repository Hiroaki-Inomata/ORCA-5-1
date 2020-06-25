/*
  CALL "coblog" USING "Message".
*/

#include <stdio.h>
#include <syslog.h>

int
coblog (char *message)
{
	fprintf(stderr, "coblog: %s\n", message);
	syslog(LOG_ERR, "%s", message);
	return 0;
}
