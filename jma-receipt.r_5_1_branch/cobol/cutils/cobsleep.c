/*
  CALL "cobsleep" USING 1.
*/

#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>

void
cobsleep (char *interval)
{
  int s;
  char *tmp;

  tmp = strndup(interval,2);
  s = atoi(tmp);
  printf("%d\n", s);
  sleep(s);
  free(tmp);
}
