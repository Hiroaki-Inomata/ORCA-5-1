/*
      Make Directory for COBOL CALL interface
      gcc -fPIC -shared orcmkdir.c -o orcmkdir.so
*/

#include	<stdio.h>
#include	<string.h>
#include	<sys/stat.h>

/*
 *	struct mkdir_ctx;
 *		char directory[1024];	DIRECTORY NAME
 *		char mode[4];		MODE
 *		char rc[1];		RETURN CODE
 */

#define	SIZE_DIRECTORY		1024
#define	SIZE_MODE		4
#define SIZE_RET_CODE		1

#define OFFSET_DIRECTORY	(0)
#define OFFSET_MODE		(OFFSET_DIRECTORY	+ SIZE_DIRECTORY)
#define OFFSET_RET_CODE		(OFFSET_MODE		+ SIZE_MODE)

#define RET_OK		'0'
#define RET_ERROR	'1'

#define	CTX(p, offset)		(p + offset)

extern void orcmkdir(char *ctx);

void orcmkdir(char *ctx)
{
	char dirname[SIZE_DIRECTORY];
	char buf[SIZE_MODE+1];
	mode_t mode;
	char *p, *e;
	struct stat st;

	if((p = strchr(CTX(ctx, OFFSET_DIRECTORY), ' ')) != NULL) *p = '\0';

	memset(CTX(ctx, OFFSET_RET_CODE), RET_OK, SIZE_RET_CODE);

	snprintf(dirname, SIZE_DIRECTORY, "%s", CTX(ctx, OFFSET_DIRECTORY));
	memcpy(buf, CTX(ctx, OFFSET_MODE), SIZE_MODE);
	buf[SIZE_MODE] = '\0';
	mode = strtoul(buf, &e, 8);

	if (stat(dirname, &st) != 0){
		if (mkdir (dirname, mode) != 0)
			memset(CTX(ctx, OFFSET_RET_CODE), RET_ERROR, SIZE_RET_CODE);
	}	
	return ;
}
