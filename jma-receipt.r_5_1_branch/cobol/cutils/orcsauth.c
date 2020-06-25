/*********************************************************************
  orcsauth.c
  gcc -fPIC -shared orcsauth.c -o orcsauth.so -lcrypt

    for OpenCOBOL 
    arg {
	char command[8],
	char id[2],
	char password1[128],
	char password2[128],
	char rc[1]
    }
*********************************************************************/

#include <crypt.h>

#include <sys/time.h>
#include <string.h>
#include <stdlib.h>

/*
 *	struct auth_ctx;
 *		char command[8];		COMMAND:"passwd" or "auth"
 *		char id[2];			ID:01:MD5
 *		char password1[STR_SIZE];	PASSWORD:plain text
 *		char password2[STR_SIZE];	PASSWORD:(crypt)
 *		char rc[1];			RETURN CODE
 */

#define STR_SIZE	 128

#define	SIZE_COMMAND		8
#define	SIZE_ID			2
#define	SIZE_PASSWORD1		STR_SIZE
#define	SIZE_PASSWORD2		STR_SIZE
#define SIZE_RET_CODE		1

#define OFFSET_COMMAND		(0)
#define OFFSET_ID		(OFFSET_COMMAND		+ SIZE_COMMAND)
#define OFFSET_PASSWORD1	(OFFSET_ID		+ SIZE_ID)
#define OFFSET_PASSWORD2	(OFFSET_PASSWORD1	+ SIZE_PASSWORD1)
#define OFFSET_RET_CODE		(OFFSET_PASSWORD2	+ SIZE_PASSWORD2)

#define RET_OK		'0'
#define RET_ERROR	'1'

#define	CTX(p, offset)		(p + offset)

void orcsauth(char *ctx);

const	char	*
AuthMakeSalt(void)
{
	struct	timeval	tv;
	static	char	result[40];

	result[0] = '\0';
	strcpy(result, "$1$");  /* magic for the new MD5 crypt() */

	/*
	 * Generate 8 chars of salt, the old crypt() will use only first 2.
	 */
	gettimeofday(&tv, (struct timezone *) 0);
	strcat(result, l64a(tv.tv_usec));
	strcat(result, l64a(tv.tv_sec + getpid() + clock()));

	if (strlen(result) > 3 + 8)  /* magic+salt */
		result[11] = '\0';

	return result;
}

void
orcsauth (char *ctx)
{
	char command[9];
	char id[3];
	char passwd1[STR_SIZE+1];
	char passwd2[STR_SIZE+1];
	char *p;

	memcpy(command, CTX(ctx, OFFSET_COMMAND), SIZE_COMMAND);
	if((p = strchr(command, ' ')) != NULL) *p = '\0';
	memcpy(id, CTX(ctx, OFFSET_ID), SIZE_ID);
	if((p = strchr(id, ' ')) != NULL) *p = '\0';
	memcpy(passwd1, CTX(ctx, OFFSET_PASSWORD1), SIZE_PASSWORD1);
	if((p = strchr(passwd1, ' ')) != NULL) *p = '\0';
	memcpy(passwd2, CTX(ctx, OFFSET_PASSWORD2), SIZE_PASSWORD2);
	if((p = strchr(passwd2, ' ')) != NULL) *p = '\0';

	memset(CTX(ctx, OFFSET_RET_CODE), RET_OK, SIZE_RET_CODE);

	if (!strncmp("passwd", command, strlen("passwd"))) {
		if (!strncmp("01", id, 2)) {
			strcpy(passwd2, crypt(passwd1, AuthMakeSalt()));
			memcpy(CTX(ctx, OFFSET_PASSWORD2), passwd2, strlen(passwd2));
		} else {
			memset(CTX(ctx, OFFSET_RET_CODE), RET_ERROR, SIZE_RET_CODE);
		}
	} else if (!strncmp("auth", command, strlen("auth"))) {
		if (!strncmp("01", id, 2)) {
			if (strcmp(passwd2, crypt(passwd1, passwd2)) != 0) {
				memset(CTX(ctx, OFFSET_RET_CODE), RET_ERROR, SIZE_RET_CODE);
			}
		} else {
			memset(CTX(ctx, OFFSET_RET_CODE), RET_ERROR, SIZE_RET_CODE);
		}
	} else {
		memset(CTX(ctx, OFFSET_RET_CODE), RET_ERROR, SIZE_RET_CODE);
	}

	return;
}
