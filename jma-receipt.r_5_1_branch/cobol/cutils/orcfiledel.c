/*
 orca
      file delete for COBOL CALL interface
      gcc -fPIC -shared orcfiledel.c -o orcfiledel.so
*/

#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>
#include	<unistd.h>

extern void orcfiledel(char *arg);

/* CALLインターフェイス連絡領域 */
struct str_chardata_contact
{
	char	retcd[2];
	char	filename[1024];
};

void orcfiledel(char *arg)
{
	char	filename_buf[1024];
	int	rc;
	int	i;

	struct	str_chardata_contact	*lt_contact;
	
	lt_contact = (struct str_chardata_contact *)arg;
	strncpy(filename_buf,lt_contact->filename, 1024);
	for( i = 1023 ; i >= 0 && filename_buf[i] == ' ' ; i-- )
	{
		filename_buf[i] = '\0';
	}
	rc = 0;
	if(filename_buf[1023] != '\0')
		rc = -1;
	else
		rc = unlink(filename_buf);

	/* エラー */
	if(rc == -1){
		lt_contact->retcd[0] = '-';
		lt_contact->retcd[1] = '1';
		return ;
	}

	/* 正常 */
	lt_contact->retcd[0] = '0';
	lt_contact->retcd[1] = '0';

	return ;
}































