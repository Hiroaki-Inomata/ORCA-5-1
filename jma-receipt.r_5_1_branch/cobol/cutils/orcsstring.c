#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#include "euc.h"

/*
	for OpenCOBOL 
	arg {
		char command[8],
		char str1[STR_SIZE],
		char str2[STR_SIZE],
		char str3[STR_SIZE],
		int num1,
		int num2
	}
*/
extern void
orcsstring (char *args)
{
	char command[9];
	char num[4];
	ORCSSTRING data;
	char *p;
	int *retnum1;
	int *retnum2;

	p = args;

	memcpy(command,p,8);
	command[8] = 0;
	p += 8;

	memcpy(data.str1,p,STR_SIZE);
	StringCobol2C(data.str1, STR_SIZE);
	p += STR_SIZE;
	memcpy(data.str2,p,STR_SIZE);
	StringCobol2C(data.str2, STR_SIZE);
	p += STR_SIZE;
	memcpy(data.str3,p,STR_SIZE);
	StringCobol2C(data.str3, STR_SIZE);
	p += STR_SIZE;

	data.num1 = *(int *)(p);
	retnum1 = (int *)(p);
	p += sizeof(int);
	
	data.num2 = *(int *)(p);
	retnum2 = (int *)(p);
	p += sizeof(int);

	switch(command[0]) {
		case 'c':
#ifdef DEBUG
			fprintf(stderr,"orcsstring: check\n");
#endif
			jis213_check(&data);
			break;
		case 's':
			if (!strncmp("substr", command, strlen("substr"))) {
#ifdef DEBUG
				fprintf(stderr,"orcsstring: substring\n");
#endif
				jis213_substr(&data);
			} else if (!strncmp("sub", command, strlen("sub"))) {
#ifdef DEBUG
				fprintf(stderr,"orcsstring: sub\n");
#endif
				jis213_sub(&data);
			} else if (!strncmp("search", command, strlen("search"))) {
#ifdef DEBUG
				fprintf(stderr,"orcsstring: search\n");
#endif
				jis213_search(&data);
			} else {
				fprintf(stderr,"orcsstring: invalid command:%s\n", command);
			}
			break;
		case 'g':
#ifdef DEBUG
			fprintf(stderr,"orcsstring: gsub\n");
#endif
			jis213_gsub(&data);
			break;
		default:
			fprintf(stderr,"orcsstring: invalid command:%s\n", command);
			break;
	}

	p = args + 8;
	StringC2Cobol(data.str1, STR_SIZE);
	memcpy(p, data.str1, STR_SIZE);
	p += STR_SIZE;
	StringC2Cobol(data.str2, STR_SIZE);
	memcpy(p, data.str2, STR_SIZE);
	p += STR_SIZE;
	StringC2Cobol(data.str3, STR_SIZE);
	memcpy(p, data.str3, STR_SIZE);
	p += STR_SIZE;
	*retnum1 = data.num1;
	*retnum2 = data.num2;
}

int
main(
	int argc,
	char **argv)
{
	int command;
	ORCSSTRING data;

	if (argc < 7) {
		printf("$ ./testorcsstring <command> <str1> <str2> <str3> <num1> <num2>\n");
		printf("command = {0:check, 1:substr, 2:search, 3:gsub 4:sub}\n");
		exit(1);
	}
	command = atoi(argv[1]);
	strncpy(data.str1,argv[2],STR_SIZE);
	data.str1[STR_SIZE] = 0;
	strncpy(data.str2,argv[3],STR_SIZE);
	data.str2[STR_SIZE] = 0;
	strncpy(data.str3,argv[4],STR_SIZE);
	data.str3[STR_SIZE] = 0;
	data.num1 = atoi(argv[5]);
	data.num2 = atoi(argv[6]);

	switch(command) {
		case 0:
			jis213_check(&data);
			break;
		case 1:
			jis213_substr(&data);
			break;
		case 2:
			jis213_search(&data);
			break;
		case 3:
			jis213_gsub(&data);
			break;
		case 4:
			jis213_sub(&data);
			break;
		default:
			printf("invalid command\n");
			exit(1);
			break;
	}
	printf("str1:%s\n",data.str1);
	printf("str2:%s\n",data.str2);
	printf("str3:%s\n",data.str3);
	printf("num1:%03d\n",data.num1);
	printf("num2:%03d\n",data.num2);

	return 0;
}
