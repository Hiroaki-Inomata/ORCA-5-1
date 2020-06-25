#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <libgen.h>

#include "euc.h"

#define STR_LEN 1024

/*
   for OpenCOBOL 
   arg {
       int str_len,
       int mode,
       char envname[str_len],
       char in_filename[str_len],
       char fullpath[str_len],
       char out_dirname[str_len],
       char out_filename[str_len],
       int writable,
       int file_exists,
       int returncode
   }
*/



int search_file(
		int	str_len,
		int	access_mode,
		char *envname,
		char *in_filename,
		char *fullpath,
		char *out_dirname,
		char *out_filename,
		int  *writable,
		int  *file_exists)
{
	char *path;
	char delimiter[]=":";
	char *tok;
	char buf_path[STR_LEN];
	char filename[STR_LEN];
	int  rc;
    struct stat buf ; 

	if (getenv(envname) != NULL){
		path = strdup(getenv(envname));
	}else{
		getcwd(filename,sizeof(filename));
		path = strdup(filename);
	}
	tok = strtok(path,delimiter);
	while( tok != NULL ){
		sprintf(filename , "%s/%s" ,tok ,in_filename);
		if (stat(filename, &buf) == 0 && S_ISREG(buf.st_mode) == 1 ){
			rc = access(filename,R_OK);
			if ( rc == 0 ){
				strncpy(fullpath,filename,str_len);

				strncpy(buf_path,filename,str_len);
				strncpy(out_dirname,dirname(buf_path),str_len);

				strncpy(buf_path,filename,str_len);
				strncpy(out_filename,basename(buf_path),str_len);

				if (access(filename,F_OK) == 0){
					*file_exists = 1;
				};
				if (access(filename,W_OK) == 0){
					*writable = 1;
				};
				return rc;
			}
		}
		tok = strtok( NULL, delimiter );
	}
	return -1;
}

void orcaccess (char *args)
{
    int  str_len,mode,access_mode;
    char *envname;
    char *in_filename;
	char buf_path[STR_LEN];
    char filename[STR_LEN];
    char *fullpath;
    char *out_dirname;
    char *out_filename;
    int  *writable,*file_exists,*retcode;
    struct stat buf ; 

	char *p;

	p = args;

    str_len = *(size_t *)(p);
	p += sizeof(int);

	mode = *(int *)(p);
	p += sizeof(int);

	envname=p;
	StringCobol2C(envname, str_len);
	p += str_len;

	in_filename=p;
	StringCobol2C(in_filename, str_len);
	p += str_len;

	fullpath=p;
	p += str_len;

	out_dirname=p;
	p += str_len;

	out_filename=p;
	p += str_len;

	writable = (int *)(p);
	p += sizeof(int);

	file_exists = (int *)(p);
	p += sizeof(int);

	retcode = (int *)(p);

    *writable=0;
    *file_exists=0;
    *file_exists=0;

    access_mode = F_OK;

	if ((mode & 1 ) > 0){
		access_mode += X_OK;
	}

	if ((mode & 2 ) > 0){
		access_mode += W_OK;
	}

	if ((mode & 4 ) > 0){
		access_mode += R_OK;
	}
	if ( *envname != 0 && strchr(in_filename,'/') == 0 && (access_mode & X_OK)){
		*retcode= search_file(str_len,access_mode,envname,in_filename,fullpath,out_dirname,out_filename,writable,file_exists);
	}else{
		strncpy(filename,in_filename,str_len);
		if (strchr(in_filename,'/') == 0){
			getcwd(filename,sizeof(filename));
			sprintf(filename,"%s/%s",filename,in_filename);
		}
		strncpy(fullpath,filename,str_len);

		strncpy(buf_path,filename,str_len);
		strncpy(out_dirname,dirname(buf_path),str_len);

		strncpy(buf_path,filename,str_len);
		strncpy(out_filename,basename(buf_path),str_len);

		if (stat(out_dirname, &buf) == 0 && S_ISDIR(buf.st_mode) == 1 && access(out_dirname,W_OK) == 0){
			if (access(filename,F_OK) == 0){
				if (stat(filename, &buf) == 0 && S_ISDIR(buf.st_mode) == 0){
					*writable = 1;
				}
			}else{
					*writable = 1;
			}
		};
		if (stat(filename, &buf) == 0 && S_ISREG(buf.st_mode) == 1){
			if (access(filename,F_OK) == 0){
				*file_exists = 1;
			};
			*retcode= access(filename,access_mode);
		}else{
			*retcode = -1;
		}
	}
	StringC2Cobol(envname, str_len);
	StringC2Cobol(in_filename, str_len);
	StringC2Cobol(fullpath, str_len);
	StringC2Cobol(out_dirname, str_len);
	StringC2Cobol(out_filename, str_len);
}

/* liked data stucture */
/*
typedef struct
{
    int  str_len;
	int	 mode;
    char envname[1024];
    char in_filename[1024];
    char fullpath[1024];
    char out_dirname[1024];
    char out_filename[1024];
    int  writable;
    int  file_exists;
    int  retcode;
} COBOLDATA;


*/
