/*
 * orcsjistoeuc - convert from SJIS(CP932) to EUC-JP
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <iconv.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#if 0
#define DEBUG 1
#endif
#ifdef DEBUG
#include <syslog.h>
#define OPENLOG	openlog("orcsjistoeuc", LOG_PID, LOG_DAEMON)
#define SYSLOG(msg)	syslog(LOG_INFO, (msg))
#else
#define OPENLOG	/**/
#define SYSLOG(msg)	/**/
#endif

/*	struct orcsjistoeuc
 *		int inlength;
 *		int outlen;
 *		int ret;
 *		char inbuf[inlen];
 *		char outbuf[outlen];
 */

void
orcsjistoeuc(char *ctx)
{
	int 	inlen;
	int 	outlen;
	int 	*ret;
	int		rc;
	char	*inbuf;
	char	*outbuf;
	char	*p;
	size_t	sib;
	size_t	sob;
	iconv_t	cd;

OPENLOG;
	p = ctx;
	inlen = *(int*)p;
	p += sizeof(int);
	
	outlen = *(int*)p;
	p += sizeof(int);

	ret = (int*)p;
	p += sizeof(int);

	inbuf = p;
	p += inlen;

	outbuf = p;
	p += outlen;

	sib = inlen;
	sob = outlen;
	memset(outbuf,' ',sob);
	*ret = 0;

	cd = iconv_open("euc-jp","shift-jis");
	rc = iconv(cd,&inbuf,&sib,&outbuf,&sob);
	if (rc != 0) {
		switch(errno) {
		case EILSEQ:
			*ret = 1;
			fprintf(stderr,"[%d][%02x%02x]\n",(int)sib,
			*(unsigned char*)(inbuf),
			*(unsigned char*)(inbuf+1));
			SYSLOG("invalid sequence");
			break;
		case E2BIG:
			*ret = 2;
			SYSLOG("buffer size overflow");
			break;
		default:
			*ret = 3;
			SYSLOG("other error");
			break;
		}
	}
SYSLOG("end");
	return;
}

#if 1
int
main(int argc, 
	char *argv[])
{
	FILE *fp;
	struct stat st;
	char *buf;
	int *ip;
	size_t size;

	if(argc < 2){
		printf("\nusage:orcsjistoeuc2 <infile>\n");
		exit(0);
	}
	if(stat(argv[1],&st)!=0) {
		printf("stat failed:%s\n",argv[1]);
		exit(1);
	}
	size = st.st_size*2+sizeof(int)*3;
	buf = malloc(size);
	if((fp = fopen(argv[1],"r")) == NULL) {
		printf("fopen failed:%s\n",argv[1]);
		exit(1);
	}
	fread(buf+sizeof(int)*3,st.st_size,1,fp);
	fclose(fp);

	ip = (int*)buf;
	*ip = (int)st.st_size;
	ip = (int*)(buf+sizeof(int));
	*ip = (int)st.st_size;
	orcsjistoeuc(buf);
	ip = (int*)(buf+sizeof(int)*2);
	fprintf(stderr,"result:%d\n",*ip);

	fp = fopen("out.txt","w");
	fwrite(buf+sizeof(int)*3+st.st_size,st.st_size,1,fp);
	fclose(fp);
	free(buf);
	return 0;
}
#endif
