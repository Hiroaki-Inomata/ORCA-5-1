#if 0
#!/bin/bash
src=$0
obj=${src%.*}
gcc -g -Wl,--no-as-needed -lm -o $obj $src
$obj
exit
#endif

/*
 * orcfileencode - File Code encode front end for jma-receipt
 *
 */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <iconv.h>

/*#define DEBUG*/
#ifdef DEBUG
#include <syslog.h>
#define OPENLOG	openlog("orcfileencode", LOG_PID, LOG_DAEMON)
#define SYSLOG(msg)	syslog(LOG_INFO, (msg))
#else
#define OPENLOG	/**/
#define SYSLOG(msg)	/**/
#endif

/*
 *	struct fileencode_ctx;
 *		char infile[256];		INPUT FILENAME 
 *		char outfile[256];		OUTPUT FILENAME
 *		char opt_hint;			ENCODE HINT(kanji or 8bit)
 *		char rc[1];			RETURN CODE
 *	};
 */

#define	MAX_DATA_SIZE	7090 * 16

#define	SIZE_INFILE			256
#define SIZE_OUTFILE			256
#define SIZE_HINT			1
#define SIZE_RET_CODE		1

#define OFFSET_INFILE		(0)
#define OFFSET_OUTFILE		(OFFSET_INFILE 		+ SIZE_INFILE)
#define OFFSET_HINT		(OFFSET_OUTFILE		+ SIZE_OUTFILE)
#define OFFSET_RET_CODE		(OFFSET_HINT		+ SIZE_HINT)

#define ENCODE_OK		'0'
#define ENCODE_ERROR	'1'
#define CHAR_CONV_ERROR	'2'
#define PARAM_ERROR		'4'
#define KANA_CONV_ERROR	'5'

#define	CTX(p, offset)		(p + offset)

void orcfileencode(char *ctx);

static int
euc2sjis(char *file, 
	char *out)
{
	FILE *fp;
	char src[MAX_DATA_SIZE];
	char dst[MAX_DATA_SIZE];
	char *ip,*op;
	size_t sib;
	size_t sob;
	iconv_t cd;
	int rc;

	if((fp = fopen(file, "r")) == NULL)return -1;
	sib = fread(src, 1, MAX_DATA_SIZE, fp);
	if(sib == 0){
		return -1;	
	}
	fclose(fp);

	ip = src;
	op = dst;
	sob = MAX_DATA_SIZE;
	memset(dst,0,MAX_DATA_SIZE);
	cd = iconv_open("cp932","euc-jp");
	rc = iconv(cd,&ip,&sib,&op,&sob);
	if (rc != 0) {
		return -1;
	}
	strncpy(out,dst,MAX_DATA_SIZE);
	return 0;
}

static int
power(int base,
	int pow)
{
	if(pow <= 0){
		return 1;
	}
	else{
		return power(base, pow - 1) * base;	
	}
}

/* convert string to integer ; "0987" => (int)987 */
static int
ctx_string2int(char *p, int offset, int size)
{
	int ret = 0;
	int i;
	for(i = 0; i < size; i++) {
		if(	*CTX(p, offset + i) >= 0x30 &&
			*CTX(p, offset + i) <= 0x39 ) {
			ret	+= ((*CTX(p, offset + i) - 0x30) % 10) * power(10, size - i - 1);
		}
	}
	return ret;
}

/* Shift-JIS hankaku-kana zenkaku-kana mapping table */
static unsigned char
z2h_map[][2][2] = {
	{{0x81,0x41},{0x00,0xA4}}, {{0x81,0x42},{0x00,0xA1}},
	{{0x81,0x45},{0x00,0xA5}}, {{0x81,0x4A},{0x00,0xDE}},
	{{0x81,0x4B},{0x00,0xDF}}, {{0x81,0x5b},{0x00,0xB0}},
	{{0x81,0x75},{0x00,0xA2}}, {{0x81,0x76},{0x00,0xA3}},
	{{0x83,0x40},{0x00,0xA7}}, {{0x83,0x41},{0x00,0xB1}},
	{{0x83,0x42},{0x00,0xA8}}, {{0x83,0x43},{0x00,0xB2}},
	{{0x83,0x44},{0x00,0xA9}}, {{0x83,0x45},{0x00,0xB3}},
	{{0x83,0x46},{0x00,0xAA}}, {{0x83,0x47},{0x00,0xB4}},
	{{0x83,0x48},{0x00,0xAB}}, {{0x83,0x49},{0x00,0xB5}},
	{{0x83,0x4A},{0x00,0xB6}}, {{0x83,0x4B},{0xB6,0xDE}},
	{{0x83,0x4C},{0x00,0xB7}}, {{0x83,0x4D},{0xB7,0xDE}},
	{{0x83,0x4E},{0x00,0xB8}}, {{0x83,0x4F},{0xB8,0xDE}},
	{{0x83,0x50},{0x00,0xB9}}, {{0x83,0x51},{0xB9,0xDE}},
	{{0x83,0x52},{0x00,0xBA}}, {{0x83,0x53},{0xBA,0xDE}},
	{{0x83,0x54},{0x00,0xBB}}, {{0x83,0x55},{0xBB,0xDE}},
	{{0x83,0x56},{0x00,0xBC}}, {{0x83,0x57},{0xBC,0xDE}},
	{{0x83,0x58},{0x00,0xBD}}, {{0x83,0x59},{0xBD,0xDE}},
	{{0x83,0x5A},{0x00,0xBE}}, {{0x83,0x5B},{0xBE,0xDE}},
	{{0x83,0x5C},{0x00,0xBF}}, {{0x83,0x5D},{0xBF,0xDE}},
	{{0x83,0x5E},{0x00,0xC0}}, {{0x83,0x5F},{0xC0,0xDE}},
	{{0x83,0x60},{0x00,0xC1}}, {{0x83,0x61},{0xC1,0xDE}},
	{{0x83,0x62},{0x00,0xAF}}, {{0x83,0x63},{0x00,0xC2}},
	{{0x83,0x64},{0xC2,0xDE}}, {{0x83,0x65},{0x00,0xC3}},
	{{0x83,0x66},{0xC3,0xDE}}, {{0x83,0x67},{0x00,0xC4}},
	{{0x83,0x68},{0xC4,0xDE}}, {{0x83,0x69},{0x00,0xC5}},
	{{0x83,0x6A},{0x00,0xC6}}, {{0x83,0x6B},{0x00,0xC7}},
	{{0x83,0x6C},{0x00,0xC8}}, {{0x83,0x6D},{0x00,0xC9}},
	{{0x83,0x6E},{0x00,0xCA}}, {{0x83,0x6F},{0xCA,0xDE}},
	{{0x83,0x70},{0xCA,0xDF}}, {{0x83,0x71},{0x00,0xCB}},
	{{0x83,0x72},{0xCB,0xDE}}, {{0x83,0x73},{0xCB,0xDF}},
	{{0x83,0x74},{0x00,0xCC}}, {{0x83,0x75},{0xCC,0xDE}},
	{{0x83,0x76},{0xCC,0xDF}}, {{0x83,0x77},{0x00,0xCD}},
	{{0x83,0x78},{0xCD,0xDE}}, {{0x83,0x79},{0xCD,0xDF}},
	{{0x83,0x7A},{0x00,0xCE}}, {{0x83,0x7B},{0xCE,0xDE}},
	{{0x83,0x7C},{0xCE,0xDF}}, {{0x83,0x7D},{0x00,0xCF}},
	{{0x83,0x7E},{0x00,0xD0}}, {{0x83,0x80},{0x00,0xD1}},
	{{0x83,0x81},{0x00,0xD2}}, {{0x83,0x82},{0x00,0xD3}},
	{{0x83,0x83},{0x00,0xAC}}, {{0x83,0x84},{0x00,0xD4}},
	{{0x83,0x85},{0x00,0xAD}}, {{0x83,0x86},{0x00,0xD5}},
	{{0x83,0x87},{0x00,0xAE}}, {{0x83,0x88},{0x00,0xD6}},
	{{0x83,0x89},{0x00,0xD7}}, {{0x83,0x8A},{0x00,0xD8}},
	{{0x83,0x8B},{0x00,0xD9}}, {{0x83,0x8C},{0x00,0xDA}},
	{{0x83,0x8D},{0x00,0xDB}}, {{0x83,0x8F},{0x00,0xDC}},
	{{0x83,0x92},{0x00,0xA6}}, {{0x83,0x93},{0x00,0xDD}},
	{{0x83,0x8D},{0x00,0xDB}}, {{0x83,0x8F},{0x00,0xDC}},
	{{0x83,0x92},{0x00,0xA6}}, {{0x83,0x93},{0x00,0xDD}},
	{{0x83,0x94},{0xB3,0xDE}}, {{0x81,0x40},{0x00,0x20}}
};

static void
z2h(unsigned char *in,
	unsigned char *out)
{
	int i;
	int j;
	int k;
	int size;
	size = strlen(in);
	for(i = 0, k = 0; i < size; ){
		/* hankaku kana */
		if(in[i] >= 0xA1 && in[i] <= 0xDF){
			out[k] = in[i];
			i++;
			k++;
		}
		/* space */
		else if(in[i] <= 0x7f){
			if(in[i] == 0x20){
				out[k] = in[i];
				k++;
			}
			i++;
		}
		/* zenkaku kana */
		else{
			for(j = 0; j < sizeof(z2h_map)/4; j++){
				if(in[i+0] == z2h_map[j][0][0] &&
					in[i+1] == z2h_map[j][0][1]){
					if(z2h_map[j][1][0] != 0x00){
						out[k+0] = z2h_map[j][1][0];
						out[k+1] = z2h_map[j][1][1];
						k += 2;
					}
					else{
						out[k+0] = z2h_map[j][1][1];
						k++;
					}
					j = 0;
					break;
				}
			}
			i += 2;
		}
	}
	out[k] = 0x00;
}

#define MAX_COLUMN_NUM 32
#define MAX_COLUMN_SIZE 256


/* parse csv, and convert patient name zenkaku kana to hankaku kana */
/* csv line was terminated 0x0d,0x0a  */
static int
parse_csv(char *in, 
	char *out)
{
	char *str;
	char *record;
	char *save;
	char delim[] = {0x0d, 0x0a, 0x00};
	char *p;
	char column[MAX_COLUMN_NUM][MAX_COLUMN_SIZE];
	char buf[MAX_COLUMN_SIZE];
	int loop;
	int i;
	int j;

	out[0] = 0x00;
	for(str = in; ; str = NULL){
		record = strtok_r(str, delim, &save);
		if(record == NULL) break;

		p = record;
		i = j = 0;
		loop = 1;
		while(loop){
			switch(*p){
				case ',':
					column[i][j] = 0x00;
					i++;
					if(i >= MAX_COLUMN_NUM) return(-1);
					j = 0;
					break;
				case 0x00:
					column[i][j] = 0x00;
					loop = 0;
					j = 0;
					break;
				default:
					column[i][j] = *p;
					j++;
					if(j >= MAX_COLUMN_SIZE) return(-1);
					break;
			}
			p++;
		}

		/* Patient name record  */
		if(strcmp(column[0], "11") == 0){
			for(j = 0; j <= i; j++){
				if(j == 3){
					z2h(column[j], buf);
					strcat(out, buf);
				}
				else{
					strcat(out, column[j]);
				}
				if(j < i) strcat(out, ",");
			}
		}
		else{
			strcat(out, record);
		}
		strcat(out, delim);
	}
	return(0);
}

static void
lf2crlf(char *in, 
	char *out)
{
	unsigned char *p,*q;

	out[0] = 0x00;
	for(p = in, q = out; *p != 0; p++,q++){
		if (*p == 0x0a) { 
			*q = 0x0d;
			q++;
			*q = 0x0a;
		} else {
			*q = *p;
		}
	}
	*q = 0;
}

static
void print_ctx(char *ctx)
{
	printf("==== orcfileencode ctx\n");
	printf("infile:%s\n", CTX(ctx, OFFSET_INFILE));
	printf("outfile:%s\n", CTX(ctx, OFFSET_OUTFILE));
	printf("hint:%02x\n", *CTX(ctx, OFFSET_HINT));
	printf("ret_code:%02X\n", *CTX(ctx, OFFSET_RET_CODE));
}

void
orcfileencode(char *ctx)
{
	FILE *fp;
	int hint;
	char buf[MAX_DATA_SIZE];
	char buf2[MAX_DATA_SIZE];
	char outfile[SIZE_OUTFILE];
	char *p;

OPENLOG;
	if((p = strchr(CTX(ctx, OFFSET_INFILE), ' ')) != NULL) *p = '\0';
	if((p = strchr(CTX(ctx, OFFSET_OUTFILE), ' ')) != NULL) *p = '\0';

	memset(CTX(ctx, OFFSET_RET_CODE), ENCODE_OK, SIZE_RET_CODE);

	if(euc2sjis(CTX(ctx, OFFSET_INFILE), buf) != 0){
		memset(CTX(ctx, OFFSET_RET_CODE), CHAR_CONV_ERROR, SIZE_RET_CODE);
		return;
	}

	hint = ctx_string2int(ctx, OFFSET_HINT, SIZE_HINT);

	if (hint < 2) {
		if(parse_csv(buf, buf2) !=0){
			memset(CTX(ctx, OFFSET_RET_CODE), KANA_CONV_ERROR, SIZE_RET_CODE);
			return;
		}
	} else {
		lf2crlf(buf,buf2);
	}

	snprintf(outfile, SIZE_OUTFILE, "%s", CTX(ctx, OFFSET_OUTFILE));

	if((fp = fopen(outfile, "w")) == NULL){
		memset(CTX(ctx, OFFSET_RET_CODE), ENCODE_ERROR, SIZE_RET_CODE);
		return;
	}
	fwrite(buf2, strlen(buf2), 1, fp);
	fclose(fp);
#if 1
	print_ctx(ctx);
#endif
SYSLOG("end");
	return;
}

#if 1
int
main(int argc, 
	char *argv[])
{
	char buf[OFFSET_RET_CODE + SIZE_RET_CODE];
	int len;

	if(argc < 3){
		printf("\nusage:orcfileencode <infile> <outfile> <hint>\n");
		exit(0);
	}

	memset(buf, 0, sizeof(buf));

	len = strlen(argv[1]);
	snprintf(CTX(buf, OFFSET_INFILE), SIZE_INFILE, "%s", argv[1]);
	memset(CTX(buf, OFFSET_INFILE) + len, ' ', SIZE_INFILE - len);
	len = strlen(argv[2]);
	snprintf(CTX(buf, OFFSET_OUTFILE), SIZE_OUTFILE, "%s", argv[2]);
	memset(CTX(buf, OFFSET_OUTFILE) + len, ' ', SIZE_OUTFILE - len);
	CTX(buf, OFFSET_INFILE)[SIZE_INFILE-1] = 0;
	CTX(buf, OFFSET_OUTFILE)[SIZE_OUTFILE-1] = 0;

	memcpy(CTX(buf, OFFSET_HINT), argv[3], SIZE_HINT);

	orcfileencode(buf);
	print_ctx(buf);

	return 0;
}
#endif
