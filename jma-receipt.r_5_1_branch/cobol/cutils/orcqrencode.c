#if 0
#!/bin/bash
src=$0
obj=${src%.*}
gcc -g -Wl,--no-as-needed -lm `pkg-config --cflags --libs libpng12 libqrencode` -o $obj $src
$obj
exit
#endif

/*
 * orcqrencode - QR Code encode front end for jma-receipt
 *
 */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <qrencode.h>
#include <png.h>
#include <iconv.h>

/*#define DEBUG*/
#ifdef DEBUG
#include <syslog.h>
#define OPENLOG	openlog("orcqrencode", LOG_PID, LOG_DAEMON)
#define SYSLOG(msg)	syslog(LOG_INFO, (msg))
#else
#define OPENLOG	/**/
#define SYSLOG(msg)	/**/
#endif

/*
 *	struct qrencode_ctx;
 *		char infile[1024];		INPUT FILENAME 
 *		char qrfile[1024];		OUTPUT FILENAME
 *		char opt_level;			ERROR CORRECTION LEVEL
 *		char opt_pixel;			PIXEL per dot
 *		char opt_margin;		SYMBOL MARGIN
 *		char opt_version[2];	SYMBOL VERSION
 *		char opt_hint;			QRENCODE HINT(kanji or 8bit)
 *		char opt_structured;	STRUCTURED FLAG(0:signle 1:structured)
 *		char out_version[2];	OUTPUT SYMBOL VERSION
 *		char number[2];			OUTPUT SYMBOL NUMBERS
 *		char rc[1];				RETURN CODE
 *	};
 */

#define	MAX_DATA_SIZE	7090 * 16

#define	SIZE_INFILE			1024
#define SIZE_QRFILE			1024
#define SIZE_LEVEL			1
#define SIZE_PIXEL			1
#define SIZE_MARGIN			1
#define SIZE_VERSION		2
#define SIZE_HINT			1
#define SIZE_STRUCTURED		1
#define SIZE_RET_VERSION	2
#define SIZE_RET_SYMBOLS	2
#define SIZE_RET_CODE		1

#define OFFSET_INFILE		(0)
#define OFFSET_QRFILE		(OFFSET_INFILE 		+ SIZE_INFILE)
#define OFFSET_LEVEL		(OFFSET_QRFILE 		+ SIZE_QRFILE)
#define OFFSET_PIXEL		(OFFSET_LEVEL  		+ SIZE_LEVEL)
#define OFFSET_MARGIN		(OFFSET_PIXEL  		+ SIZE_PIXEL)
#define OFFSET_VERSION		(OFFSET_MARGIN 		+ SIZE_MARGIN)
#define OFFSET_HINT			(OFFSET_VERSION		+ SIZE_VERSION)
#define OFFSET_STRUCTURED	(OFFSET_HINT   		+ SIZE_HINT)
#define OFFSET_RET_VERSION	(OFFSET_STRUCTURED	+ SIZE_STRUCTURED)
#define OFFSET_RET_SYMBOLS	(OFFSET_RET_VERSION	+ SIZE_RET_VERSION)
#define OFFSET_RET_CODE		(OFFSET_RET_SYMBOLS + SIZE_RET_SYMBOLS)

#define QRENCODE_OK		'0'
#define QRENCODE_ERROR	'1'
#define CHAR_CONV_ERROR	'2'
#define WRITE_PNG_ERROR	'3'
#define PARAM_ERROR		'4'
#define KANA_CONV_ERROR	'5'

#define	CTX(p, offset)		(p + offset)

void orcqrencode(char *ctx);

static int 
writePNG(QRcode *qrcode, 
	const char *outfile,
	int size,
	int margin)
{
	FILE *fp;
	png_structp png_ptr;
	png_infop info_ptr;
	unsigned char *row, *p, *q;
	int x, y, xx, yy, bit;
	int realwidth;
	
	realwidth = (qrcode->width + margin * 2) * size;
	row = (unsigned char *)malloc((realwidth + 7) / 8);

	if(outfile[0] == '-' && outfile[1] == '\0') {
		fp = stdout;
	} else {
		fp = fopen(outfile, "w");
		if(fp == NULL) {
			return -1;
		}
	}

	png_ptr = png_create_write_struct(PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);
	if(png_ptr == NULL) {
		fclose(fp);
		return -1;
	}

	info_ptr = png_create_info_struct(png_ptr);
	if(info_ptr == NULL) {
		fclose(fp);
		return -1;
	}

	if(setjmp(png_jmpbuf(png_ptr))) {
		png_destroy_write_struct(&png_ptr, &info_ptr);
		fclose(fp);
		return -1;
	}

	png_init_io(png_ptr, fp);
	png_set_IHDR(png_ptr, info_ptr,
			realwidth, realwidth,
			1,
			PNG_COLOR_TYPE_GRAY,
			PNG_INTERLACE_NONE,
			PNG_COMPRESSION_TYPE_DEFAULT,
			PNG_FILTER_TYPE_DEFAULT);
	png_write_info(png_ptr, info_ptr);

	/* top margin */
	memset(row, 0xff, (realwidth + 7) / 8);
	for(y=0; y<margin * size; y++) {
		png_write_row(png_ptr, row);
	}

	/* data */
	p = qrcode->data;
	for(y=0; y<qrcode->width; y++) {
		bit = 7;
		memset(row, 0xff, (realwidth + 7) / 8);
		q = row;
		q += margin * size / 8;
		bit = 7 - (margin * size % 8);
		for(x=0; x<qrcode->width; x++) {
			for(xx=0; xx<size; xx++) {
				*q ^= (*p & 1) << bit;
				bit--;
				if(bit < 0) {
					q++;
					bit = 7;
				}
			}
			p++;
		}
		for(yy=0; yy<size; yy++) {
			png_write_row(png_ptr, row);
		}
	}
	/* bottom margin */
	memset(row, 0xff, (realwidth + 7) / 8);
	for(y=0; y<margin * size; y++) {
		png_write_row(png_ptr, row);
	}

	png_write_end(png_ptr, info_ptr);
	png_destroy_write_struct(&png_ptr, &info_ptr);

	fclose(fp);
	free(row);
	return 0;
}

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
	printf("==== orcqrencode ctx\n");
	printf("infile:%s\n", CTX(ctx, OFFSET_INFILE));
	printf("qrfile:%s\n", CTX(ctx, OFFSET_QRFILE));
	printf("level:%02x\n", *CTX(ctx, OFFSET_LEVEL));
	printf("pixel:%02x\n", *CTX(ctx, OFFSET_PIXEL));
	printf("margin:%02x\n", *CTX(ctx, OFFSET_MARGIN));
	printf("version:%02x%02x\n", 
		*CTX(ctx, OFFSET_VERSION), *CTX(ctx, OFFSET_VERSION+1));
	printf("hint:%02x\n", *CTX(ctx, OFFSET_HINT));
	printf("structured:%02x\n", *CTX(ctx, OFFSET_STRUCTURED));
	printf("ret_version:%02x%02x\n", 
		*CTX(ctx, OFFSET_RET_VERSION), *CTX(ctx, OFFSET_RET_VERSION+1));
	printf("ret_symbols:%02x%02x\n", 
		*CTX(ctx, OFFSET_RET_SYMBOLS), *CTX(ctx, OFFSET_RET_SYMBOLS+1));
	printf("ret_code:%02x\n", *CTX(ctx, OFFSET_RET_CODE));
}

void
orcqrencode(char *ctx)
{
	int version;
	int level;
	int hint;
	int size;
	int margin;
	int i;
	int doEncodeStructured;
	char buf[MAX_DATA_SIZE];
	char buf2[MAX_DATA_SIZE];
	char qrfile[SIZE_QRFILE];
	char qrfile_suffix[SIZE_QRFILE + 10];
	char *p;
	QRcode *code;
	QRcode_List *head, *entry;

OPENLOG;
	if((p = strchr(CTX(ctx, OFFSET_INFILE), ' ')) != NULL) *p = '\0';
	if((p = strchr(CTX(ctx, OFFSET_QRFILE), ' ')) != NULL) *p = '\0';

	memset(CTX(ctx, OFFSET_RET_CODE), QRENCODE_OK, SIZE_RET_CODE);

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

	snprintf(qrfile, SIZE_QRFILE, "%s", CTX(ctx, OFFSET_QRFILE));
	version = ctx_string2int(ctx, OFFSET_VERSION, SIZE_VERSION);
	if(version <= 0 || version > 40){
		memset(CTX(ctx, OFFSET_RET_CODE), PARAM_ERROR, SIZE_RET_CODE);
		return;	
	}
	switch(*CTX(ctx, OFFSET_LEVEL)){
		case 'L':
			level = QR_ECLEVEL_L;
			break;
		case 'M':
			level = QR_ECLEVEL_M;
			break;
		case 'Q':
			level = QR_ECLEVEL_Q;
			break;
		case 'H':
			level = QR_ECLEVEL_H;
			break;
		default:
			level = QR_ECLEVEL_L;
	}

	p = strrchr(qrfile, '.');
	if(p != NULL)*p = '\0';
	doEncodeStructured = ctx_string2int(ctx, OFFSET_STRUCTURED, SIZE_STRUCTURED);
	hint = (hint % 2) == 0 ? QR_MODE_KANJI : QR_MODE_8;
	size = ctx_string2int(ctx, OFFSET_PIXEL, SIZE_PIXEL);
	margin = ctx_string2int(ctx, OFFSET_MARGIN, SIZE_MARGIN);

	code = QRcode_encodeString(buf2, version, level, hint, 1);
	if (code != NULL) {
		if (code->version <= version) {
			snprintf(qrfile_suffix, sizeof(qrfile_suffix), 
				"%s_%02d.png", qrfile, 1);
			if(writePNG(code, qrfile_suffix, size, margin) != 0){
				memset(CTX(ctx, OFFSET_RET_CODE), WRITE_PNG_ERROR, SIZE_RET_CODE);
				QRcode_free(code);
				return;
			}
		SYSLOG("write single image");
			sprintf(buf, "%02d" , code->version);
			memcpy(CTX(ctx, OFFSET_RET_VERSION), buf, SIZE_RET_VERSION);
			sprintf(buf, "%02d" , 1);
			memcpy(CTX(ctx, OFFSET_RET_SYMBOLS), "01", SIZE_RET_SYMBOLS);
			QRcode_free(code);
			return;
		}
		QRcode_free(code);
	}
	if (doEncodeStructured) {
		head = QRcode_encodeStringStructured(buf2, version, level, hint ,1);
		if(head) {
			entry = head;
			i = 0;
			while(entry != NULL) {
				code = entry->code;
				snprintf(qrfile_suffix, 
					sizeof(qrfile_suffix), "%s_%02d.png", qrfile, i + 1);
				if(writePNG(code, qrfile_suffix, size, margin) != 0){
					memset(CTX(ctx, OFFSET_RET_CODE), WRITE_PNG_ERROR, SIZE_RET_CODE);
					break;
				}
				entry = entry->next;	
				i++;
			}
		SYSLOG("write multi images");
			QRcode_List_free(entry);
		}
		else {
			memset(CTX(ctx, OFFSET_RET_CODE), QRENCODE_ERROR, SIZE_RET_CODE);
		 	return;
		}
		sprintf(buf, "%02d" , head->code->version);
		memcpy(CTX(ctx, OFFSET_RET_VERSION), buf, SIZE_RET_VERSION);
		sprintf(buf, "%02d" , i);
		memcpy(CTX(ctx, OFFSET_RET_SYMBOLS), buf, SIZE_RET_SYMBOLS);
		QRcode_List_free(head);
	}
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

	if(argc < 7){
		printf("\nusage:orcqrencode <infile> <outfile> <version> <level> <hint> <structured>\n");
		exit(0);
	}

	memset(buf, 0, sizeof(buf));

	len = strlen(argv[1]);
	snprintf(CTX(buf, OFFSET_INFILE), SIZE_INFILE, "%s", argv[1]);
	memset(CTX(buf, OFFSET_INFILE) + len, ' ', SIZE_INFILE - len);
	len = strlen(argv[2]);
	snprintf(CTX(buf, OFFSET_QRFILE), SIZE_QRFILE, "%s", argv[2]);
	memset(CTX(buf, OFFSET_QRFILE) + len, ' ', SIZE_QRFILE - len);
	CTX(buf, OFFSET_INFILE)[SIZE_INFILE-1] = 0;
	CTX(buf, OFFSET_QRFILE)[SIZE_QRFILE-1] = 0;

	memcpy(CTX(buf, OFFSET_LEVEL), argv[4], SIZE_LEVEL);
	memcpy(CTX(buf, OFFSET_PIXEL), "3", SIZE_PIXEL);
	memcpy(CTX(buf, OFFSET_MARGIN), "1", SIZE_MARGIN);
	memcpy(CTX(buf, OFFSET_VERSION), argv[3], SIZE_VERSION);
	memcpy(CTX(buf, OFFSET_HINT), argv[5], SIZE_HINT);
	memcpy(CTX(buf, OFFSET_STRUCTURED), argv[6], SIZE_STRUCTURED);

	orcqrencode(buf);
	print_ctx(buf);

	return 0;
}
#endif
