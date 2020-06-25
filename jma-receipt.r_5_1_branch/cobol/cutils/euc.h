/*
 orca
*/

#define SPLF         1
#define HIRAGANA     2
#define KATAKANA     4
#define OTHER        8
#define ZENKAKU     14
#define ZENSPLF     15
#define NUMBER      16
#define ALPHA       32
#define KIGOU       64
#define ASCII      112
#define ASCSPLF    113
#define HKANA      128
#define GAIJI      256
#define UNKNOWN    256

#define ISZENKAKU(c)    	(((c) >= 0xa1) && ((c) <= 0xfe))
#define ISHANKAKUKANA(c)	(((c)&0xff) == 0x8e)
#define ISDAKU(c)			(((c)&0xff) == 0xde)
#define ISHANDAKU(c)		(((c)&0xff) == 0xdf)
#define ISHIRAGANA(c)		(((c)&0xff) == 0xa4)
#define ISKATAKANA(c)		(((c)&0xff) == 0xa5)
#define MAYBEDAKU(c)		((c)-0xb6u < 15 || (c)-0xcau < 5)
#define MAYBEHANDAKU(c)		((c)-0xcau < 5)
#define ISHANKAKU(c)		(((c)&0xff) <= 0x7e)
#define ISLF(c)         	(((c)&0xff) == 0x0a)
#define ISG3(c)         	(((c)&0xff) == 0x8f)
#define ISSPACEONLY(c)     	(((c)&0xff) == 0x20)

typedef struct {
	int  out_len;
	int in_type;
	int out_type;
	unsigned char out_char[3];
} ConvertChar;

#define STR_SIZE 50000

typedef struct {
	char str1[STR_SIZE+1];
	char str2[STR_SIZE+1];
	char str3[STR_SIZE+1];
	int	num1;
	int num2;
} ORCSSTRING;

int ISJIS208(unsigned char c0, unsigned char c1);
void StringCobol2C(char	*str, int size);
void StringC2Cobol(char	*str,  int size);
int search_ascii_zenkaku(unsigned char d0, unsigned char d1);
int asciiconv(int conv_flg, int char_type, unsigned char inputChar, ConvertChar *cchar);
int hankakukanaconv(int conv_flg, int char_type, unsigned char c0, unsigned char c1, unsigned char c2, unsigned char c3, ConvertChar *cchar);
void gaiji(ConvertChar *cchar);
int zenkakuconv(int conv_flg, int char_type, unsigned char c0, unsigned char c1, ConvertChar *cchar);
int g3conv(int conv_flg, int char_type, unsigned char c0, unsigned char c1, unsigned char c2, ConvertChar *cchar);
void tokatakana(ConvertChar *cchar);
void tohiragana(ConvertChar *cchar);
int kana_euc_convert (int conv_flg, int char_type, int max_len, int *ret_len, char *inchar, char *ouchar);

int  jis213_count(char *in,char *out,int *result);
void jis213_check(ORCSSTRING *str);
void jis213_substr(ORCSSTRING *str);
void jis213_search(ORCSSTRING *str);
void jis213_gsub(ORCSSTRING *str);
void jis213_sub(ORCSSTRING *str);
void jis213_nstrcat(char *out,char *in,int size);
void jis213_nstrncat(char *out,char *in,int n,int size);


