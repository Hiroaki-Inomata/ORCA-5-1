#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include <memory.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <errno.h>

#include "euc.h"

/*
   for OpenCOBOL 
   arg {
       int len,
       int ret_val,
       int ret_len,
       int conv_flg,
       int char_type,
       int str_len,
       char org_str[str_len],
       char new_str[str_len]
   }
*/

/*
 *   返り値( 0: 正常 1: 範囲外文字 2: 外字、変換不能 )
      03 KANACONV-RETURN          PIC S9(9) BINARY.
 *   変換文字列数(バイト)
      03 KANACONV-MAXLEN          PIC S9(9) BINARY.
 *   変換フラグ (0: 変換しない,     1: 変換する
 *               2: 1+カタカナ変換, 3: 1+ひらがな変換)
 *   ※ 半角カナおよび外字は全てのフラグで変換される
 *      か捨てられる。
 *     (外字は■)
     03 KANACONV-CONV-FLAG       PIC S9(9) BINARY.
 *   出力文字種指定
 *       0: 半角カナ、外字のみ変換
 *      14: 全角   2: ひらがな  4: カタカナ
 *      112: ASCII 16: 数字     32: アルファベット
 *      15: 全角+スペース改行   113: ASCII+スペース改行
 *   出力文字種は OR をとることで複数指定できる
 *   例: (6: ひらがな、カタカナのみ)
 *       (48: 英数字のみ)
 */       

extern int
kanaconv (char *args)
{
	int conv_flg, char_type;
	int *ret_val;
	int len, str_len;
	int *ret_len;
	char *org_str, *new_str;
	char *p;

	p = args;

	len = *(size_t *)(p);

	p += sizeof(int);
	ret_val = (int *)(p);

	p += sizeof(int);
	ret_len = (int *)(p);

	p += sizeof(int);
	conv_flg = *(int *)(p);

	p += sizeof(int);
	char_type = *(int *)(p);

	p += sizeof(int);
	str_len = *(int *)(p);

	p += sizeof(int);
	org_str = p;
	StringCobol2C(org_str, str_len);

	p += str_len;
	new_str = p;
	StringCobol2C(new_str, str_len);

	if ( str_len < len){
		len = str_len;
	}
	*ret_val = kana_euc_convert(conv_flg, char_type, len, ret_len,
							 org_str, new_str);

	StringC2Cobol(org_str, str_len);
	StringC2Cobol(new_str, str_len);

	return *ret_val;
}

int
main(
	int argc,
	char **argv)
{
	int retval;
	int conv_flg;
	int char_type;
	char *in;
	char *out;
	struct stat st;
	int retlen;
	FILE *fp;
	
	if (argc < 4) {
		printf("$ ./testkanaconv <conv_flg> <char_type> <file>\n");
		exit(1);
	}

	conv_flg = atoi(argv[1]);
	char_type = atoi(argv[2]);

	if (stat(argv[3],&st) != 0) {
		printf("stat failure:%s\n",strerror(errno));
		exit(1);
	}
	in = malloc(st.st_size + 1);
	in[st.st_size] = 0;
	out = malloc(st.st_size + 1);
	out[st.st_size] = 0;

	if ((fp = fopen(argv[3],"r")) == NULL) {
		printf("fopen failure:%s\n",strerror(errno));
		exit(1);
	}
	if (st.st_size != fread(in,sizeof(char),st.st_size,fp)) {
		printf("fread failure:%s\n",strerror(errno));
		exit(1);
	}
	fclose(fp);
	retlen = st.st_size + 1;
	retval = kana_euc_convert(conv_flg, char_type, st.st_size+1, &retlen, in, out);
	printf("%s\n",out);
	printf("retval:%d\n",retval);

	return 0;
}
