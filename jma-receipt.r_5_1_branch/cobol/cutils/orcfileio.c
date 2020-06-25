/*
 orca
      uid set proc by using uuidgen command
      gcc -fPIC -shared orcfileio.c -o liborcfileio.so
*/

#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>

/* 窓口関数 */
extern void orcfileio(int hWnd, char *arg) ;

/* 窓口関数で使用する形の構造体形式  */
struct str_chardata_contact
{
	char	flg[1];
	char	filename[200];
	char	str_file[100000] ;
	char	retcd[2] ;
};

void orcfileio(int hWhd, char *arg)
{
#define SIZE 100000
#define N 1
	char	filename_buf[200];
	char	buff[100000] ;
	FILE	*fp;
	char	s[N][SIZE];
	int	i,i2,i3;
	struct	str_chardata_contact	*lt_contact ;
	lt_contact = (struct str_chardata_contact *)arg ;
	strncpy(filename_buf,lt_contact->filename, 200);
	for( i = 199 ; i >= 0 && filename_buf[i] == ' ' ; i-- )
	{
		filename_buf[i] = '\0';
	}
	if	(lt_contact->flg[0] == 'r'){
		if	((fp = fopen(filename_buf,"r"))  == NULL ){
//			printf("can not open file %s= \n",lt_contact->filename);
			lt_contact->retcd[0] = '-' ;
			lt_contact->retcd[1] = '1' ;
			return ;
		}
		for( i = 0 ; i < N ;i++)for(i2 = 0 ; i2 < SIZE ; i2++) s[i][i2] = ' '; 
//		for( i = 0 ; i < N ;i++)for(i2 = 0 ; i2 < SIZE ; i2++) s[i][i2] = '\0'; 
		fread(s,SIZE,N,fp);
		i3 = 0;
		for( i = 99999 ; i >= 0 && s[0][i] == '\0' ; i-- )
		{
			buff[i] = '\0';
//			++i3;
		}
//		printf("i=%d\n",i);
		i3 = i;
	//	strncpy(lt_contact->str_file, s[0], 70000) ;
		strncpy(lt_contact->str_file, s[0], i) ;
		lt_contact->retcd[0] = '0' ;
		lt_contact->retcd[1] = '0' ;
//		return ;

	}
	else if	(lt_contact->flg[0] == 'w'){
		if	((fp = fopen(filename_buf,"w"))  == NULL ){
//			printf("can not open file %s= \n",lt_contact->filename);
			lt_contact->retcd[0] = '-' ;
			lt_contact->retcd[1] = '1' ;
			return ;
		}
		strncpy(buff,lt_contact->str_file, 100000);
		for( i = 99999 ; i >= 0 && buff[i] == ' ' || '\0' ; i-- )
//		for( i = 99999 ; i >= 0 && buff[i] == '\0' ; i-- )
		{
			buff[i] = '\0';
//			++i3;
		}
		i3 = i;
		printf("i3=%d\n",i3);
		for( i = 0 ; i < N ;i++)for(i2 = 0 ; i2 < SIZE ; i2++) s[i][i2] = buff[i2]; 
//		fwrite(s,SIZE,N,fp);
		fwrite(s,i3,N,fp);
		lt_contact->retcd[0] = '0' ;
		lt_contact->retcd[1] = '0' ;
	}
	else	{
			lt_contact->retcd[0] = '-' ;
			lt_contact->retcd[1] = '2' ;
	}
	fclose(fp) ;

}
/*
int main(void)
{
	char	lt_contact[256] ;
	uidset( 0, lt_contact) ;
	printf("ret = %s\n",lt_contact);
}
*/
