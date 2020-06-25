#if 0
#!/bin/bash
src=$0
obj=${src%.*}
gcc -g -Wl,--no-as-needed `pkg-config --cflags --libs uuid` -o $obj $src
$obj
exit
#endif

#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>
#include	<uuid/uuid.h>

/* 窓口関数 */
extern void orcuidset(int hWnd, char *arg) ;

/* 窓口関数で使用する形の構造体形式  */
struct str_chardata_contact
{
	char	retcd[2] ;
	char	str_uid[36] ;
};

void orcuidset(int hWhd, char *arg)
{
	struct str_chardata_contact	*lt_contact ;
	uuid_t u;

	lt_contact = (struct str_chardata_contact *)arg ;
	uuid_generate(u);
	uuid_unparse(u,lt_contact->str_uid);

	lt_contact->retcd[0] = '0' ;
	lt_contact->retcd[1] = '0' ;
	return ;
}

int main(void)
{
	char lt_contact[256];

	memset(lt_contact,0,sizeof(lt_contact));
	orcuidset(0, lt_contact) ;
	printf("ret = %s\n",lt_contact);
}
