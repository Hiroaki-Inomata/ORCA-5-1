name	orca13;

arraysize	20;
textsize	400;

bind	""		"OpenCOBOL"	"ORCGQ01";
bind	"Q01"		"OpenCOBOL"	"ORCGQ01";
bind	"Q02"		"OpenCOBOL"	"ORCGQ02";
bind	"Q02A"		"OpenCOBOL"	"ORCGQ02A";
bind	"Q02B"		"OpenCOBOL"	"ORCGQ02B";
bind	"Q03"		"OpenCOBOL"	"ORCGQ03";
bind	"Q04"		"OpenCOBOL"	"ORCGQ04";
bind	"Q05"		"OpenCOBOL"	"ORCGQ05";
bind	"Q96"		"OpenCOBOL"	"ORCGQ96";
bind	"Q97"		"OpenCOBOL"	"ORCGQ97";
bind	"Q98"		"OpenCOBOL"	"ORCGQ98";
bind	"Q99"		"OpenCOBOL"	"ORCGQ99";
bind	"Q100"		"OpenCOBOL"	"ORCGQ100";
bind	"QERR"		"OpenCOBOL"	"ORCGQERR";
bind	"QID1"		"OpenCOBOL"	"ORCGQID1";
bindapi "findv3"    "OpenCOBOL"	"ORCGQAPI01"; 
bindapi "findinfv3" "OpenCOBOL"	"ORCGQAPI02"; 
bindapi "foundv3"   "OpenCOBOL"	"ORCGQAPI03"; 

multiplex_group	orca13;

data	{
	spa	orcaspa;
	window	{
		Q01;
		Q02;
		Q02A;
		Q02B;
		Q03;
		Q04;
		Q05;
		Q96;
		Q97;
		Q98;
		Q99;
		Q100;
        QERR;
        QID1;
        findv3;
        findinfv3;
        foundv3;
	};
};

db "monblob" {
 monblob;
};

db	{
  tbl_srysrh;
  tbl_skysrh;
  tbl_rrksrh;
  tbl_ptsrh;
  tbl_jobkanri;
  tbl_shoukaikanri;
  tbl_syskanri;
  tbl_hknnum;
  tbl_userbyomei;
  tbl_adrs;
  tbl_tensu;
  tbl_inputcd;
  tbl_byomei;
  tbl_shoukaidata;
  tbl_spa_tmp;
  tbl_file_info;
  tbl_ptnyuinrrk;
  tbl_jyurrk;
  tbl_srykarrk;
  tbl_hkncombi;
  tbl_ptinf;
  tbl_ptnum_public;
  tbl_sysuser;
  tbl_sysbase;
  tbl_passwd;
  tbl_dbcatalog;
  tbl_api_para;
  tbl_pthkninf;
  tbl_ptkohinf;
  tbl_ptrsiinf;
  tbl_tsyrrk;
  tbl_tnkrrk;
};

db "xml2" {
 xml_findv3req;
 xml_findv3res;
 xml_findinfv3req;
 xml_findinfv3res;
 xml_foundv3req;
 xml_foundv3res;
};


/* #include "orcadb.inc" */
#include "orcadbh.inc"


