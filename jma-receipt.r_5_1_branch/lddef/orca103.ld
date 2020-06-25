name	orca103;

arraysize	20;
textsize	400;

bind	""		"OpenCOBOL"	"ORCGX01";
bind	"X01"		"OpenCOBOL"	"ORCGX01";
bind	"X02"		"OpenCOBOL"	"ORCGX02";
bind	"X03"		"OpenCOBOL"	"ORCGX03";
bind	"X04"		"OpenCOBOL"	"ORCGX04";
bind	"X05"		"OpenCOBOL"	"ORCGX05";
bind	"X06"		"OpenCOBOL"	"ORCGX06";
bind	"X07"		"OpenCOBOL"	"ORCGX07";
bind	"X08"		"OpenCOBOL"	"ORCGX08";
bind	"X91"		"OpenCOBOL"	"ORCGX91";
bind	"X97"		"OpenCOBOL"	"ORCGX97";
bind	"X98"		"OpenCOBOL"	"ORCGX98";
bind	"X99"		"OpenCOBOL"	"ORCGX99";
bind	"X100"		"OpenCOBOL"	"ORCGX100";
bind	"XERR"		"OpenCOBOL"	"ORCGXERR";
bind	"XID1"		"OpenCOBOL"	"ORCGXID1";
bind	"XID2"		"OpenCOBOL"	"ORCGXID2";

multiplex_group	orca103;


data	{
	spa	orcaspa;
	window	{
		X01;
		X02;
		X03;
		X04;
		X05;
		X06;
		X07;
		X08;
		X91;
		X97;
		X98;
		X99;
		X100;
		XERR;
		XID1;
		XID2;
	};
};

db	{
	tbl_syskanri;
	tbl_byomei;
	tbl_chk;
	tbl_inputcd;
	tbl_ptbyomei;
	tbl_tensu;
	tbl_userbyomei;
	tbl_jobkanri;
	tbl_chksnd;
	tbl_chktrd;
	tbl_tekioubyomei;
	tbl_chk005;
	tbl_inputset;
	tbl_srycdchg;
	tbl_dbcatalog;
	tbl_prtdata;
	tbl_prtkanri;
	tbl_tekiou_disease;
	tbl_material_s;
	tbl_chk_master;
	tbl_chk_user;
	tbl_spa_tmp;
};

#include "orcadbh.inc"
