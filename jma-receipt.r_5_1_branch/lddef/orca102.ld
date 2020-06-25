name	orca102;

arraysize	20;
textsize	400;

bind	""		"OpenCOBOL"	"ORCGZ01";
bind	"Z01"		"OpenCOBOL"	"ORCGZ01";
bind	"Z02"		"OpenCOBOL"	"ORCGZ02";
bind	"Z03"		"OpenCOBOL"	"ORCGZ03";
bind	"Z04"		"OpenCOBOL"	"ORCGZ04";
bind	"Z05"		"OpenCOBOL"	"ORCGZ05";
bind	"Z06"		"OpenCOBOL"	"ORCGZ06";
bind	"Z07"		"OpenCOBOL"	"ORCGZ07";
bind	"Z08"		"OpenCOBOL"	"ORCGZ08";
bind	"Z010"		"OpenCOBOL"	"ORCGZ010";
bind	"Z90"		"OpenCOBOL"	"ORCGZ90";
bind	"Z91"		"OpenCOBOL"	"ORCGZ91";
bind	"Z97"		"OpenCOBOL"	"ORCGZ97";
bind	"Z98"		"OpenCOBOL"	"ORCGZ98";
bind	"Z99"		"OpenCOBOL"	"ORCGZ99";
bind	"ZERR"		"OpenCOBOL"	"ORCGZERR";
bind	"ZID1"		"OpenCOBOL"	"ORCGZID1";
bind	"ZID2"		"OpenCOBOL"	"ORCGZID2";
bind	"ZID3"		"OpenCOBOL"	"ORCGZID3";
bind	"Z100"		"OpenCOBOL"	"ORCGZ100";
bindapi	"medicatonmodv2"	"OpenCOBOL"	"ORAPI102S1V2";

multiplex_group	orca102;


data	{
	spa	orcaspa;
	window	{
		Z01;
		Z02;
		Z03;
		Z04;
		Z05;
		Z06;
		Z07;
		Z08;
		Z010;
                Z90;
                Z91;
                Z97;
                Z98;  
		Z99;
		ZERR;
		ZID1;
		ZID2;
		ZID3;
		Z100;
		medicatonmodv2;
	};
};
db "xml2" {
 xml_medicationv2req;
 xml_medicationv2res;
};
db	{
	tbl_chk;
	tbl_chk005;
	tbl_chksnd;
	tbl_chktrd;
	tbl_dbcatalog;
	tbl_file_info;
	tbl_genericname;
	tbl_inputcd;
	tbl_inputset;
	tbl_jobkanri;
	tbl_lock;
	tbl_passwd;
	tbl_ptinf;
	tbl_ptkinki;
	tbl_ptsrh;
	tbl_rrksrh;
	tbl_seikyu_etc;
	tbl_seikyu_koh;
	tbl_seikyu_main;
	tbl_seikyu_rsi;
	tbl_skysrh;
	tbl_sryacct_main;
	tbl_sryacct_sub;
	tbl_srycdchg;
	tbl_srysrh;
	tbl_sysbase;
	tbl_syskanri;
	tbl_sysuser;
	tbl_syumei;
	tbl_syunou_main;
	tbl_syunou_nyuin;
	tbl_tensu;
	tbl_tensu_master;
	tbl_tensu_price;
	tbl_tensu_user;
	tbl_tensuplus;
	tbl_tensuplus2;
	tbl_yakujyo;
	tbl_material_h;
	tbl_srycdchg_user;
	tbl_srycdchg_master;
};

/* #include "orcadb.inc" */
#include "orcadbh.inc"
