name	orca42;

arraysize	20;
textsize	400;

bind	""		"OpenCOBOL"	"ORCGR03";
bind	"R01"		"OpenCOBOL"	"ORCGR01";
bind	"R02"		"OpenCOBOL"	"ORCGR02";
bind	"R03"		"OpenCOBOL"	"ORCGR03";
bind	"R04"		"OpenCOBOL"	"ORCGR04";
bind	"R05"		"OpenCOBOL"	"ORCGR05";
bind	"R06"		"OpenCOBOL"	"ORCGR06";
bind    "R96"		"OpenCOBOL"	"ORCGR96";
bind    "R97"		"OpenCOBOL"	"ORCGR97";
bind    "R98"		"OpenCOBOL"	"ORCGR98";
bind	"R99"		"OpenCOBOL"	"ORCGR99";
bind	"RERR"		"OpenCOBOL"	"ORCGRERR";
bind	"RID1"		"OpenCOBOL"	"ORCGRID1";
bind	"RID2"		"OpenCOBOL"	"ORCGRID2";
bind	"RID3"		"OpenCOBOL"	"ORCGRID3";
bind	"RID4"		"OpenCOBOL"	"ORCGRID4";
bindapi	"receiptmakev3"	"OpenCOBOL"	"ORAPI042R1V3";
bindapi	"receiptprintv3" "OpenCOBOL"	"ORAPI042R2V3";

multiplex_group	orca42;


data	{
	spa	orcaspa;
	window	{
		R01;
		R02;
		R03;
		R04;
		R05;
		R06;
		R96;
		R97;
		R98;
		R99;
		RERR;
		RID1;
		RID2;
		RID3;
		RID4;
		receiptmakev3;
		receiptprintv3;
	};
};

db "xml2" {
 xml_receipt_makev3req;
 xml_receipt_makev3res;
 xml_receipt_printv3req;
 xml_receipt_printv3res;
};

db "monblob" {
 monblob;
};

db	{
	tbl_btpara;
	tbl_hkncombi;
	tbl_hknnum;
	tbl_hknjainf;
	tbl_jobkanri;
	tbl_jyurrk;
	tbl_kohplus;
	tbl_kohsky;
	tbl_monthlynum;
	tbl_nyuinacct;
	tbl_nyuinact;
	tbl_ptinf;
	tbl_pthkninf;
	tbl_ptkohftn;
	tbl_ptkohinf;
	tbl_ptnum;
	tbl_ptnyuinrrk;
	tbl_ptrsiinf;
	tbl_receden;
	tbl_recekanri;
	tbl_receprt;
	tbl_seikyu_main;
	tbl_seikyu_koh;
	tbl_seikyu_etc;
	tbl_seikyu_rsi;
	tbl_senteicdchg;
	tbl_spa_tmp;
	tbl_sryact;
	tbl_syskanri;
	tbl_syuka;
	tbl_syunou_main;
	tbl_syunou_nyuin;
	tbl_tensu;
	tbl_tnkrrk;
	tbl_tsyrrk;
	query_receprt;
	tbl_sryacct_main;
	tbl_sryacct_sub;
	tbl_dbcatalog;
	tbl_ptkohftnetc;
	tbl_ryocom;
	tbl_pt_hdist_day;
	tbl_sysuser;
	tbl_sysbase;
};


/* #include "orcadb.inc" */
#include "orcadbh.inc"
