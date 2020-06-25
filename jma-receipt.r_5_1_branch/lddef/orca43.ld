name	orca43;

arraysize	20;
textsize	400;

bind	""		    "OpenCOBOL"	"ORCGH01";
bind	"H01"		"OpenCOBOL"	"ORCGH01";
bind	"H02"		"OpenCOBOL"	"ORCGH02";
bind	"H03"		"OpenCOBOL"	"ORCGH03";
bind	"H99"		"OpenCOBOL"	"ORCGH99";
bind	"HERR"		"OpenCOBOL"	"ORCGHERR";
bind	"HID1"		"OpenCOBOL"	"ORCGHID1";

multiplex_group	orca43;


data	{
	spa	orcaspa;
	window	{
		H01;
		H02;
		H03;
		H99;
		HERR;
		HID1;
	};
};

db	{
	tbl_syskanri;
	tbl_ptnum;
	tbl_labor_sio;
	tbl_lock;
	tbl_ldmap;
	tbl_monthlynum;
	tbl_seikyu_main;
	tbl_seikyu_koh;
	tbl_seikyu_etc;
	tbl_seikyu_rsi;
	tbl_kohsky;
	tbl_ptinf;
	tbl_ptkohinf;
	tbl_henrei_kanri;
	tbl_henrei_body;
	tbl_receden;
	tbl_jibai_prt;
};

/* #include "orcadb.inc" */
#include "orcadbh.inc"
