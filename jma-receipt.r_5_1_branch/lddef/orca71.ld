name	orca71;

arraysize	20;
textsize	400;

bind	""		"OpenCOBOL"	"ORCGE01";
bind	"E01"		"OpenCOBOL"	"ORCGE01";
bind	"E02"		"OpenCOBOL"	"ORCGE02";
bind	"EERR"		"OpenCOBOL"	"ORCGEERR";
bind	"EID1"		"OpenCOBOL"	"ORCGEID1";
bind	"EID2"		"OpenCOBOL"	"ORCGEID2";
bind	"F01"		"OpenCOBOL"	"ORCGF01";
bind	"FERR"		"OpenCOBOL"	"ORCGFERR";
bind	"FID1"		"OpenCOBOL"	"ORCGFID1";
bind	"FID2"		"OpenCOBOL"	"ORCGFID2";

multiplex_group	orca71;



data	{
	spa	orcaspa;
	window	{
		E01;
		E02;
		EERR;
		EID1;
		EID2;
		F01;
		FERR;
		FID1;
		FID2;
	};
};

db	{
	tbl_jobkanri;
	tbl_jyurrk;
	tbl_jyurrk_prv;
	tbl_ptnum;
	tbl_syskanri;
};

/* #include "orcadb.inc" */
#include "orcadbh.inc"
