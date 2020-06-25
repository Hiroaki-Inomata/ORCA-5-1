name	orca03;

arraysize	20;
textsize	400;

bind	""		"OpenCOBOL"	"ORCGXC01";
bind	"XC01"		"OpenCOBOL"	"ORCGXC01";
bind	"XC02"		"OpenCOBOL"	"ORCGXC01";
bind	"XC99"		"OpenCOBOL"	"ORCGXC99";
bind	"XCERR"		"OpenCOBOL"	"ORCGXCERR";

multiplex_group	orca03;


data	{
	spa	orcaspa;
	window	{
		XC01;
		XC02;
		XC99;
		XCERR;
	};
};

db {
	tbl_syskanri;
	tbl_ptnum;
	tbl_receprt;
	tbl_prtdata;
	tbl_prtkanri;
	tbl_prvkanri;
	tbl_jobkanri;
};

#include "orcadbh.inc"
