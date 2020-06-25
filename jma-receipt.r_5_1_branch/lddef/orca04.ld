name	orca04;

arraysize	20;
textsize	400;

bind	""		"OpenCOBOL"	"ORCGXD01";
bind	"XD01"		"OpenCOBOL"	"ORCGXD01";
bind	"XDERR"		"OpenCOBOL"	"ORCGXDERR";
bind	"XDID1"		"OpenCOBOL"	"ORCGXDID1";

multiplex_group	orca04;


data	{
	spa	orcaspa;
	window	{
		XD01;
		XDERR;
		XDID1;
	};
};

db	{
	tbl_syskanri;
	tbl_hkncombi;
	tbl_hknnum;
	tbl_jyurrk;
	tbl_pthkninf;
	tbl_ptkohinf;
	tbl_ptrsiinf;
	tbl_ptinf;
	tbl_tnkrrk;
	tbl_tsyrrk;
	tbl_list_doctor;
};

/* #include "orcadb.inc" */
#include "orcadbh.inc"
