name	orca19;

arraysize	20;
textsize	400;

bind	""		"OpenCOBOL"	"ORCGP97";
bind	"P97"		"OpenCOBOL"	"ORCGP97";
bind	"P97ERR" 	"OpenCOBOL"	"ORCGP97ERR";

multiplex_group	orca19;


data	{
	spa	orcaspa;
	window	{
		P97;
                P97ERR;
	};
};

db	{
	tbl_syskanri;
	tbl_hkncombi;
	tbl_jyurrk;
	tbl_srykarrk;
	tbl_ptnum;
	tbl_ptinf;
	tbl_ptnyuinrrk;
	tbl_lock;
	tbl_ldmap;
};

/* #include "orcadb.inc" */
#include "orcadbh.inc"
