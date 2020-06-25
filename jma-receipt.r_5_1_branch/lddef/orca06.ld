name	orca06;

arraysize	20;
textsize	400;

bind	""		"OpenCOBOL"	"ORCGXF01";
bind	"XF01"		"OpenCOBOL"	"ORCGXF01";
bind	"XF011"		"OpenCOBOL"	"ORCGXF011";
bind	"XFERR"		"OpenCOBOL"	"ORCGXFERR";
bind	"XFID1"		"OpenCOBOL"	"ORCGXFID1";
bind	"XF01X"		"OpenCOBOL"	"ORCGXF01X";
bind	"XFID2"		"OpenCOBOL"	"ORCGXFID2";

multiplex_group	orca06;


data	{
	spa	orcaspa;
	window	{
		XF01;
		XF011;
		XFERR;
		XFID1;
		XF01X;
		XFID2;
	};
};

db	{
	tbl_syskanri;
	tbl_ptmemoinf;
        tbl_example;
        tbl_uketuke;
};

/* #include "orcadb.inc" */
#include "orcadbh.inc"
db "push" {
        push_accept02;
};
