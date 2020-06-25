name	orca01;

arraysize	20;
textsize	400;

bind	""		"OpenCOBOL"	"ORCGXA01";
bind	"XA01"		"OpenCOBOL"	"ORCGXA01";
bind	"XA02"		"OpenCOBOL"	"ORCGXA02";
bind	"XA03"		"OpenCOBOL"	"ORCGXA03";
bind	"XA99"		"OpenCOBOL"	"ORCGXA99";
bind	"XAERR"		"OpenCOBOL"	"ORCGXAERR";
bind	"XAID1"		"OpenCOBOL"	"ORCGXAID1";
bind	"XAID2"		"OpenCOBOL"	"ORCGXAID2";
bindapi	"reprintv3"	"OpenCOBOL"	"ORAPI001R1V3";

multiplex_group	orca01;


data	{
	spa	orcaspa;
	window	{
		XA01;
		XA02;
		XA03;
		XA99;
		XAERR;
		XAID1;
		XAID2;
		reprintv3;
	};
};


db "xml2" {
 xml_reprintv3req;
 xml_reprintv3res;
};

db "monblob" {
 monblob;
};

db	{
	tbl_syskanri;
	tbl_jobkanri;
	tbl_prtkanri;
	tbl_prtdata;
	tbl_ldmap;
	tbl_ptinf;
	tbl_ptnum;
	tbl_prvkanri;
	tbl_sysuser;
	tbl_sysbase;
	tbl_dbcatalog;
};

/* #include "orcadb.inc" */
#include "orcadbh.inc"
