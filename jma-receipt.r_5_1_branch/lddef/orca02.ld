name	orca02;

arraysize	20;
textsize	400;

bind	""		"OpenCOBOL"	"ORCGXB01";
bind	"XB01"		"OpenCOBOL"	"ORCGXB01";
bind	"XBER"   	"OpenCOBOL"	"ORCGXBERR";
bind	"XBID"   	"OpenCOBOL"	"ORCGXBID1";
bindapi	"jobmanagev3"	"OpenCOBOL"	"ORAPI002R1V3";

multiplex_group	orca02;


data	{
	spa	orcaspa;
	window	{
		XB01;
                XBER;
                XBID;
		jobmanagev3;
	};
};


db "xml2" {
 xml_job_managev3req;
 xml_job_managev3res;
};

db "monblob" {
 monblob;
};


db	{
	tbl_jobkanri;
	tbl_sysuser;
	tbl_sysbase;
	tbl_syskanri;
	tbl_dbcatalog;
};

/* #include "orcadb.inc" */
#include "orcadbh.inc"
