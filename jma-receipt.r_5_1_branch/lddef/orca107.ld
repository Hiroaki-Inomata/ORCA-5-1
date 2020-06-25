name	orca107;

arraysize	20;
textsize	400;

bind	""		"OpenCOBOL"	"ORCGN21";
bind	"N21"		"OpenCOBOL"	"ORCGN21";
bind	"N22"		"OpenCOBOL"	"ORCGN22";
bind	"N23"		"OpenCOBOL"	"ORCGN23";
bind	"N29"		"OpenCOBOL"	"ORCGN29";
bind	"N21ERR"		"OpenCOBOL"	"ORCGN21ERR";
bind	"N21ID1"		"OpenCOBOL"	"ORCGN21ID1";
bind	"N21ID2"		"OpenCOBOL"	"ORCGN21ID2";
bind	"N21FIL"		"OpenCOBOL"	"ORCGN21FIL";

multiplex_group	orca107;


data	{
	spa	orcaspa;
	window	{
		N21;
		N22;
		N23;
		N29;
		N21ERR;
		N21ID1;
		N21ID2;
		N21FIL;
	};
};

db	{
	tbl_inputcd;
	tbl_tensu;
	tbl_yakujyo;
	tbl_yakujyo_temp;
	tbl_jobkanri;
	tbl_syskanri;
	tbl_med_image;
};

 db	"shell"	{
	shell;
};
db	"system" {
	metadb;
};
db      "blob_body" {
         blob;
};
/* #include "orcadb.inc" */
/* #include "orcadbh.inc" */
