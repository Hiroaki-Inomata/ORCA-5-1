name	orca104;

arraysize	20;
textsize	400;

bind	""		"OpenCOBOL"	"ORCGV01";
bind	"V01"		"OpenCOBOL"	"ORCGV01";
bind	"V02"		"OpenCOBOL"	"ORCGV02";
bind	"V03"		"OpenCOBOL"	"ORCGV03";
bind	"VERR"		"OpenCOBOL"	"ORCGVERR";
bind	"VID1"		"OpenCOBOL"	"ORCGVID1";
bind	"N01"		"OpenCOBOL"	"ORCGN01";
bind	"NERR"		"OpenCOBOL"	"ORCGNERR";
bind	"NID1"		"OpenCOBOL"	"ORCGNID1";
bind	"N11"		"OpenCOBOL"	"ORCGN11";
bind	"N11ERR"	"OpenCOBOL"	"ORCGN11ERR";
bind	"N11ID1"	"OpenCOBOL"	"ORCGN11ID1";
bind	"N31"		"OpenCOBOL"	"ORCGN31";
bind	"N31ERR"	"OpenCOBOL"	"ORCGN31ERR";
bind	"N31ID1"	"OpenCOBOL"	"ORCGN31ID1";
bind	"N41"		"OpenCOBOL"	"ORCGN41";
bind	"N41ERR"	"OpenCOBOL"	"ORCGN41ERR";
bind	"N41ID1"	"OpenCOBOL"	"ORCGN41ID1";

multiplex_group	orca104;


data	{
	spa	orcaspa;
	window	{
		V01;
		V02;
		V03;
		VERR;
		VID1;
		N01;
		NERR;
		NID1;
		N11;
		N11ERR;
		N11ID1;
		N31;
		N31ERR;
		N31ID1;
		N41;
		N41ERR;
		N41ID1;
	};
};

db	{
	tbl_hknnum;
	tbl_kohplus;
	tbl_hknjainf;
	tbl_adrs;
	tbl_jinmei;
	tbl_jobkanri;
	tbl_syskanri;
	tbl_help;
	tbl_spa_tmp;
	tbl_adrs_master;
	tbl_adrs_user;
	tbl_hknjainf_master;
	tbl_hknjainf_user;
};

 db	"shell"	{
	shell;
};

/* #include "orcadb.inc" */
/* #include "orcadbh.inc" */
