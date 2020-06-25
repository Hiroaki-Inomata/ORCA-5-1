name	orca00;

arraysize	20;
textsize	400;

bind	""		"OpenCOBOL"	"ORCGM00";
bind	"M00"		"OpenCOBOL"	"ORCGM00";
bind	"M01"		"OpenCOBOL"	"ORCGM01";
bind	"M01N"		"OpenCOBOL"	"ORCGM01N";
bind	"M02"		"OpenCOBOL"	"ORCGM02";
bind	"M95"		"OpenCOBOL"	"ORCGM95";
bind	"MERR"		"OpenCOBOL"	"ORCGMERR";
bind	"MID1"		"OpenCOBOL"	"ORCGMID1";
bind	"MVER"		"OpenCOBOL"	"ORCGMVER";
bind	"M98"		"OpenCOBOL"	"ORCGM98";
bind	"MUID"		"OpenCOBOL"	"ORCGMUID";
bind	"M99"		"OpenCOBOL"	"ORCGM99";
bind	"MID2"		"OpenCOBOL"	"ORCGMID2";
bind	"MDAS"		"OpenCOBOL"	"ORCGMDAS";
bind	"MLOG"		"OpenCOBOL"	"ORCGMLOG";
bind	"MINF"		"OpenCOBOL"	"ORCGMINF";
bindapi "print"         "OpenCOBOL"     "ORCGMPRT";

multiplex_group	orca00;


data	{
	spa	orcaspa;
	window	{
                M00;
		M01;
		M01N;
                M02;
                M95;
                MERR;
                MID1;
		MVER;
		M98;
		MUID;
		M99;
		MID2;
		MDAS;
		MLOG;
		MINF;
		print;
	};
};

db	{
	tbl_syskanri;
	tbl_jobkanri;
	tbl_dbkanri;
	tbl_ldmap;
	tbl_lock;
        tbl_pgkanri;
	tbl_joblog;
	tbl_sysbase;
	tbl_mstkanri;
	tbl_sysuser;
	tbl_adrs;
	tbl_infection;
	tbl_ptinf;
	tbl_byomei;
	tbl_dbcatalog;
	tbl_prtctrl;
	tbl_prtkanri;
	tbl_prtdata;
	tbl_das_conf;
	tbl_list_doctor;
	tbl_access_key;
};

/* #include "orcadb.inc" */
#include "orcadbh.inc"
