name	orca29;

arraysize	20;
textsize	400;

bind	""		"OpenCOBOL"	"ORCGKA01";
bind	"KA01"		"OpenCOBOL"	"ORCGKA01";
bind	"KA02"		"OpenCOBOL"	"ORCGKA02";
bind	"KA021"		"OpenCOBOL"	"ORCGKA021";
bind	"KAERR"		"OpenCOBOL"	"ORCGKAERR";
bind	"KAID1"		"OpenCOBOL"	"ORCGKAID1";
bind	"KAID2"		"OpenCOBOL"	"ORCGKAID2";

multiplex_group	orca29;


data	{
	spa	orcaspa;
	window	{
		KA01;
		KA02;
		KA021;
		KAERR;
		KAID1;
		KAID2;
	};
};

db	{
	tbl_syskanri;
	tbl_hkncombi;
	tbl_hknnum;
	tbl_jyurrk;
	tbl_ptcom;
	tbl_ptnum;
	tbl_pthkninf;
	tbl_ptkohinf;
	tbl_ptinf;
	tbl_sryact;
	tbl_tensu;
        tbl_tensuplus;
	tbl_wksryact;
	tbl_ptrsiinf;
	tbl_yakujyo;
	tbl_monthlynum;
	tbl_jobkanri;
	tbl_byomei;
	tbl_ptbyomei;
	tbl_tsyrrk;
        tbl_tensuplus2;
        tbl_genericname;
	tbl_prgoption;
        tbl_ptnyuinrrk;
        tbl_ptkohftn;
	tbl_sryacct_main;
	tbl_sryacct_sub;
	tbl_dbcatalog;
	tbl_prtctrl;
	tbl_sryacctplus;
        tbl_generic_log;
        tbl_mnote_h;
        tbl_mnote_b;
        tbl_ptconf;
	tbl_tnkrrk;
	tbl_spa_tmp;
	tbl_med_image;
	tbl_list_doctor;
        tbl_generic_price;
	tbl_santei;
	tbl_prtdata;
	tbl_prtkanri;
	tbl_api_para;
        tbl_ptnum_public;
};
db "xml2" {
 fm_medicine_notebook;
 fm_prescription;
 fm_medicine_information;
 fm_medicine_notebook_h;
 fm_medicine_information_h;
};


/* #include "orcadb.inc" */
#include "orcadbh.inc"
