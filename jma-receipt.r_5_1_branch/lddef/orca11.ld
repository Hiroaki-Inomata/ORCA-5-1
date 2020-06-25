name	orca11;

arraysize	20;
textsize	400;

bind	""		"OpenCOBOL"	"ORCGU02";
bind	"U02"		"OpenCOBOL"	"ORCGU02";
bind	"U02Q"		"OpenCOBOL"	"ORCGU02Q";
bind	"U02X"		"OpenCOBOL"	"ORCGU02X";
bind	"U2ERR"		"OpenCOBOL"	"ORCGU2ERR";
bind	"U2ID1"		"OpenCOBOL"	"ORCGU2ID1";
bindapi	"acceptmodv2"	"OpenCOBOL"	"ORAPI011S1V2";

multiplex_group	orca11;


data	{
	spa	orcaspa;
	window	{
		U02;
		U02Q;
		U02X;
		U2ERR;
		U2ID1;
		acceptmodv2;
	};
};

db "xml2" {
 xml_acceptv2req;
 xml_acceptv2res;
 fm_karte_no1;
 fm_prescription;
};
/*
db	{
	tbl_syskanri;
	tbl_hkncombi;
	tbl_hknnum;
	tbl_jyurrk;
	tbl_srykarrk;
	tbl_ptbyomei;
	tbl_ptcom;
	tbl_ptnum;
	tbl_pthkninf;
	tbl_ptkohinf;
	tbl_ptinf;
	tbl_sryact;
	tbl_tensu;
	tbl_uketuke;
	tbl_yyk;
	tbl_wksryact;
	tbl_hknjainf;
	tbl_ptrsiinf;
	tbl_ldmap;
	tbl_lock;
        tbl_ptkohftn;
	tbl_sryacct_main;
	tbl_sryacct_sub;
};
*/

#include "orcadb.inc"
#include "orcadbh.inc"
db "push" {
        push_accept02;
};
