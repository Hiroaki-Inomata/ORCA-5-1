name	orca41;

arraysize	20; textsize	400;

bind	""		"OpenCOBOL"	"ORCGD01";
bind	"D01"		"OpenCOBOL"	"ORCGD01";
bind	"D02"		"OpenCOBOL"	"ORCGD02";
bind	"D03"		"OpenCOBOL"	"ORCGD03";
bind	"D04"		"OpenCOBOL"	"ORCGD04";
bind	"D05"		"OpenCOBOL"	"ORCGD05";
bind	"D99"		"OpenCOBOL"	"ORCGD99";
bind	"DERR"		"OpenCOBOL"	"ORCGDERR";
bind	"DID1"		"OpenCOBOL"	"ORCGDID1";
bind	"DID2"		"OpenCOBOL"	"ORCGDID2";
bindapi "datacheckv3" "OpenCOBOL"	"ORCGDAPI01"; 

multiplex_group	orca41;



data	{
	spa	orcaspa;
	window	{
		D01;
		D02;
		D03;
		D04;
		D05;
		D99;
		DERR;
		DID1;
		DID2;
        datacheckv3;
	};
};

db "xml2" {
 xml_data_checkv3req;
 xml_data_checkv3res;
};

db "monblob" {
 monblob;
};

db	{
	tbl_sryacct_main;
	tbl_sryacct_sub;
	tbl_seikyu_main;
	tbl_seikyu_koh;
	tbl_seikyu_etc;
	tbl_seikyu_rsi;
	tbl_syunou_main;
	tbl_syunou_nyuin;
	tbl_nyuinacct;
	tbl_syskanri;
	tbl_srysrh;
	tbl_skysrh;
	tbl_rrksrh;
	tbl_ptsrh;
	tbl_jobkanri;
	tbl_lock;
	tbl_ldmap;
	tbl_ptnum;
	tbl_ptinf;
	tbl_ptnyuinrrk;
	tbl_sryact;
	tbl_nyuinact;
	tbl_hkncombi;
	tbl_jyurrk;
	tbl_pthkninf;
	tbl_ptrsiinf;
	tbl_hknnum;
	tbl_ptkohinf;
	tbl_chkreigai;
	tbl_tensu;
	tbl_ptbyomei;
	tbl_chk;
	tbl_chksnd;
	tbl_chktrd;
	tbl_chk004;
	tbl_chk005;
	tbl_inputcd;
	tbl_tekioubyomei;
	tbl_byomei ;
	tbl_etensu_3_1 ;
	tbl_etensu_3_2 ;
	tbl_etensu_3_3 ;
	tbl_etensu_5 ;
	tbl_nsrysrh ;
	tbl_nrrksrh ;
	tbl_tnkrrk;
	tbl_tsyrrk;
	tbl_spa_tmp;
	tbl_tekiou_disease;
    tbl_material_s;
    tbl_material_k;
	tbl_dbcatalog;
	tbl_sysuser;
	tbl_sysbase;
};

/* #include "orcadb.inc" */
#include "orcadbh.inc"
