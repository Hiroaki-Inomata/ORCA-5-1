name	orca12;

arraysize	20;
textsize	400;

bind	""		"OpenCOBOL"	"ORCGP02";
bind	"P02"		"OpenCOBOL"	"ORCGP02";
bind	"P02A"		"OpenCOBOL"	"ORCGP02A";
bind	"P02D"		"OpenCOBOL"	"ORCGP02D";
bind	"P02E"		"OpenCOBOL"	"ORCGP02E";
bind	"P02F"		"OpenCOBOL"	"ORCGP02F";
bind	"P014"		"OpenCOBOL"	"ORCGP014";
bind	"P015"		"OpenCOBOL"	"ORCGP015";
bind	"P98"		"OpenCOBOL"	"ORCGP98";
bind	"P981"		"OpenCOBOL"	"ORCGP981";
bind	"P0201"		"OpenCOBOL"	"ORCGP0201";
bind	"P982"		"OpenCOBOL"	"ORCGP982";
bind	"P100"		"OpenCOBOL"	"ORCGP100";
bind	"P02X"		"OpenCOBOL"	"ORCGP02X";
bind	"P02S"		"OpenCOBOL"	"ORCGP02S";
bind	"P02Q"		"OpenCOBOL"	"ORCGP02Q";
bind	"P02Q1"		"OpenCOBOL"	"ORCGP02Q1";
bind	"P02Y"		"OpenCOBOL"	"ORCGP02Y";
bind	"P02Y1"		"OpenCOBOL"	"ORCGP02Y1";
bind	"P02G"		"OpenCOBOL"	"ORCGP02G";
bind	"P02G2"		"OpenCOBOL"	"ORCGP02G2";
bind	"P02K"		"OpenCOBOL"	"ORCGP02K";
bind	"P02H"		"OpenCOBOL"	"ORCGP02H";
bind	"P02T"		"OpenCOBOL"	"ORCGP02T";
bind	"PERR"		"OpenCOBOL"	"ORCGPERR";
bind	"PID1"		"OpenCOBOL"	"ORCGPID1";
bindapi "patientmodv2"  "OpenCOBOL"	"ORAPI012S1V2"; 
bindapi "patientmodv31" "OpenCOBOL"	"ORAPI012S1V3"; 
bindapi "patientmodv32" "OpenCOBOL"	"ORAPI012S2V3"; 
bindapi "patientmodv33" "OpenCOBOL"	"ORAPI012S3V3"; 
bindapi "patientmodv34" "OpenCOBOL"	"ORAPI012S4V3"; 
bindapi "patientmodv35" "OpenCOBOL"	"ORAPI012S5V3"; 
bindapi "patientmodv36" "OpenCOBOL"	"ORAPI012S6V3"; 
bindapi "patientmodv37" "OpenCOBOL"	"ORAPI012S7V3"; 


multiplex_group	orca12;


data	{
	spa	orcaspa;
	window	{
		P02;
		P02A;
		P02D;
		P02E;
		P02F;
		P014;
		P015;
		P98;
                P981;
		P0201;
                P982;
		P100;
		P02X;
		P02S;
		P02Q;
		P02Q1;
		P02Y;
		P02Y1;
		P02G;
		P02G2;
		P02K;
		P02H;
		P02T;
		PERR;
		PID1;
		patientmodv2;
		patientmodv31;
		patientmodv32;
		patientmodv33;
		patientmodv34;
		patientmodv35;
		patientmodv36;
		patientmodv37;
	};
};

/*
db	{
	tbl_syskanri;
	tbl_byomei;
	tbl_hkncombi;
	tbl_hknnum;
	tbl_adrs;
	tbl_jyurrk;
	tbl_srykarrk;
	tbl_ptbyomei;
	tbl_ptcom;
	tbl_ptnum;
	tbl_pthkninf;
	tbl_ptkohinf;
	tbl_ptinf;
	tbl_kyuseirrk;
	tbl_sryact;
	tbl_syumei;
	tbl_syunou_main;
	tbl_syunou_nyuin;
	tbl_tsyrrk;
	tbl_tensu;
	tbl_uketuke;
	tbl_yyk;
	tbl_sykrrk;
	tbl_tnkrrk;
	tbl_wksryact;
	tbl_santei;
	tbl_para;
	tbl_tensuplus;
	tbl_hknjainf;
	tbl_seikyu_main;
	tbl_seikyu_koh;
	tbl_seikyu_etc;
	tbl_seikyu_rsi;
	tbl_rececom;
	tbl_ptrsiinf;
	tbl_jinmei;
	tbl_monthlynum;
	tbl_ptnyuinrrk;
	tbl_pttainfuka;
	tbl_nyuinacct;
	tbl_nyuinact;
	tbl_ptteikirrk;
        tbl_kohsky;
	tbl_ldmap;
	tbl_lock;
	tbl_ptkohftn;
	tbl_receden;
        tbl_syuday;
        tbl_opecom;
        tbl_syutotal;
        tbl_toukeimemo;
	tbl_receupd;
	tbl_sryacctplus;
	tbl_sryacct_main;
	tbl_sryacct_sub;
};
*/

db "xml2" {
 xml_patientmodv2req;
 xml_patientmodv2res;
 xml_patientmodv3req1;
 xml_patientmodv3res1;
 xml_patientmodv3req2;
 xml_patientmodv3res2;
 xml_patientmodv3req3;
 xml_patientmodv3res3;
 xml_patientmodv3req4;
 xml_patientmodv3res4;
 xml_patientmodv3req5;
 xml_patientmodv3res5;
 xml_patientmodv3req6;
 xml_patientmodv3res6;
 xml_patientmodv3req7;
 xml_patientmodv3res7;
 fm_karte_no1;
 fm_prescription;
};

#include "orcadb.inc"
#include "orcadbh.inc"
db "push" {
        push_patient01;
};
