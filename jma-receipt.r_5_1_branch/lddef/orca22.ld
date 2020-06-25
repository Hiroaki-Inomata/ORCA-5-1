name	orca22;

arraysize	20;
textsize	400;

bind	""		"OpenCOBOL"	"ORCGC02";
bind	"C02"		"OpenCOBOL"	"ORCGC02";
bind	"C03"		"OpenCOBOL"	"ORCGC03";
bind	"C04"		"OpenCOBOL"	"ORCGC04";
bind	"C05"		"OpenCOBOL"	"ORCGC05";
bind	"C06"		"OpenCOBOL"	"ORCGC06";
bind	"C09"		"OpenCOBOL"	"ORCGC09";
bind	"C10"		"OpenCOBOL"	"ORCGC10";
bind	"C11"		"OpenCOBOL"	"ORCGC11";
bind	"C12"		"OpenCOBOL"	"ORCGC12";
bind	"C99"		"OpenCOBOL"	"ORCGC99";
bind	"CERR"		"OpenCOBOL"	"ORCGCERR";
bind	"CID1"		"OpenCOBOL"	"ORCGCID1";
bind	"CID2"		"OpenCOBOL"	"ORCGCID2";
bind	"CID3"		"OpenCOBOL"	"ORCGCID3";
bind	"CID4"		"OpenCOBOL"	"ORCGCID4";
bind	"CID5"		"OpenCOBOL"	"ORCGCID5";
bind	"CID6"		"OpenCOBOL"	"ORCGCID6";
bindapi	"diseasev2"	"OpenCOBOL"	"ORAPI022R1V3";
bindapi	"diseasev3"	"OpenCOBOL"	"ORAPI022R2V3";

multiplex_group	orca22;


data	{
	spa	orcaspa;
	window	{
		C02;
		C03;
		C04;
		C05;
		C06;
		C09;
		C10;
		C11;
		C12;
		C99;
		CERR;
		CID1;
		CID2;
		CID3;
		CID4;
		CID5;
		CID6;
		diseasev2;
		diseasev3;
	};
};

db "xml2" {
 xml_diseasev2req;
 xml_diseasev2res;
 xml_diseasev3req;
 xml_diseasev3res;
};

#include "orcadb.inc"
#include "orcadbh.inc"
