name	orca31;

arraysize	20;
textsize	400;

bind	""				"OpenCOBOL"	"ORCGI01";
bind	"I01"			"OpenCOBOL"	"ORCGI01";
bind	"I02"			"OpenCOBOL"	"ORCGI02";
bind	"I03"			"OpenCOBOL"	"ORCGI03";
bind	"I04"			"OpenCOBOL"	"ORCGI04";
bind	"I05"			"OpenCOBOL"	"ORCGI05";
bind	"I06"			"OpenCOBOL"	"ORCGI06";
bind	"I0ERR"			"OpenCOBOL"	"ORCGI0ERR";
bind	"I0ID1"			"OpenCOBOL"	"ORCGI0ID1";
bind	"I0ID2"			"OpenCOBOL"	"ORCGI0ID2";
bind	"I041"			"OpenCOBOL"	"ORCGI041";
bindapi "hsptinfmodv2" 	"OpenCOBOL"	"ORCGI0API01";
bindapi "birthdeliveryv2" "OpenCOBOL"	"ORCGI0API02";
bindapi "hsacctmodv2" 	"OpenCOBOL"	"ORCGI4API02";
bindapi "hspmmv2" 	"OpenCOBOL"	"ORCGI4API03";

multiplex_group	orca31;


data	{
	spa	orcaspa;
	window	{
		I01;
		I02;
		I03;
		I04;
		I05;
		I06;
		I0ERR;
		I0ID1;
		I0ID2;
		I041;
		hsptinfmodv2;
		birthdeliveryv2;
		hsacctmodv2;
		hspmmv2;
	};
};

db "xml2" {
 xml_hsptinfmodv2req;
 xml_hsptinfmod01v2res;
 xml_hsptinfmod02v2res;
 xml_birthdeliveryv2req;
 xml_birthdeliveryv2res;
 fm_prescription;
 fm_discharge_certificate;
 fm_karte_no1_h;
 fm_invoice_receipt_h;
 fm_statement_h;
 xml_hsacctmodv2req;
 xml_hsacctmodv2res;
 xml_hspmmv2req;
 xml_hspmmv2res;
};

#include "orcadb.inc"
#include "orcadbh.inc"

db "push" {
 push_hospital_stay;
};

