name	orca21;

arraysize	20;
textsize	400;

bind	""		"OpenCOBOL"	"ORCGK02";
bind	"K01"		"OpenCOBOL"	"ORCGK01";
bind	"K02"		"OpenCOBOL"	"ORCGK02";
bind	"K02N"		"OpenCOBOL"	"ORCGK02NYU";
bind	"K021"		"OpenCOBOL"	"ORCGK021";
bind	"K023"		"OpenCOBOL"	"ORCGK023";
bind	"K03"		"OpenCOBOL"	"ORCGK03";
bind	"K04"		"OpenCOBOL"	"ORCGK04";
bind	"K05"		"OpenCOBOL"	"ORCGK05";
bind	"K051"		"OpenCOBOL"	"ORCGK051";
bind	"K052"		"OpenCOBOL"	"ORCGK052";
bind	"K06"		"OpenCOBOL"	"ORCGK06";
bind	"K061"		"OpenCOBOL"	"ORCGK061";
bind	"K062"		"OpenCOBOL"	"ORCGK062";
bind	"K07"		"OpenCOBOL"	"ORCGK07";
bind	"K08"		"OpenCOBOL"	"ORCGK08";
bind	"K081"		"OpenCOBOL"	"ORCGK081";
bind	"K09"		"OpenCOBOL"	"ORCGK09";
bind	"K10"		"OpenCOBOL"	"ORCGK10";
bind	"K97"		"OpenCOBOL"	"ORCGK97";
bind	"K98"		"OpenCOBOL"	"ORCGK98";
bind	"K99"		"OpenCOBOL"	"ORCGK99";
bind	"K03X"		"OpenCOBOL"	"ORCGK03X";
bind	"KERR"		"OpenCOBOL"	"ORCGKERR";
bind	"KERR2"		"OpenCOBOL"	"ORCGKERR2";
bind	"KID1"		"OpenCOBOL"	"ORCGKID1";
bind	"KID2"		"OpenCOBOL"	"ORCGKID2";
bind	"KHELP"		"OpenCOBOL"	"ORCGKHELP";
bind	"K024"		"OpenCOBOL"	"ORCGK024";
bind	"K0241"		"OpenCOBOL"	"ORCGK0241";
bindapi "medicalsetv2"  "OpenCOBOL"	"ORAPI021SETV2"; 

multiplex_group	orca21;


data	{
	spa	orcaspa;
	window	{
		K01;
		K02;
		K02N;
		K021;
		K023;
		K03;
		K04;
                K05;
                K051;
                K052;
                K06;
                K061;
                K062;
		K07;
                K08;
                K081;
                K09;
                K10;
		K97;
		K98;
		K99;
		K03X;
                KERR;
                KERR2;
                KID1;
                KID2;
                KHELP;
                K024;
                K0241;
		medicalsetv2;
	};
};

db "xml2" {
 xml_medicalsetv2req;
 xml_medicalsetv2res;
 fm_medicine_notebook;
 fm_prescription;
 fm_medicine_information;
 fm_appointment_form;
 fm_statement;
 fm_invoice_receipt;
 fm_prescription_h;
 fm_order_injection;
 fm_order_direction;
};

#include "orcadb.inc"
#include "orcadbh.inc"
db "push" {
        push_medical01;
};

