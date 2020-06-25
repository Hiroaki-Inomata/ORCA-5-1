name	orca14;

arraysize	20;
textsize	400;

bind	""		"OpenCOBOL"	"ORCGY01";
bind	"Y01"		"OpenCOBOL"	"ORCGY01";
bind	"Y011"		"OpenCOBOL"	"ORCGY011";
bind	"Y012"		"OpenCOBOL"	"ORCGY012";
bind	"Y03"		"OpenCOBOL"	"ORCGY03";
bind	"Y04"		"OpenCOBOL"	"ORCGY04";
bind	"Y05"		"OpenCOBOL"	"ORCGY05";
bind	"YERR"		"OpenCOBOL"	"ORCGYERR";
bind	"YID1"		"OpenCOBOL"	"ORCGYID1";
bindapi "appointmodv2"	"OpenCOBOL"	"ORAPI014S1V2";

multiplex_group	orca14;


data	{
	spa	orcaspa;
	window	{
		Y01;
		Y011;
		Y012;
		Y03;
		Y04;
		Y05;
		YERR;
		YID1;
		appointmodv2;
	};
};


db "xml2" {
 xml_appointv2req;
 xml_appointv2res;
 fm_appointment_form;
 fm_appointment_list;
 fm_appointment_list_patient;
};

#include "orcadb.inc"
#include "orcadbh.inc"

