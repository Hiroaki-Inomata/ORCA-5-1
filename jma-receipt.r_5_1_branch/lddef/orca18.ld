name	orca18;

arraysize	20;
textsize	400;

bind	""		"OpenCOBOL"	"ORCGU01";
bind	"U01"		"OpenCOBOL"	"ORCGU01";
bind	"U012"		"OpenCOBOL"	"ORCGU012";
bind	"UDR"		"OpenCOBOL"	"ORCGUDR";
bind	"U1ERR"		"OpenCOBOL"	"ORCGU1ERR";
bind	"U1ID1"		"OpenCOBOL"	"ORCGU1ID1";

multiplex_group	orca18;


data	{
	spa	orcaspa;
	window	{
		U01;
		U012;
		UDR;
                U1ERR;
                U1ID1;
	};
};

#include "orcadb.inc"
#include "orcadbh.inc"
