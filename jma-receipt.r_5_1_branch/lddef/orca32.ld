name	orca32;

arraysize	20;
textsize	400;

bind	""		"OpenCOBOL"	"ORCGI41";
bind	"I41"		"OpenCOBOL"	"ORCGI41";
bind	"I42"		"OpenCOBOL"	"ORCGI42";
bind	"I44"		"OpenCOBOL"	"ORCGI44";
bind	"I47"		"OpenCOBOL"	"ORCGI47";
bind	"I48"		"OpenCOBOL"	"ORCGI48";
bind	"I49"		"OpenCOBOL"	"ORCGI49";
bind	"I411"		"OpenCOBOL"	"ORCGI411";
bind	"I4ERR"		"OpenCOBOL"	"ORCGI4ERR";
bind	"I4ID1"		"OpenCOBOL"	"ORCGI4ID1";
bind	"I4ID2"		"OpenCOBOL"	"ORCGI4ID2";
bindapi	"hsptevalmodv2"	"OpenCOBOL"	"ORCGI4API01";

multiplex_group	orca32;

data 	{
	spa	orcaspa;
	window	{
		I41;
		I42;
		I44;
		I47;
		I48;
		I49;
		I411;
		I4ERR;
		I4ID1;
		I4ID2;
		hsptevalmodv2;
	};
};

db "xml2" {
 xml_hsptevalmodv2req;
 xml_hsptevalmodv2res;
};

#include "orcadb.inc"
#include "orcadbh.inc"
