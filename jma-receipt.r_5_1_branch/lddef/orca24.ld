name	orca24;

arraysize	20;
textsize	400;

bind	""		"OpenCOBOL"	"ORCGJ02";
bind	"J02"		"OpenCOBOL"	"ORCGJ02";
bind	"J021"		"OpenCOBOL"	"ORCGJ021";
bind	"J022"		"OpenCOBOL"	"ORCGJ022";
bind	"J025"		"OpenCOBOL"	"ORCGJ025";
bind	"J026"		"OpenCOBOL"	"ORCGJ026";
bind	"J03"		"OpenCOBOL"	"ORCGJ03";
bind	"J04"		"OpenCOBOL"	"ORCGJ04";
bind	"J041"		"OpenCOBOL"	"ORCGJ041";
bind	"J042"		"OpenCOBOL"	"ORCGJ042";
bind	"J98"		"OpenCOBOL"	"ORCGJ98";
bind	"J99"		"OpenCOBOL"	"ORCGJ99";
bind	"JERR"		"OpenCOBOL"	"ORCGJERR";
bind	"JERR2"		"OpenCOBOL"	"ORCGJERR2";
bind	"JID1"		"OpenCOBOL"	"ORCGJID1";
bind	"JID2"		"OpenCOBOL"	"ORCGJID2";

multiplex_group	orca24;



data	{
	spa	orcaspa;
	window	{
		J02;
		J021;
                J022;
                J025;
                J026;
		J03;
		J04;
		J041;
		J042;
		J98;
		J99;
                JERR;
                JERR2;
                JID1;
                JID2;
	};
};

db "xml2" {
 fm_karte_no3;
 fm_karte_no3_h;
};


#include "orcadb.inc"
#include "orcadbh.inc"
