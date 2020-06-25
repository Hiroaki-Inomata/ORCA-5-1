name	orca61;

arraysize	20;
textsize	400;

bind	""		"OpenCOBOL"	"ORCGKT01";
bind	"KT01"		"OpenCOBOL"	"ORCGKT01";
bind	"KT011"		"OpenCOBOL"	"ORCGKT011";
bind	"KT012"		"OpenCOBOL"	"ORCGKT012";
bind	"KT013"		"OpenCOBOL"	"ORCGKT013";
bind	"KT02"		"OpenCOBOL"	"ORCGKT02";
bind	"KT03"		"OpenCOBOL"	"ORCGKT03";
bind	"KT98"		"OpenCOBOL"	"ORCGKT98";
bind	"KTERR"		"OpenCOBOL"	"ORCGKTERR";
bind	"KTERR2"	"OpenCOBOL"	"ORCGKTERR2";
bind	"KTID1"		"OpenCOBOL"	"ORCGKTID1";
bind	"KTID2"		"OpenCOBOL"	"ORCGKTID2";

multiplex_group	orca61;


data	{
	spa	orcaspa;
	window	{
		KT01;
		KT011;
		KT012;
		KT013;
		KT02;
		KT03;
		KT98;
		KTERR;
		KTERR2;
		KTID1;
		KTID2;
	};
};

#include "orcadb.inc"
#include "orcadbh.inc"
