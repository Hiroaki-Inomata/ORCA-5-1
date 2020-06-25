name	orca34;

arraysize	20;
textsize	400;

bind	""		"OpenCOBOL"	"ORCGI51";
bind	"I51"		"OpenCOBOL"	"ORCGI51";
bind	"I5ERR"		"OpenCOBOL"	"ORCGI5ERR";
bind	"I5ID1"		"OpenCOBOL"	"ORCGI5ID1";

multiplex_group	orca34;


data	{
	spa	orcaspa;
	window	{
		I51;
		I5ERR;
		I5ID1;
	};
};

db "xml2" {
 fm_invoice_receipt_h;
 fm_statement_h;
};

#include "orcadb.inc"
#include "orcadbh.inc"
