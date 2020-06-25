name	orca33;

arraysize	20;
textsize	400;

bind	""		"OpenCOBOL"	"ORCGO01";
bind	"O01"		"OpenCOBOL"	"ORCGO01";
bind	"O02"		"OpenCOBOL"	"ORCGO02";
bind	"O03"		"OpenCOBOL"	"ORCGO03";
bind	"O04"		"OpenCOBOL"	"ORCGO04";
bind	"O05"		"OpenCOBOL"	"ORCGO05";
bind	"O99"		"OpenCOBOL"	"ORCGO99";
bind	"OERR"		"OpenCOBOL"	"ORCGOERR";
bind	"OID1"		"OpenCOBOL"	"ORCGOID1";
bind	"OID2"		"OpenCOBOL"	"ORCGOID2";
bind	"OID3"		"OpenCOBOL"	"ORCGOID3";

multiplex_group	orca33;



data	{
	spa	orcaspa;
	window	{
		O01;
		O02;
		O03;
		O04;
		O05;
		O99;
		OERR;
		OID1;
		OID2;
		OID3;
	};
};

db "xml2" {
 fm_invoice_receipt_h;
 fm_statement_h;
};

#include "orcadb.inc"
#include "orcadbh.inc"
