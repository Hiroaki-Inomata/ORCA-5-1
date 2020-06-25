name	orca23;

arraysize	20;
textsize	400;

bind	""		"OpenCOBOL"	"ORCGS02";
bind	"S02"		"OpenCOBOL"	"ORCGS02";
bind	"S03"		"OpenCOBOL"	"ORCGS03";
bind    "S04"           "OpenCOBOL"     "ORCGS04";
bind    "S06"           "OpenCOBOL"     "ORCGS06";
bind    "S07"           "OpenCOBOL"     "ORCGS07";
bind    "S08"           "OpenCOBOL"     "ORCGS08";
bind    "S09"           "OpenCOBOL"     "ORCGS09";
bind    "S10"           "OpenCOBOL"     "ORCGS10";
bind    "S99"           "OpenCOBOL"     "ORCGS99";
bind	"SERR"		"OpenCOBOL"	"ORCGSERR";
bind	"SID1"		"OpenCOBOL"	"ORCGSID1";
bind	"SID2"		"OpenCOBOL"	"ORCGSID2";
bind	"SID3"		"OpenCOBOL"	"ORCGSID3";
bind	"SID4"		"OpenCOBOL"	"ORCGSID4";
bindapi "incomev3"    "OpenCOBOL"	"ORCGSAPI01"; 

multiplex_group	orca23;



data	{
	spa	orcaspa;
	window	{
		S02;
		S03;
                S04;
                S06;
                S07;
                S08;
                S09;
                S10;
                S99;
                SERR;
                SID1;
                SID2;
                SID3;
                SID4;
                incomev3;
	};
};

db "monblob" {
 monblob;
};

db "xml2" {
 fm_statement;
 fm_invoice_receipt;
 xml_incomev3req;
 xml_incomev3req01;
 xml_incomev3res01;
 xml_incomev3req02;
 xml_incomev3res02;
 xml_incomev3res03;
 fm_invoice_receipt_h;
 fm_statement_h;
};

db "push" {
  push_print001;
  push_batch01;
};

#include "orcadb.inc"
#include "orcadbh.inc"
