name	api21;

arraysize	20;
textsize	400;

bindapi	"medicalmodv2"	"OpenCOBOL"	"ORAPI021S1V2";
bindapi	"medicalmodv31"	"OpenCOBOL"	"ORAPI021S1V3";
bindapi	"medicalmodv32"	"OpenCOBOL"	"ORAPI021S2V3";
bindapi	"medicalmodv33"	"OpenCOBOL"	"ORAPI021S3V3";
bindapi	"medicalmodv34"	"OpenCOBOL"	"ORAPI021S4V3";
bindapi	"claimreceivev2"	"OpenCOBOL"	"ORAPICLAIM21S1V2";
bindapi	"medicalmodv35"	"OpenCOBOL"	"ORAPI021S5V3";
bindapi	"medicalmodv36"	"OpenCOBOL"	"ORAPI021S6V3";
bindapi	"tmedicalmodv2"	"OpenCOBOL"	"ORAPI021S2V2";
bindapi	"medicalmodv37"	"OpenCOBOL"	"ORAPI021S7V3";
bindapi	"medicalmodav31"	"OpenCOBOL"	"ORAPI021NS1V3";
bindapi	"medicalmodav32"	"OpenCOBOL"	"ORAPI021NS2V3";
bindapi	"medicalmodav33"	"OpenCOBOL"	"ORAPI021NS3V3";
bindapi	"medicalmodav34"	"OpenCOBOL"	"ORAPI021NS4V3";
bindapi	"medicalmodv23"	"OpenCOBOL"	"ORAPI021S3V2";
bindapi	"medicalmodav35"	"OpenCOBOL"	"ORAPI021NS5V3";

multiplex_group	api21;


data	{
	spa	orcaspa;
	window	{
		medicalmodv2;
		medicalmodv31;
		medicalmodv32;
		medicalmodv33;
		medicalmodv34;
		claimreceivev2;
		medicalmodv35;
		medicalmodv36;
		tmedicalmodv2;
		medicalmodv37;
		medicalmodav31;
		medicalmodav32;
		medicalmodav33;
		medicalmodav34;
		medicalmodv23;
		medicalmodav35;
	};
};
db "xml2" {
 xml_medicalv2req;
 xml_medicalv2res;
 xml_medicalv3req1;
 xml_medicalv3res1;
 xml_medicalv3req2;
 xml_medicalv3res2;
 xml_medicalv3req3;
 xml_medicalv3res3;
 xml_medicalv3req4;
 xml_medicalv3res4;
 xml_claim_receivev2req;
 xml_claim_receivev2res;
 xml_medicalv3req5;
 xml_medicalv3res5;
 xml_medicalv3req6;
 xml_medicalv3res6;
 fm_medicine_notebook;
 fm_prescription;
 fm_medicine_information;
 fm_appointment_form;
 fm_statement;
 fm_invoice_receipt;
 xml_tmedicalmodv2req;
 xml_tmedicalmodv2res;
 xml_medicalv3req7;
 xml_medicalv3res7;
 xml_medicalav3req1;
 xml_medicalav3res1;
 xml_medicalav3req2;
 xml_medicalav3res2;
 xml_medicalav3req3;
 xml_medicalav3res3;
 xml_medicalav3req4;
 xml_medicalav3res4;
 fm_prescription_h;
 fm_order_injection;
 fm_order_direction;
 xml_medicalv2req3;
 xml_medicalv2res3;
 xml_medicalav3req5;
 xml_medicalav3res5;
};

#include "orcadb.inc"
#include "orcadbh.inc"

db "push" {
  push_print001;
};
db "monblob" {
 monblob;
};
