name	orca51;

arraysize	20;
textsize	400;

bind	""		"OpenCOBOL"	"ORCGL01";
bind	"L01"		"OpenCOBOL"	"ORCGL01";
bind	"L02"		"OpenCOBOL"	"ORCGL02";
bind	"L03"		"OpenCOBOL"	"ORCGL03";
bind	"L04"		"OpenCOBOL"	"ORCGL04";
bind	"L06"		"OpenCOBOL"	"ORCGL06";
bind	"L07"		"OpenCOBOL"	"ORCGL07";
bind	"L99"		"OpenCOBOL"	"ORCGL99";
bind	"L100"		"OpenCOBOL"	"ORCGL100";
bind	"LERR"		"OpenCOBOL"	"ORCGLERR";
bind	"LID1"		"OpenCOBOL"	"ORCGLID1";
bind	"LID2"		"OpenCOBOL"	"ORCGLID2";
bind	"G01"		"OpenCOBOL"	"ORCGG01";
bind	"G02"		"OpenCOBOL"	"ORCGG02";
bind	"G03"		"OpenCOBOL"	"ORCGG03";
bind	"G05"		"OpenCOBOL"	"ORCGG05";
bind	"G07"		"OpenCOBOL"	"ORCGG07";
bind	"G99"		"OpenCOBOL"	"ORCGG99";
bind	"G100"		"OpenCOBOL"	"ORCGG100";
bind	"GERR"		"OpenCOBOL"	"ORCGGERR";
bind	"GID1"		"OpenCOBOL"	"ORCGGID1";
bind	"GID2"		"OpenCOBOL"	"ORCGGID2";
bindapi	"diseasemasterlstv3"	"OpenCOBOL"	"ORAPI052R1V3";
bindapi	"medicationmasterlstv3"	"OpenCOBOL"	"ORAPI052R2V3";
bindapi	"stock1v2"		"OpenCOBOL"	"ORAPI052R3V3";
bindapi	"patientbasisallv3"	"OpenCOBOL"	"ORAPI052R4V3";
bindapi	"patientdiseaseallv3"	"OpenCOBOL"	"ORAPI052R5V3";
bindapi	"masterlastupdatev3"	"OpenCOBOL"	"ORAPI052R6V3";
bindapi	"patientmedicalallv3"	"OpenCOBOL"	"ORAPI052R7V3";
bindapi	"addressmasterlstv3"	"OpenCOBOL"	"ORAPI052R8V3";
bindapi	"tempmedicaladdv3"	"OpenCOBOL"	"ORAPI051R1V3";
bindapi	"statisticsformv3"	"OpenCOBOL"	"ORAPI051R2V3";
bindapi	"masterexportv3"	"OpenCOBOL"	"ORAPI052R9V3";

multiplex_group	orca51;


data	{
	spa	orcaspa;
	window	{
		L01;
		L02;
		L03;
		L04;
		L06;
		L07;
		L99;
		L100;
		LERR;
		LID1;
		LID2;
		G01;
		G02;
		G03;
		G05;
		G07;
		G99;
		G100;
		GERR;
		GID1;
		GID2;
		diseasemasterlstv3;
		medicationmasterlstv3;
		stock1v2;
		patientbasisallv3;
		patientdiseaseallv3;
		masterlastupdatev3;
		patientmedicalallv3;
		addressmasterlstv3;
		tempmedicaladdv3;
		statisticsformv3;
		masterexportv3;
	};
};


db "xml2" {
 xml_disease_masterlstv3req;
 xml_disease_masterlstv3res;
 xml_medication_masterlstv3req;
 xml_medication_masterlstv3res;
 xml_stock1v2req;
 xml_stock1v2res;
 xml_patient_basis_allv3req;
 xml_patient_basis_allv3res;
 xml_patient_disease_allv3req;
 xml_patient_disease_allv3res;
 xml_masterlastupdatev3req;
 xml_masterlastupdatev3res;
 xml_patient_medical_allv3req;
 xml_patient_medical_allv3res;
 xml_address_masterlstv3req;
 xml_address_masterlstv3res;
 xml_temp_medical_addv3req;
 xml_temp_medical_addv3res;
 xml_statistics_formv3req;
 xml_statistics_formv3res;
 xml_master_exportv3req;
 xml_master_exportv3res;
};

db "monblob" {
 monblob;
};

db	{
	tbl_syskanri;
	tbl_ptnum;
	tbl_ptinf;
	tbl_jobkanri;
        tbl_toukeimemo;
	tbl_hours;
	tbl_file_info; 
        tbl_btpara; 
        tbl_prtctrl; 
	tbl_dbcatalog;
	tbl_sysuser;
	tbl_sysbase;
	tbl_ptbyomei;
	tbl_tensu;
	tbl_byomei;
};

/* #include "orcadb.inc" */
#include "orcadbh.inc"
