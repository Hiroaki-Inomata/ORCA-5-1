--
-- Selected TOC Entries:
--
--
-- TOC Entry ID 2 (OID 14273387)
--
-- Name: tbl_ptcom_prv Type: TABLE Owner: orca
--

CREATE TABLE "tbl_ptcom_prv" (
	"hospid" character(24) NOT NULL,
	"ptid" numeric(10,0) NOT NULL,
	"zainum" numeric(8,0) NOT NULL,
	"srycd" character(9) NOT NULL,
	"rennum" numeric(3,0) NOT NULL,
	"inputcoment" character varying(80),
	"inputchi1" character(8),
	"inputchi2" character(8),
	"inputchi3" character(8),
	"inputchi4" character(8),
	"termid" character varying(16),
	"opid" character varying(16),
	"creymd" character(8),
	"upymd" character(8),
	"uphms" character(6),
	Constraint "tbl_ptcom_prv_primary_key" Primary Key ("hospid", "ptid", "zainum", "srycd", "rennum")
);

