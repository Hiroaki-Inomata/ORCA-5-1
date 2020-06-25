--
-- Selected TOC Entries:
--
--
-- TOC Entry ID 2 (OID 14273402)
--
-- Name: tbl_srykarrk_prv Type: TABLE Owner: orca
--

CREATE TABLE "tbl_srykarrk_prv" (
	"hospid" character(24) NOT NULL,
	"ptid" numeric(10,0) NOT NULL,
	"sryka" character(2) NOT NULL,
	"syosinymd1" character(8),
	"syosinymd2" character(8),
	"lastymd" character(8),
	"termid" character varying(16),
	"opid" character varying(16),
	"creymd" character(8),
	"upymd" character(8),
	"uphms" character(6),
	Constraint "tbl_srykarrk_prv_primary_key" Primary Key ("hospid", "ptid", "sryka", "creymd")
);

