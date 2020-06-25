--
-- Selected TOC Entries:
--
\connect - orca

--
-- TOC Entry ID 2 (OID 14273408)
--
-- Name: tbl_syumei_prv Type: TABLE Owner: orca
--

CREATE TABLE "tbl_syumei_prv" (
	"hospid" character(24) NOT NULL,
	"nyugaikbn" character(1) NOT NULL,
	"ptid" numeric(10,0) NOT NULL,
	"denpnum" numeric(7,0) NOT NULL,
	"denpedanum" numeric(2,0) NOT NULL,
	"nyukinrennum" numeric(3,0) DEFAULT 0,
	"sryka" character(2),
	"meisaijoutaikbn" character(1),
	"sky_printymd" character(8),
	"sky_reprintymd" character(8),
	"ryosyu_printymd" character(8),
	"ryosyu_reprintymd" character(8),
	"skymoney" numeric(7,0) DEFAULT 0,
	"nyuhen_kbn" character(1),
	"nyuhen_money" numeric(7,0) DEFAULT 0,
	"nyuhen_ymd" character(8),
	"nyukin_hoho" character(2),
	"joutaikbn" character(1),
	"termid" character varying(16),
	"opid" character varying(16),
	"creymd" character(8),
	"upymd" character(8),
	"uphms" character(6),
	Constraint "tbl_syumei_prv_primary_key" Primary Key ("hospid", "nyugaikbn", "ptid", "denpnum", "denpedanum", "creymd")
);

