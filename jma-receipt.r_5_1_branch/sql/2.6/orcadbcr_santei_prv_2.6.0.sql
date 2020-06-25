--
-- Selected TOC Entries:
--
\connect - orca

--
-- TOC Entry ID 2 (OID 14273708)
--
-- Name: tbl_santei_prv Type: TABLE Owner: orca
--

CREATE TABLE "tbl_santei_prv" (
	"hospid" character(24) NOT NULL,
	"ptid" numeric(10,0) NOT NULL,
	"sryym" character(6) NOT NULL,
	"srycd" character(9) NOT NULL,
	"nyugaikbn" character(1) NOT NULL,
	"sryka" character(2) NOT NULL,
	"hkncombinum" numeric(4,0) DEFAULT 0 NOT NULL,
	"fsanteiymd" character(8),
	"msanteid" character(2),
	"msanteicnt" numeric(3,0) DEFAULT 0,
	"day1" numeric(3,0) DEFAULT 0,
	"day2" numeric(3,0) DEFAULT 0,
	"day3" numeric(3,0) DEFAULT 0,
	"day4" numeric(3,0) DEFAULT 0,
	"day5" numeric(3,0) DEFAULT 0,
	"day6" numeric(3,0) DEFAULT 0,
	"day7" numeric(3,0) DEFAULT 0,
	"day8" numeric(3,0) DEFAULT 0,
	"day9" numeric(3,0) DEFAULT 0,
	"day10" numeric(3,0) DEFAULT 0,
	"day11" numeric(3,0) DEFAULT 0,
	"day12" numeric(3,0) DEFAULT 0,
	"day13" numeric(3,0) DEFAULT 0,
	"day14" numeric(3,0) DEFAULT 0,
	"day15" numeric(3,0) DEFAULT 0,
	"day16" numeric(3,0) DEFAULT 0,
	"day17" numeric(3,0) DEFAULT 0,
	"day18" numeric(3,0) DEFAULT 0,
	"day19" numeric(3,0) DEFAULT 0,
	"day20" numeric(3,0) DEFAULT 0,
	"day21" numeric(3,0) DEFAULT 0,
	"day22" numeric(3,0) DEFAULT 0,
	"day23" numeric(3,0) DEFAULT 0,
	"day24" numeric(3,0) DEFAULT 0,
	"day25" numeric(3,0) DEFAULT 0,
	"day26" numeric(3,0) DEFAULT 0,
	"day27" numeric(3,0) DEFAULT 0,
	"day28" numeric(3,0) DEFAULT 0,
	"day29" numeric(3,0) DEFAULT 0,
	"day30" numeric(3,0) DEFAULT 0,
	"day31" numeric(3,0) DEFAULT 0,
	"msanteiflg" numeric(1,0) DEFAULT 0,
	"msanteiten" numeric(8,0) DEFAULT 0,
	"msanteitenkbn" numeric(1,0) DEFAULT 0,
	"termid" character varying(16),
	"opid" character varying(16),
	"creymd" character(8),
	"upymd" character(8),
	"uphms" character(6),
	Constraint "tbl_santei_prv_primary_key" Primary Key ("hospid", "ptid", "sryym", "srycd", "nyugaikbn", "sryka", "hkncombinum")
);

