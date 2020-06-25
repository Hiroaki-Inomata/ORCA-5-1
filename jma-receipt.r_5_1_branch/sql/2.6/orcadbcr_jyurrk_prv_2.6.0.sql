--
-- Selected TOC Entries:
--
\connect - orca

--
-- TOC Entry ID 2 (OID 14273620)
--
-- Name: tbl_jyurrk_prv Type: TABLE Owner: orca
--

CREATE TABLE "tbl_jyurrk_prv" (
	"hospid" character(24) NOT NULL,
	"ptid" numeric(10,0) NOT NULL,
	"nyugaikbn" character(1) NOT NULL,
	"sryka" character(2) NOT NULL,
	"sryymd" character(8) NOT NULL,
	"rennum" numeric(1,0) NOT NULL,
	"douji_rennum" numeric(1,0) NOT NULL,
	"kaikei_rennum" numeric(3,0) NOT NULL,
	"edanum" numeric(1,0) NOT NULL,
	"drcd" character(5),
	"hkncombinum" character(4),
	"hknkbn" character(1),
	"denpnum" numeric(7,0),
	"grp_denpnum" numeric(7,0) DEFAULT 0,
	"grp_rennum" numeric(2,0) DEFAULT 0,
	"syu_updkbn" character(1) DEFAULT ' ',
	"srykbn1" character(2),
	"srykbn2" character(2),
	"srykbn3" character(2),
	"srykbn4" character(2),
	"srykbn5" character(2),
	"srykbn6" character(2),
	"srykbn7" character(2),
	"srykbn8" character(2),
	"srykbn9" character(2),
	"srykbn10" character(2),
	"srykbn11" character(2),
	"zainum1" numeric(8,0),
	"zainum2" numeric(8,0),
	"zainum3" numeric(8,0),
	"zainum4" numeric(8,0),
	"zainum5" numeric(8,0),
	"zainum6" numeric(8,0),
	"zainum7" numeric(8,0),
	"zainum8" numeric(8,0),
	"zainum9" numeric(8,0),
	"zainum10" numeric(8,0),
	"zainum11" numeric(8,0),
	"zainum12" numeric(8,0),
	"zainum13" numeric(8,0),
	"zainum14" numeric(8,0),
	"zainum15" numeric(8,0),
	"termid" character varying(16),
	"opid" character varying(16),
	"creymd" character(8),
	"upymd" character(8),
	"uphms" character(6),
	Constraint "tbl_jyurrk_prv_primary_key" Primary Key ("hospid", "ptid", "nyugaikbn", "sryka", "sryymd", "rennum", "douji_rennum", "kaikei_rennum", "edanum")
);

--
-- TOC Entry ID 3 (OID 16174981)
--
-- Name: "idx_jyurrk_prv_sryymd" Type: INDEX Owner: orca
--

CREATE INDEX idx_jyurrk_prv_sryymd ON tbl_jyurrk_prv USING btree (hospid, sryymd, nyugaikbn);

--
-- TOC Entry ID 4 (OID 16174982)
--
-- Name: "idx_jyurrk_prv_upymd" Type: INDEX Owner: orca
--

CREATE INDEX idx_jyurrk_prv_upymd ON tbl_jyurrk_prv USING btree (hospid, upymd, nyugaikbn);

