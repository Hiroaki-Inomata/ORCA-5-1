--
-- Selected TOC Entries:
--
\connect - orca

--
-- TOC Entry ID 2 (OID 14273491)
--
-- Name: tbl_ptinf_prv Type: TABLE Owner: orca
--

CREATE TABLE "tbl_ptinf_prv" (
	"hospid" character(24) NOT NULL,
	"ptid" numeric(10,0) NOT NULL,
	"kananame" character varying(100),
	"name" character varying(100),
	"nickname" character varying(100),
	"sex" character(1),
	"birthday" character(8),
	"deathkbn" character(1),
	"home_post" character varying(7),
	"home_adrs" character varying(200),
	"home_banti" character varying(200),
	"home_tel1" character varying(15),
	"home_tel2" character varying(15),
	"setainusi" character varying(100),
	"zokugara" character varying(60),
	"job" character varying(40),
	"keitai_tel" character varying(15),
	"fax" character varying(15),
	"email" character varying(50),
	"discount" character(2),
	"discount_rate" character(2),
	"condition_1" character(2),
	"condition_2" character(2),
	"condition_3" character(2),
	"comment_1" character varying(200),
	"comment_2" character varying(200),
	"taboo_1" character varying(200),
	"taboo_2" character varying(200),
	"allergy_1" character varying(200),
	"allergy_2" character varying(200),
	"kansensyo_1" character varying(200),
	"kansensyo_2" character varying(200),
	"renraku_name" character varying(100),
	"renraku_zokugara" character varying(60),
	"renraku_post" character varying(7),
	"renraku_adrs" character varying(200),
	"renraku_banti" character varying(200),
	"renraku_tel1" character varying(15),
	"renraku_tel2" character varying(15),
	"office_name" character varying(100),
	"office_post" character varying(7),
	"office_adrs" character varying(200),
	"office_banti" character varying(200),
	"office_tel" character varying(15),
	"kisei_name" character varying(100),
	"kisei_post" character varying(7),
	"kisei_adrs" character varying(200),
	"kisei_banti" character varying(200),
	"kisei_tel" character varying(15),
	"tstptnumkbn" character(1),
	"maxzainum" numeric(8,0) DEFAULT 0,
	"rrknum" numeric(3,0) DEFAULT 0,
	"rrkedanum" numeric(3,0) DEFAULT 0,
	"termid" character varying(16),
	"opid" character varying(16),
	"creymd" character(8),
	"upymd" character(8),
	"uphms" character(6),
	Constraint "tbl_ptinf_prv_primary_key" Primary Key ("hospid", "ptid")
);

--
-- TOC Entry ID 3 (OID 16174974)
--
-- Name: "idx_ptinf_prv_kananame" Type: INDEX Owner: orca
--

CREATE INDEX idx_ptinf_prv_kananame ON tbl_ptinf_prv USING btree (kananame);

--
-- TOC Entry ID 4 (OID 16174975)
--
-- Name: "idx_ptinf_prv_name" Type: INDEX Owner: orca
--

CREATE INDEX idx_ptinf_prv_name ON tbl_ptinf_prv USING btree (name);

