--                                    --
--  患者情報（プレビュー用）テーブルの変更 --
--                                    --
-- 項目の追加                         --
--     入金方法 NYUKIN-HOHO           --
--                                    --
-- Update Date : 2005/09/14           --
--                                    --

-- TBL_PTINF_PRV 削除 --

DROP TABLE tbl_ptinf_prv ;

-- TBL_PTINF_PRV 作成 --

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
	"nyukin_hoho" character(2),
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

CREATE INDEX idx_ptinf_prv_kananame ON tbl_ptinf_prv USING btree (kananame);

CREATE INDEX idx_ptinf_prv_name ON tbl_ptinf_prv USING btree (name);
