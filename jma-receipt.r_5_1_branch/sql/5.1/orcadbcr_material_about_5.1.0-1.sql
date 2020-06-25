\set ON_ERROR_STOP
--
-- 特材品目・算定データベース／在宅・調剤
-- Create Date : 2017/03/18
--

CREATE SCHEMA IF NOT EXISTS
    master
;

-- 品目情報

CREATE TABLE master.tbl_material_h_m (
KANRINO    bigint default 0,
JANCD      character varying(13),
SNAMECD    smallint default 0,
KIKAKUCD   smallint default 0,
SRYCD      character (9),
COMPANYCD1 smallint default 0,
COMPANYCD2 smallint default 0,
DOCKANRICD character varying(30),
KINOKBNNO  character varying(30),
CHGYMD     character varying(8),
CREYMD     character varying(8),
UPYMD      character varying(8),
UPHMS      character varying(6)
);

ALTER TABLE ONLY master.tbl_material_h_m
  ADD CONSTRAINT tbl_material_h_m_primary_key PRIMARY KEY (KANRINO);

-- 品目情報／製造販売業者

CREATE TABLE master.tbl_material_h_c (
COMPANYCD smallint default 0,
NAME      character varying(100),
CREYMD    character varying(8),
UPYMD     character varying(8),
UPHMS     character varying(6)
);

ALTER TABLE ONLY master.tbl_material_h_c
  ADD CONSTRAINT tbl_material_h_c_primary_key PRIMARY KEY (COMPANYCD);

-- 品目情報／規格

CREATE TABLE master.tbl_material_h_k (
KIKAKUCD smallint default 0,
KIKAKU   character varying(120),
CREYMD   character varying(8),
UPYMD    character varying(8),
UPHMS    character varying(6)
);

ALTER TABLE ONLY master.tbl_material_h_k
  ADD CONSTRAINT tbl_material_h_k_primary_key PRIMARY KEY (KIKAKUCD);

-- 品目情報／製品名

CREATE TABLE master.tbl_material_h_n (
SNAMECD smallint default 0,
SNAME   character varying(200),
CREYMD  character varying(8),
UPYMD   character varying(8),
UPHMS   character varying(6)
);

ALTER TABLE ONLY master.tbl_material_h_n
  ADD CONSTRAINT tbl_material_h_n_primary_key PRIMARY KEY (SNAMECD);

CREATE INDEX idx_material_h_n_sname ON master.tbl_material_h_n USING btree (sname);

-- 関連手技情報

CREATE TABLE master.tbl_material_k (
SRYCD   character(9) NOT NULL,
RENNUM1 smallint default 0,
RENNUM2 smallint default 0,
SCD     character varying(9) NOT NULL,
CREYMD  character varying(8),
UPYMD   character varying(8),
UPHMS   character varying(6)
);

ALTER TABLE ONLY master.tbl_material_k
  ADD CONSTRAINT tbl_material_k_primary_key PRIMARY KEY (SRYCD, RENNUM1, RENNUM2);

-- 適応疾患情報

CREATE TABLE master.tbl_material_s (
SRYCD     character(9) NOT NULL,
RENNUM1   smallint default 0,
RENNUM2   smallint default 0,
BYOMEI    character varying(200),
BYOMEICD  character varying(7) NOT NULL,
BYOMEIKBN smallint default 0,
CREYMD    character varying(8),
UPYMD     character varying(8),
UPHMS     character varying(6)
);

ALTER TABLE ONLY master.tbl_material_s
  ADD CONSTRAINT tbl_material_s_primary_key PRIMARY KEY (SRYCD, RENNUM1, RENNUM2);

-- 品目情報ビュー

CREATE VIEW
    tbl_material_h
AS
(
SELECT
  h.KANRINO,
  h.JANCD,
  n.SNAME,
  k.KIKAKU,
  h.SRYCD,
  c1.NAME AS COMPANY1,
  c2.NAME AS COMPANY2,
  h.DOCKANRICD,
  h.KINOKBNNO,
  h.CHGYMD,
  h.CREYMD,
  h.UPYMD,
  h.UPHMS
FROM
  master.tbl_material_h_m AS h
    INNER JOIN master.tbl_material_h_n AS n  ON (h.snamecd = n.snamecd)
    INNER JOIN master.tbl_material_h_k AS k  ON (h.kikakucd = k.kikakucd)
    INNER JOIN master.tbl_material_h_c AS c1 ON (h.companycd1 = c1.companycd)
    INNER JOIN master.tbl_material_h_c AS c2 ON (h.companycd2 = c2.companycd)
ORDER BY
    kanrino
)
;

-- 関連手技情報ビュー

CREATE VIEW tbl_material_k AS SELECT * FROM master.tbl_material_k;

-- 適応疾患情報

CREATE VIEW tbl_material_s AS SELECT * FROM master.tbl_material_s;
