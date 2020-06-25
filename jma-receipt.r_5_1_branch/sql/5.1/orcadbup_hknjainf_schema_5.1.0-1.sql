\set ON_ERROR_STOP

SET client_encoding = 'EUC_JP';

-- マスタ管理（一時）テーブルを作成する
CREATE TABLE IF NOT EXISTS tbl_mstkanri_org (
    hospnum numeric(2,0) NOT NULL,
    kanricd character(8) NOT NULL,
    dbrversion1 character varying(64),
    dbrversion2 character varying(64),
    termid character varying(32),
    opid character varying(16),
    creymd character(8),
    upymd character(8),
    uphms character(6),
    name character varying(64)
);

--ALTER TABLE ONLY tbl_mstkanri_org
--    ADD CONSTRAINT tbl_mstkanri_org_primary_key PRIMARY KEY (hospnum, kanricd);

-- マスタ管理レコードを移動 --
INSERT INTO
    tbl_mstkanri_org
SELECT
    *
FROM
    tbl_mstkanri_public
WHERE
    kanricd = 'ORCADB09'
;

DELETE FROM
    tbl_mstkanri_public
WHERE
    kanricd = 'ORCADB09'
;

-- 現行テーブル名を変更する

ALTER TABLE IF EXISTS tbl_hknjainf DROP CONSTRAINT tbl_hknjainf_primary_key;
ALTER TABLE IF EXISTS tbl_hknjainf RENAME TO tbl_hknjainf_org;
ALTER TABLE ONLY tbl_hknjainf_org
    ADD CONSTRAINT tbl_hknjainf_org_primary_key PRIMARY KEY (hospnum, hknjanum);

-- ユーザ用テーブル作成

CREATE TABLE IF NOT EXISTS tbl_hknjainf_user (
    hknjanum character varying(8) NOT NULL,
    hknjaname character varying(200),
    hknjaname_tan1 character varying(100),
    hknjaname_tan2 character varying(40),
    hknjaname_tan3 character varying(40),
    hknnum character varying(3),
    hon_gaikyurate smallint DEFAULT 0,
    hon_nyukyurate smallint DEFAULT 0,
    kzk_gaikyurate smallint DEFAULT 0,
    kzk_nyukyurate smallint DEFAULT 0,
    post character varying(7),
    adrs character varying(200),
    banti character varying(200),
    tel character varying(15),
    kigo character varying(80),
    idokbn character varying(1),
    idoymd character varying(8),
    termid character varying(16),
    opid character varying(16),
    creymd character varying(8),
    upymd character varying(8),
    uphms character varying(6),
    hospnum smallint NOT NULL
);

ALTER TABLE IF EXISTS ONLY tbl_hknjainf_user
    ADD CONSTRAINT tbl_hknjainf_user_primary_key PRIMARY KEY (hospnum, hknjanum);

-- マスター用テーブル作成

CREATE TABLE IF NOT EXISTS master.tbl_hknjainf_master (
    hknjanum character varying(8) NOT NULL,
    hknjaname character varying(200),
    hknjaname_tan1 character varying(100),
    hknjaname_tan2 character varying(40),
    hknjaname_tan3 character varying(40),
    hknnum character varying(3),
    hon_gaikyurate smallint DEFAULT 0,
    hon_nyukyurate smallint DEFAULT 0,
    kzk_gaikyurate smallint DEFAULT 0,
    kzk_nyukyurate smallint DEFAULT 0,
    post character varying(7),
    adrs character varying(200),
    banti character varying(200),
    tel character varying(15),
    kigo character varying(80),
    idokbn character varying(1),
    idoymd character varying(8),
    termid character varying(16),
    opid character varying(16),
    creymd character varying(8),
    upymd character varying(8),
    uphms character varying(6),
    hospnum smallint NOT NULL
);

ALTER TABLE IF EXISTS ONLY master.tbl_hknjainf_master
    ADD CONSTRAINT tbl_hknjainf_master_primary_key PRIMARY KEY (hospnum, hknjanum);

-- ビュー作成

DROP VIEW IF EXISTS
    tbl_hknjainf
;

DROP TABLE IF EXISTS
    view_hknjainf_master
;

CREATE VIEW
    view_hknjainf_master
AS
SELECT
    a.hknjanum,
    a.hknjaname,
    a.hknjaname_tan1,
    a.hknjaname_tan2,
    a.hknjaname_tan3,
    a.hknnum,
    a.hon_gaikyurate,
    a.hon_nyukyurate,
    a.kzk_gaikyurate,
    a.kzk_nyukyurate,
    a.post,
    a.adrs,
    a.banti,
    a.tel,
    a.kigo,
    a.idokbn,
    a.idoymd,
    a.termid,
    a.opid,
    a.creymd,
    a.upymd,
    a.uphms,
    b.hospnum::smallint
FROM
    master.tbl_hknjainf_master a
    CROSS JOIN tbl_sysbase b
;

CREATE VIEW
    tbl_hknjainf
AS
(
SELECT
    *
FROM
    tbl_hknjainf_user
UNION ALL
SELECT
    *
FROM
    view_hknjainf_master
WHERE
    NOT EXISTS (
        SELECT
            1
        FROM
            tbl_hknjainf_user
        WHERE
            ( view_hknjainf_master.hospnum = tbl_hknjainf_user.hospnum ) AND
            ( view_hknjainf_master.hknjanum = tbl_hknjainf_user.hknjanum )
    )
-- ORDER BY
);
