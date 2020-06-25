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
    kanricd = 'ORCADB10'
;

DELETE FROM
    tbl_mstkanri_public
WHERE
    kanricd = 'ORCADB10'
;

-- 現行テーブル名を変更する

DROP INDEX IF EXISTS idx_adrs_post;
ALTER TABLE IF EXISTS tbl_adrs DROP CONSTRAINT tbl_adrs_primary_key;
ALTER TABLE IF EXISTS tbl_adrs RENAME TO tbl_adrs_org;
ALTER TABLE ONLY tbl_adrs_org
    ADD CONSTRAINT tbl_adrs_org_primary_key PRIMARY KEY (hospnum, lpubcd, post, rennum);
CREATE INDEX idx_adrs_org_post ON tbl_adrs_org USING btree (hospnum, post);

-- ユーザ用テーブル作成

CREATE TABLE IF NOT EXISTS tbl_adrs_user (
    lpubcd character varying(5) NOT NULL,
    post character varying(7) NOT NULL,
    rennum smallint NOT NULL,
    prefkana character varying(40),
    citykana character varying(200),
    townkana character varying(400),
    prefname character varying(20),
    cityname character varying(100),
    townname character varying(200),
    editadrs_kana character varying(640),
    editadrs_name character varying(320),
    towndivflg character varying(1) DEFAULT '0'::bpchar,
    manytownflg character varying(1) DEFAULT '0'::bpchar,
    hospnum smallint NOT NULL,
    townname2 character varying(1200)
);

ALTER TABLE IF EXISTS ONLY tbl_adrs_user
    ADD CONSTRAINT tbl_adrs_user_primary_key PRIMARY KEY (hospnum, lpubcd, post, rennum);
CREATE INDEX IF NOT EXISTS idx_adrs_user_post ON tbl_adrs_user USING btree (hospnum, post);

-- マスター用テーブル作成

CREATE TABLE IF NOT EXISTS master.tbl_adrs_master (
    lpubcd character varying(5) NOT NULL,
    post character varying(7) NOT NULL,
    rennum smallint NOT NULL,
    prefkana character varying(40),
    citykana character varying(200),
    townkana character varying(400),
    prefname character varying(20),
    cityname character varying(100),
    townname character varying(200),
    editadrs_kana character varying(640),
    editadrs_name character varying(320),
    towndivflg character varying(1) DEFAULT '0'::bpchar,
    manytownflg character varying(1) DEFAULT '0'::bpchar,
    hospnum smallint NOT NULL,
    townname2 character varying(1200)
);

ALTER TABLE IF EXISTS ONLY master.tbl_adrs_master
    ADD CONSTRAINT tbl_adrs_master_primary_key PRIMARY KEY (hospnum, lpubcd, post, rennum);
CREATE INDEX IF NOT EXISTS idx_adrs_master_post ON master.tbl_adrs_master USING btree (hospnum, post);

-- ビュー作成

DROP VIEW IF EXISTS
    tbl_adrs
;

DROP TABLE IF EXISTS
    view_adrs_master
;

CREATE VIEW
    view_adrs_master
AS
SELECT
    a.lpubcd,
    a.post,
    a.rennum,
    a.prefkana,
    a.citykana,
    a.townkana,
    a.prefname,
    a.cityname,
    a.townname,
    a.editadrs_kana,
    a.editadrs_name,
    a.towndivflg,
    a.manytownflg,
    b.hospnum::smallint,
    a.townname2
FROM
    master.tbl_adrs_master a
    CROSS JOIN tbl_sysbase b
;

CREATE VIEW
    tbl_adrs
AS
(
SELECT
    *
FROM
    tbl_adrs_user
UNION ALL
SELECT
    *
FROM
    view_adrs_master
WHERE
    NOT EXISTS (
        SELECT
            1
        FROM
            tbl_adrs_user
        WHERE
            ( view_adrs_master.hospnum = tbl_adrs_user.hospnum ) AND
            ( view_adrs_master.lpubcd = tbl_adrs_user.lpubcd ) AND
            ( view_adrs_master.post = tbl_adrs_user.post ) AND
            ( view_adrs_master.rennum = tbl_adrs_user.rennum )
    )
-- ORDER BY
);
